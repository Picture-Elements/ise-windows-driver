/*
 * Copyright (c) 2001 Picture Elements, Inc.
 *    Stephen Williams (steve@picturel.com)
 *
 * $Id$
 */

# include  "ise_sys.h"

static NTSTATUS dev_read_2(struct instance_t*xsp, IRP*irp);
static NTSTATUS dev_read_3(struct instance_t*xsp, IRP*irp);
static void dev_read_flush_cancel(struct instance_t*xsp, IRP*irp);

/*
 * This is the entry into the chain of events that leads to a read. I
 * set up the initial state of the read for the IRP, then start the
 * flush_channel. The flhs_channel will call the text step of the read
 * when it is done.
 */
NTSTATUS dev_read(DEVICE_OBJECT*dev, IRP*irp)
{
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct instance_t*xsp = (struct instance_t*)dev->DeviceExtension;
      struct channel_t*xpd = get_channel(irp);

	/* Make sure there are not currently pending reads already. */
      if (xpd->read_pending) {
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

      xsp->pending_read_count.scheduled += 1;
      xpd->read_pending = irp;
      xpd->read_pstate = 100; /* state for debugging */

	/* Steal the ByteOffset member as a progress pointer for the
	   actual read operation. This allows me to span function
	   calls using only the IRP for context. */
      stp->Parameters.Read.ByteOffset.LowPart = 0;

      if (debug_flag & UCR_TRACE_CHAN)
	    printk("ise%u.%u: read %u bytes\n",
		   xsp->id, xpd->channel, stp->Parameters.Read.Length);

	/* Jump right into the phase 2 read function. This may detect
	   that it is blecked and reschedule itself, but that is for
	   itself to decide. */
      return dev_read_2(xsp, irp);
}

static void read_cancel(DEVICE_OBJECT*dev, IRP*irp);

/*
 * Here the channel is flushed. Now check to see of there is any data
 * in the channel to be read. If there is *not*, then queue the irp
 * and set a cancel routine.
 *
 * Note that the test for an empty channel needs to be done with
 * interrupts masked, as an interrupt will cause me to retest.
 */
static NTSTATUS dev_read_2(struct instance_t*xsp, IRP*irp)
{
      NTSTATUS status;
      KIRQL save_irql;
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct channel_t*xpd = get_channel(irp);

      unsigned long mask;

      KeAcquireSpinLock(&xsp->mutex, &save_irql);
      mask = dev_mask_irqs(xsp);

      if (xpd->read_pending != irp)
	    printk("ise%u.%u: IRP mismatch is read_2?\n", xsp->id,
		   xpd->channel);

      xpd->read_pstate = 200; /* read state for debugging. */

      if (debug_flag & UCR_TRACE_CHAN)
	    printk("ise%u.%u: read flush done, checking for data.\n",
		   xsp->id, xpd->channel);

	/* If I can't read *any* data, then wait until I can. */
      if (CHANNEL_IN_EMPTY(xpd)) {

	    xpd->read_pstate = 201; /* read state for debugging. */

	      /* If the read_timeout is 0, then this is a poll. Return
		 a byte count of 0 and complete the request. */
	    if (xpd->read_timeout == 0) {
		  dev_unmask_irqs(xsp, mask);
		  KeReleaseSpinLock(&xsp->mutex, save_irql);
		  xpd->read_pending = 0;
		  xpd->read_pstate = 0;
		  irp->IoStatus.Status = STATUS_SUCCESS;
		  irp->IoStatus.Information = 0;
		  IoCompleteRequest(irp, IO_NO_INCREMENT);
		  return STATUS_SUCCESS;
	    }

	    xpd->read_pstate = 202; /* read state for debugging. */

	    irp->Tail.Overlay.DriverContext[0] = &dev_read_3;
	    ExInterlockedInsertTailList(&xsp->pending_read_irps,
					&irp->Tail.Overlay.ListEntry,
					&xsp->pending_read_sync);
	    IoMarkIrpPending(irp);
	    irp->IoStatus.Status = STATUS_PENDING;

	    xpd->read_pstate = 203; /* read state for debugging. */

	      /* If there is a timeout, then set the timeout value and
		 start the timer. */
	    if (xpd->read_timeout > 0) {
		  LARGE_INTEGER due_time;
		  xpd->read_pstate = 204; /* read state for debugging. */

		  due_time = RtlEnlargedIntegerMultiply(
				    xpd->read_timeout, 10000);
		  KeSetTimer(&xpd->read_timer,
			     RtlLargeIntegerNegate(due_time),
			     &xpd->read_timer_dpc);
	    }

	    IoSetCancelRoutine(irp, &read_cancel);
	    dev_unmask_irqs(xsp, mask);
	    KeReleaseSpinLock(&xsp->mutex, save_irql);
	    return STATUS_PENDING;
      }

      xpd->read_pstate = 205; /* read state for debugging. */

      status = dev_read_3(xsp, irp);
      dev_unmask_irqs(xsp, mask);
      KeReleaseSpinLock(&xsp->mutex, save_irql);

      return status;
}

/*
 * At this point we *know* that there is at least some data to read,
 * so blocking is done. This was either assured by the dev_read_2
 * function, or failing that the pending_read_dpc function. That means
 * that this function may haven been called from a thread context or
 * from the DPC.
 *
 * No matter. However we got here, we are going to complete the IRP
 * with a success, and some amount of data.
 *
 * irqs for the board should be blocked by the caller, so we do not
 * block irqs or grab the mutex.
 */
static NTSTATUS dev_read_3(struct instance_t*xsp, IRP*irp)
{
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct channel_t*xpd = get_channel(irp);
      unsigned char*bytes;
      unsigned long count, tcount;

      if (debug_flag & UCR_TRACE_CHAN)
	    printk("ise%u.%u: read check done, performing read.\n",
		   xsp->id, xpd->channel);

	/* Clear the read_cancel pointer. */
      IoSetCancelRoutine(irp, 0);

      xpd->read_pstate = 300; /* read state for debugging */

      bytes  = irp->AssociatedIrp.SystemBuffer;
      count  = stp->Parameters.Read.Length;
      tcount = count;

      bytes  += stp->Parameters.Read.ByteOffset.LowPart;
      tcount -= stp->Parameters.Read.ByteOffset.LowPart;

	/* By now we believe that there is at least some data to be
	   read. That means no more blocking. Read what we can into
	   the user's buffer and return the transfer count. */

      while (tcount > 0 && ! CHANNEL_IN_EMPTY(xpd)) {
	    unsigned trans = tcount;

	    unsigned char*buf = xpd->in[xpd->table->first_in_idx].ptr;
	    unsigned long siz = xpd->table->in[xpd->table->first_in_idx].count;

	    if ((trans + xpd->in_off) > siz)
		  trans = siz - xpd->in_off;

	    if (debug_flag & UCR_TRACE_CHAN)
		  printk("ise%u.%u (d): read %u of %u bytes "
			 "[in_off=%u]\n", xsp->id, xpd->channel,
			 trans, siz, xpd->in_off);

	    RtlCopyMemory(bytes, buf + xpd->in_off, trans);

	    tcount -= trans;
	    bytes  += trans;
	    xpd->in_off += trans;

	    stp->Parameters.Read.ByteOffset.LowPart += trans;

	      /* If I get to the end of a buffer, release the current
		 one and notify the ISE board that the channel has
		 changed. This may get me more read buffers. */
	    if (xpd->in_off == siz) {
		  xpd->in_off = 0;
		  xpd->table->in[xpd->table->first_in_idx].count = PAGE_SIZE;
		  INCR_IN_IDX(xpd->table->first_in_idx);
		  dev_set_bells(xsp, CHANGE_BELLMASK);
	    }
      }

      if (debug_flag & UCR_TRACE_CHAN)
	    printk("ise%u.%u (d): read complete (%u of %u bytes)\n",
		   xsp->id, xpd->channel,
		   stp->Parameters.Read.ByteOffset.LowPart, count);

      xsp->pending_read_count.complete += 1;

      xpd->read_pending = 0;
      xpd->read_pstate = 0;
      irp->IoStatus.Status = STATUS_SUCCESS;
      irp->IoStatus.Information = stp->Parameters.Read.ByteOffset.LowPart;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return STATUS_SUCCESS;
}

/*
 * This function is invoked as a DPC when the read status of the board
 * has possibly changed. I check all the pending IRPs, and schedule
 * the ones that can run.
 */
void pending_read_dpc(KDPC*dpc, void*ctx, void*arg1, void*arg2)
{
      unsigned long mask;

      LIST_ENTRY tmp;
      struct instance_t*xsp = (struct instance_t*)ctx;


      KeAcquireSpinLockAtDpcLevel(&xsp->mutex);
      mask = dev_mask_irqs(xsp);


	/* Move the irps from the pending_write_irps list to a tmp
	   list. Acquire the lock once and run through them with
	   not-interlocked methods, safe yet efficient. */

      InitializeListHead(&tmp);
      KeAcquireSpinLockAtDpcLevel(&xsp->pending_read_sync);

      while (! IsListEmpty(&xsp->pending_read_irps)) {
	    LIST_ENTRY*qe;

	    qe = RemoveHeadList(&xsp->pending_read_irps);
	    InsertTailList(&tmp, qe);
      }


	/* Try the listed items and schedule all the ones that have
	   been released by available space in the write ring for the
	   channel.

	   For each IRP, if it is still blocked on a read, then
	   re-queue it. Otherwise, call the callback function to
	   resume it.

	   Note that interrupts from the boar are blocked for the
	   whole duration of this loop. This is OK as the interrupt
	   can't do anything other then schedule the DPC, and that is
	   what I am. So the cost is nothing, and I may even save a
	   few useless calls to the interrupt handler.

	   Watch out that this IRP may have been cancelled. If that is
	   the case, do *not* requeue it, just ignore it. */

      while (! IsListEmpty(&tmp)) {
	    LIST_ENTRY*qe;
	    IRP*irp;
	    struct channel_t*xpd;
	    callback_t call_fun;

	    qe  = RemoveHeadList(&tmp);
	    irp = CONTAINING_RECORD(qe, IRP, Tail.Overlay.ListEntry);
	    xpd = get_channel(irp);

	    if (CHANNEL_IN_EMPTY(xpd)) {
		  if (! irp->Cancel)
			InsertTailList(&xsp->pending_read_irps, qe);

		  continue;
	    }

	    KeCancelTimer(&xpd->read_timer);

	    IoSetCancelRoutine(irp, 0);
	    if (irp->Cancel)
		  continue;

	      /* In reality, this only calls read_3. That function
		 does not call dev_mask_irqs, so we can leave the irqs
		 mask in our iteration. */
	    call_fun = (callback_t) irp->Tail.Overlay.DriverContext[0];
	    irp->Tail.Overlay.DriverContext[0] = 0;
	    (*call_fun) (xsp, irp);
      }


      dev_unmask_irqs(xsp, mask);
      KeReleaseSpinLockFromDpcLevel(&xsp->mutex);

      KeReleaseSpinLockFromDpcLevel(&xsp->pending_read_sync);
}

static void read_cancel(DEVICE_OBJECT*dev, IRP*irp)
{
      KIRQL irql;
      struct instance_t*xsp = (struct instance_t*)dev->DeviceExtension;
      struct channel_t*xpd = get_channel(irp);

      KeAcquireSpinLock(&xsp->pending_read_sync, &irql);

      RemoveEntryList(&irp->Tail.Overlay.ListEntry);
      xsp->pending_read_count.cancelled += 1;

      KeCancelTimer(&xpd->read_timer);

      KeReleaseSpinLock(&xsp->pending_read_sync, irql);

      IoReleaseCancelSpinLock(irp->CancelIrql);

      xpd->read_pending = 0;
      irp->IoStatus.Status = STATUS_CANCELLED;
      irp->IoStatus.Information = 0;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
}

void read_timeout(KDPC*dpc, void*ctx, void*arg1, void*arg2)
{
      KIRQL irql, save_irql;
      struct channel_t*xpd = (struct channel_t*)ctx;
      struct instance_t*xsp = xpd->xsp;
      IRP*irp;

	/* This protects the main-line reads from me. */
      KeAcquireSpinLock(&xsp->mutex, &save_irql);

      irp = xpd->read_pending;
      if (irp == 0) {
	    KeReleaseSpinLock(&xsp->mutex, save_irql);
	    return;
      }

	/* mark this as no longer pending. */
      xpd->read_pending = 0;

	/* pull the IRP out of the read list, much like cancel. */
      KeAcquireSpinLock(&xsp->pending_read_sync, &irql);
      RemoveEntryList(&irp->Tail.Overlay.ListEntry);
      IoSetCancelRoutine(irp, 0);
      KeReleaseSpinLock(&xsp->pending_read_sync, irql);

      KeReleaseSpinLock(&xsp->mutex, save_irql);

      if (irp->Cancel)
	    return;

      irp->IoStatus.Status = STATUS_SUCCESS;
      irp->IoStatus.Information = 0;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
}


/*
 * $Log$
 * Revision 1.9  2003/01/09 22:38:06  steve
 *  No need to do flush in a READ.
 *
 * Revision 1.8  2001/10/01 22:48:20  steve
 *  Cancel timers in cancel routines.
 *
 * Revision 1.7  2001/09/28 20:34:08  steve
 *  Fix dangling cancel routines.
 *
 * Revision 1.6  2001/09/28 18:09:53  steve
 *  Create a per-device mutex to manage multi-processor access
 *  to the instance object.
 *
 *  Fix some problems with timeout handling.
 *
 *  Add some diagnostic features for tracking down locking
 *  or delay problems.
 *
 * Revision 1.5  2001/09/06 22:53:56  steve
 *  Flush can be cancelled.
 *
 * Revision 1.4  2001/09/06 18:28:43  steve
 *  Read timeouts.
 *
 * Revision 1.3  2001/09/05 01:19:58  steve
 *  make read robust to multiple blocked read attempts.
 *
 * Revision 1.2  2001/07/30 21:32:42  steve
 *  Rearrange the status path to follow the return codes of
 *  the callbacks, and preliminary implementation of the
 *  RUN_PROGRAM ioctl.
 *
 * Revision 1.1  2001/07/26 00:31:30  steve
 *  Windows 2000 driver.
 *
 */

