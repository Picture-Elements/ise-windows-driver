/*
 * Copyright (c) 2001 Picture Elements, Inc.
 *    Stephen Williams (steve@picturel.com)
 *
 * $Id$
 */

# include  "ise_sys.h"

static NTSTATUS dev_read_2(struct instance_t*xsp, IRP*irp);

NTSTATUS dev_read(DEVICE_OBJECT*dev, IRP*irp)
{
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct instance_t*xsp = (struct instance_t*)dev->DeviceExtension;
      struct channel_t*xpd = get_channel(irp);

      xsp->pending_read_count.scheduled += 1;

	/* Steal the ByteOffset member as a progress pointer for the
	   actual read operation. This allows me to span function
	   calls using only the IRP for context. */
      stp->Parameters.Read.ByteOffset.LowPart = 0;

      if (debug_flag & UCR_TRACE_CHAN)
	    printk("ise%u.%u: read %u bytes\n",
		   xsp->id, xpd->channel, stp->Parameters.Read.Length);

	/* Start the read with a flush of the write side. This gets
	   the board thinking about the last write command that I
	   might have sent. */
      return flush_channel(xsp, irp, &dev_read_2);
}

static void read_2_cancel(DEVICE_OBJECT*dev, IRP*irp);

static NTSTATUS dev_read_2(struct instance_t*xsp, IRP*irp)
{
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct channel_t*xpd = get_channel(irp);
      unsigned char*bytes;
      unsigned long count, tcount;

      bytes  = irp->AssociatedIrp.SystemBuffer;
      count  = stp->Parameters.Read.Length;
      tcount = count;

      bytes  += stp->Parameters.Read.ByteOffset.LowPart;
      tcount -= stp->Parameters.Read.ByteOffset.LowPart;

	/* If I can't read *any* data, then wait until I can. */
      if (CHANNEL_IN_EMPTY(xpd)) {
	    irp->Tail.Overlay.DriverContext[0] = &dev_read_2;
	    ExInterlockedInsertTailList(&xsp->pending_read_irps,
					&irp->Tail.Overlay.ListEntry,
					&xsp->pending_read_sync);
	    IoMarkIrpPending(irp);
	    irp->IoStatus.Status = STATUS_PENDING;
	    IoSetCancelRoutine(irp, &read_2_cancel);
	    return STATUS_PENDING;
      }

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

      irp->IoStatus.Status = STATUS_SUCCESS;
      irp->IoStatus.Information = stp->Parameters.Read.ByteOffset.LowPart;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return STATUS_SUCCESS;
}

void pending_read_dpc(KDPC*dpc, void*ctx, void*arg1, void*arg2)
{
      unsigned long mask;

      LIST_ENTRY tmp;
      struct instance_t*xsp = (struct instance_t*)ctx;

	/* Move the irps from the pending_write_irps list to a tmp
	   list. Acquire the lock once and run through them with
	   not-interlocked methods, safe yet efficient.

	   As we do this, remove the cancel routines as well. This
	   protects us from cancels that can be delivered to these
	   IRPs. Watch out for the race where a cancel routine might
	   be started while I have the read_sync. In this case, the
	   Cancel flag will be set. */

      InitializeListHead(&tmp);
      KeAcquireSpinLockAtDpcLevel(&xsp->pending_read_sync);
      while (! IsListEmpty(&xsp->pending_read_irps)) {
	    LIST_ENTRY*qe;
	    IRP*irp;

	    qe = RemoveHeadList(&xsp->pending_read_irps);
	    irp = CONTAINING_RECORD(qe, IRP, Tail.Overlay.ListEntry);
	    IoSetCancelRoutine(irp, 0);
	    if (! irp->Cancel)
		  InsertTailList(&tmp, qe);
      }
      KeReleaseSpinLockFromDpcLevel(&xsp->pending_read_sync);


	/* Try the listed items and schedule all the ones that have
	   been released by available space in the write ring for the
	   channel. */
      while (! IsListEmpty(&tmp)) {
	    LIST_ENTRY*qe;
	    IRP*irp;
	    callback_t call_fun;

	    qe  = RemoveHeadList(&tmp);
	    irp = CONTAINING_RECORD(qe, IRP, Tail.Overlay.ListEntry);

	    call_fun = (callback_t) irp->Tail.Overlay.DriverContext[0];
	    irp->Tail.Overlay.DriverContext[0] = 0;

	    (*call_fun) (xsp, irp);
      }
}

static void read_2_cancel(DEVICE_OBJECT*dev, IRP*irp)
{
      KIRQL irql;
      struct instance_t*xsp = (struct instance_t*)dev->DeviceExtension;

      KeAcquireSpinLock(&xsp->pending_read_sync, &irql);
      RemoveEntryList(&irp->Tail.Overlay.ListEntry);
      xsp->pending_read_count.cancelled += 1;
      KeReleaseSpinLock(&xsp->pending_read_sync, irql);

      IoReleaseCancelSpinLock(irp->CancelIrql);

      irp->IoStatus.Status = STATUS_CANCELLED;
      irp->IoStatus.Information = 0;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
}

/*
 * $Log$
 * Revision 1.2  2001/07/30 21:32:42  steve
 *  Rearrange the status path to follow the return codes of
 *  the callbacks, and preliminary implementation of the
 *  RUN_PROGRAM ioctl.
 *
 * Revision 1.1  2001/07/26 00:31:30  steve
 *  Windows 2000 driver.
 *
 */

