/*
 * Copyright (c) 2001 Picture Elements, Inc.
 *    Stephen Williams (steve@picturel.com)
 *
 * $Id$
 */

# include  "ise_sys.h"

/*
 * Output to the ISE board is managed through a ring of CHANNEL_OBUFS
 * buffers in the channel_table. This is a buffer ring managed by the
 * next_out_idx and first_out_idx members.
 */

static int write_ring_space_available(struct channel_t*xpd)
{
      return NEXT_OUT_IDX(xpd->table->next_out_idx)
	    != xpd->table->first_out_idx;
}

static void wait_for_write_ring(struct instance_t*xsp,
				struct channel_t*xpd,
				IRP*irp, callback_t callback)
{
      unsigned long mask = dev_mask_irqs(xsp);

      if (irp->Tail.Overlay.DriverContext[0] != 0) {
	    printk("ise%u.%u: warning: DriverContext[0] overrun"
		   " in wait_for_write_ring\n", xsp->id, xpd->channel);
      }

      irp->Tail.Overlay.DriverContext[0] = callback;
      ExInterlockedInsertTailList(&xsp->pending_write_irps,
				  &irp->Tail.Overlay.ListEntry,
				  &xsp->pending_write_sync);
      IoMarkIrpPending(irp);
      irp->IoStatus.Status = STATUS_PENDING;
      KeInsertQueueDpc(&xsp->pending_write_dpc, 0, 0);

      dev_unmask_irqs(xsp, mask);
}

void pending_write_dpc(KDPC*dpc, void*ctx, void*arg1, void*arg2)
{
      unsigned long mask;

      LIST_ENTRY tmp;
      struct instance_t*xsp = (struct instance_t*)ctx;

	/* Move the irps from the pending_write_irps list to a tmp
	   list. Acquire the lock once and run through them with
	   not-interlocked methods, safe yet efficient. */

      InitializeListHead(&tmp);
      KeAcquireSpinLockAtDpcLevel(&xsp->pending_write_sync);
      while (! IsListEmpty(&xsp->pending_write_irps)) {
	    LIST_ENTRY*qe = RemoveHeadList(&xsp->pending_write_irps);
	    InsertTailList(&tmp, qe);
      }
      KeReleaseSpinLockFromDpcLevel(&xsp->pending_write_sync);


	/* Try the listed items and schedule all the ones that have
	   been released by available space in the write ring for the
	   channel. */
      while (! IsListEmpty(&tmp)) {
	    LIST_ENTRY*qe;
	    IRP*irp;
	    struct channel_t*xpd;

	    qe  = RemoveHeadList(&tmp);
	    irp = CONTAINING_RECORD(qe, IRP, Tail.Overlay.ListEntry);
	    xpd = get_channel(irp);

	    if (write_ring_space_available(xpd)) {
		  callback_t call_fun = (callback_t)
			irp->Tail.Overlay.DriverContext[0];
		  irp->Tail.Overlay.DriverContext[0] = 0;

		  (*call_fun) (xsp, irp);

	    } else {

		  ExInterlockedInsertTailList(&xsp->pending_write_irps, qe,
					      &xsp->pending_write_sync);
	    }
      }
}

/*
 * The flush_channel function assures that the output buffer is
 * flushed into the write ring and that the current buffer is
 * completely empty (and ready for new data). If the current buffer
 * already is empty, then there is nothing to do. Otherwise, wait for
 * space in the write ring, then perform the actual flush in the
 * flush_channel_2 function below.
 *
 * The flush_channel calls the callback function when the flush is
 * complete. If I need to delay for the write ring, it uses
 * DriverContext[1] to save the callback. The wait_for_write_ring
 * function is called, which uses DriverContext[0].
 *
 * It is possible that the flush can be done without waiting. In that
 * case, the callback is called right away, and this function returns
 * STATUS_SUCCESS. Otherwise, the irp is marked as pending and this
 * function returns STATUS_PENDING. The flush_channel_2 function will
 * be called some time in the future, and it will call the callback.
 */
static void flush_channel_2(struct instance_t*xsp, IRP*irp);

NTSTATUS flush_channel(struct instance_t*xsp, IRP*irp, callback_t callback)
{
      int rc;
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct channel_t*xpd = get_channel(irp);

      if (xpd->out_off == 0) {
	    (*callback)(xsp, irp);
	    return STATUS_SUCCESS;
      }

      if (irp->Tail.Overlay.DriverContext[1] != 0) {
	    printk("ise%u.%u: warning: DriverContext[1] overrun"
		   " in flush_channel\n", xsp->id, xpd->channel);
      }

      irp->Tail.Overlay.DriverContext[1] = callback;
      wait_for_write_ring(xsp, xpd, irp, flush_channel_2);
      return STATUS_PENDING;
}

/*
 * The pending_write_dpc calls this function (at the request of the
 * flush_channel function) when there is at least one buffer of space
 * available in the write ring. Shift the current buffer into that
 * space and start a new empty buffer.
 */
static void flush_channel_2(struct instance_t*xsp, IRP*irp)
{
      unsigned rc;
      callback_t callback;
      struct channel_t*xpd = get_channel(irp);

	/* Set the count for the buffer that I am working on. */
      xpd->table->out[xpd->table->next_out_idx].count = xpd->out_off;

	/* Initialize the count of the next buffer that I am going to
	   be workin on shortly, and clear my offset pointer. */
      rc = NEXT_OUT_IDX(xpd->table->next_out_idx);
      xpd->table->out[rc].count = PAGE_SIZE;
      xpd->out_off = 0;

	/* Tell the target board that the next_out pointer has
	   moved. This causes the board to notice that the buffer is
	   ready. */
      xpd->table->next_out_idx = rc;
      dev_set_bells(xsp, CHANGE_BELLMASK);

      callback = (callback_t)irp->Tail.Overlay.DriverContext[1];
      irp->Tail.Overlay.DriverContext[1] = 0;

      (*callback)(xsp, irp);
}

/*
 * Sync channel is similar to flush channel, except that it waits for
 * the write ring to be completely empty before invoking the
 * callback. Since it is using the write_ring_dpc (via the
 * wait_for_write_ring function) it needs to be prepared to call back
 * several times.
 *
 * The wait_for_write_ring function uses DriverContext[0] for the
 * callback to sync_channel_2, so I use DriverContext[1] for the
 * callback that is passed to me.
 */
static void sync_channel_2(struct instance_t*xsp, IRP*irp);

static NTSTATUS sync_channel(struct instance_t*xsp, IRP*irp,
			     callback_t callback)
{
      struct channel_t*xpd = get_channel(irp);

      if (xpd->table->first_out_idx == xpd->table->next_out_idx) {
	    (*callback)(xsp, irp);
	    return STATUS_SUCCESS;
      }

      if (irp->Tail.Overlay.DriverContext[1] != 0) {
	    printk("ise%u.%u: warning: DriverContext[1] overrun"
		   " in sync_channel\n", xsp->id, xpd->channel);
      }

      irp->Tail.Overlay.DriverContext[1] = callback;
      wait_for_write_ring(xsp, xpd, irp, sync_channel_2);
      return STATUS_PENDING;
}

/*
 * This function is called after a change in write ring status. If the
 * ring is now really empty, then call the callback. Otherwise, retry
 * the wait.
 */
static void sync_channel_2(struct instance_t*xsp, IRP*irp)
{
      struct channel_t*xpd = get_channel(irp);

      if (xpd->table->first_out_idx == xpd->table->next_out_idx) {
	    callback_t callback =
		  (callback_t)irp->Tail.Overlay.DriverContext[1];
	    irp->Tail.Overlay.DriverContext[1] = 0;

	    (*callback)(xsp, irp);
	    return;
      }

      wait_for_write_ring(xsp, xpd, irp, sync_channel_2);
}

static NTSTATUS dev_ioctl_flush(DEVICE_OBJECT*dev, IRP*irp)
{
      struct instance_t*xsp = (struct instance_t*)dev->DeviceExtension;
      return flush_channel(xsp, irp, complete_success);
}

static void dev_ioctl_sync_2(struct instance_t*xsp, IRP*irp);

static NTSTATUS dev_ioctl_sync(DEVICE_OBJECT*dev, IRP*irp)
{
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct instance_t*xsp = (struct instance_t*)dev->DeviceExtension;
      struct channel_t*xpd = get_channel(irp);

      flush_channel(xsp, irp, dev_ioctl_sync_2);
      return irp->IoStatus.Status;
}

static void dev_ioctl_sync_2(struct instance_t*xsp, IRP*irp)
{
      sync_channel(xsp, irp, complete_success);
}

/*
 * Change the channel number for this channel. This sends the channel
 * through several states on the way, so there are lots of callbacks
 * that lead to me.
 */
static void dev_ioctl_channel_2(struct instance_t*xsp, IRP*irp);

static NTSTATUS dev_ioctl_channel(DEVICE_OBJECT*dev, IRP*irp)
{
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct instance_t*xsp = (struct instance_t*)dev->DeviceExtension;
      struct channel_t*xpd = get_channel(irp);

      unsigned long arg;

      if (stp->Parameters.DeviceIoControl.InputBufferLength != sizeof arg) {
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

      arg = *(unsigned long*)irp->AssociatedIrp.SystemBuffer;

      printk("ise%u: iotcl switch from channel %u to %u\n",
	     xsp->id, xpd->channel, arg);

      if (arg >= ROOT_TABLE_CHANNELS) {
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

      if (xpd->channel == arg) {
	    irp->IoStatus.Status = STATUS_SUCCESS;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_SUCCESS;
      }

	/* If the channel number is used elsewhere, this catches the
	   error. */
      if (channel_by_id(xsp, (unsigned short)arg) != 0) {
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

      irp->Tail.Overlay.DriverContext[0] = 0;
      irp->Tail.Overlay.DriverContext[1] = 0;

      flush_channel(xsp, irp, dev_ioctl_channel_2);
      return irp->IoStatus.Status;
}

static void dev_ioctl_channel_3(struct instance_t*xsp, IRP*irp);

static void dev_ioctl_channel_2(struct instance_t*xsp, IRP*irp)
{
      printk("ise%u: iotcl switch flush complete, starting sync...\n",
	     xsp->id);

      sync_channel(xsp, irp, dev_ioctl_channel_3);
}

static void dev_ioctl_channel_3(struct instance_t*xsp, IRP*irp)
{
      struct root_table*newroot;
      PHYSICAL_ADDRESS newrootl;
      struct channel_t*xpd = get_channel(irp);

      unsigned long arg = *(unsigned long*)irp->AssociatedIrp.SystemBuffer;


	/* Allocate a duplicate root that I can edit, and move the
	   channel table within the root table. */
      newroot = duplicate_root(xsp, &newrootl);
      if (newroot == 0) {
	    irp->IoStatus.Status = STATUS_NO_MEMORY;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return;
      }

      printk("ise%u: Switch from channel %u to %u\n", xsp->id,
	     xpd->channel, arg);

      newroot->chan[arg].ptr   = newroot->chan[xpd->channel].ptr;
      newroot->chan[arg].magic = newroot->chan[xpd->channel].magic;

      newroot->chan[xpd->channel].ptr = 0;
      newroot->chan[xpd->channel].magic = 0;

      xpd->channel = (unsigned short)arg;

      root_to_board(xsp, irp, newroot, newrootl, complete_success);
}

NTSTATUS dev_ioctl(DEVICE_OBJECT*dev, IRP*irp)
{
      NTSTATUS status;
      unsigned long cmd;
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);

      cmd = stp->Parameters.DeviceIoControl.IoControlCode;

      switch (cmd) {

	  case UCR_FLUSH:
	    status = dev_ioctl_flush(dev, irp);
	    break;

	  case UCR_SYNC:
	    status = dev_ioctl_sync(dev, irp);
	    break;

	  case UCR_CHANNEL:
	    status = dev_ioctl_channel(dev, irp);
	    break;

	  default:
	    printk("ise?: invalid IO control code %x\n", cmd);
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    status = STATUS_UNSUCCESSFUL;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    break;
      }

      return status;
}

/*
 * $Log$
 * Revision 1.1  2001/07/26 00:31:30  steve
 *  Windows 2000 driver.
 *
 */

