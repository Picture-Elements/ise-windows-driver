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
 * next_out_idx and first_out_idx members. The driver sends buffers to
 * the ISE board by moving the next_out_idx pointer, and the ISE board
 * frees (consumes) buffers by moving the first_out_idx pointer in the
 * channel table. The synchronization process is simplified by the
 * knowledge that each pointer is manipulated by only one side.
 *
 * Very nearly the only moment of synchronization is when the write
 * ring is full. In that case, attempts by the driver to go to the
 * next buffer should block. That feat is managed by the
 * wait_for_write_ring method.
 */

typedef int (*ring_test_t)(struct channel_t*xpd);

static int write_ring_space_available(struct channel_t*xpd)
{
      return NEXT_OUT_IDX(xpd->table->next_out_idx)
	    != xpd->table->first_out_idx;
}

static int write_ring_space_empty(struct channel_t*xpd)
{
      return xpd->table->first_out_idx == xpd->table->next_out_idx;
}

static void write_ring_cancel(DEVICE_OBJECT*dev, IRP*irp)
{
      struct channel_t*xpd = get_channel(irp);
      struct instance_t*xsp = xpd->xsp;

      KeInsertQueueDpc(&xsp->pending_write_dpc, 0, 0);
      IoReleaseCancelSpinLock(irp->CancelIrql);
}

static void simple_flush_cancel(struct instance_t*xsp, IRP*irp)
{
      irp->IoStatus.Status = STATUS_CANCELLED;
      irp->IoStatus.Information = 0;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
}

/*
 * The wait_for_write_ring method is called by the flush code to
 * schedule the IRP for some time in the future when the write ring is
 * not full. The wait_for_write_ring marks the IRP pending and puts it
 * into the pending_write_irps list, where the IRP sits until the
 * pending_write_dpc is called.
 */
static void wait_for_write_ring(struct instance_t*xsp,
				struct channel_t*xpd,
				IRP*irp, callback_t callback,
				vcallback_t cancel,
				ring_test_t tester)
{
      unsigned long mask = dev_mask_irqs(xsp);

      if (irp->Tail.Overlay.DriverContext[0] != 0) {
	    printk("ise%u.%u: warning: DriverContext[0] overrun"
		   " in wait_for_write_ring\n", xsp->id, xpd->channel);
      }

      if (irp->Tail.Overlay.DriverContext[2] != 0) {
	    printk("ise%u.%u: warning: DriverContext[2] overrun"
		   " in wait_for_write_ring\n", xsp->id, xpd->channel);
      }

      if (irp->Tail.Overlay.DriverContext[3] != 0) {
	    printk("ise%u.%u: warning: DriverContext[3] overrun"
		   " in wait_for_write_ring\n", xsp->id, xpd->channel);
      }

      irp->Tail.Overlay.DriverContext[0] = callback;
      irp->Tail.Overlay.DriverContext[2] = cancel;
      irp->Tail.Overlay.DriverContext[3] = tester;
      ExInterlockedInsertTailList(&xsp->pending_write_irps,
				  &irp->Tail.Overlay.ListEntry,
				  &xsp->pending_write_sync);
      IoMarkIrpPending(irp);
      irp->IoStatus.Status = STATUS_PENDING;
      KeInsertQueueDpc(&xsp->pending_write_dpc, 0, 0);

      IoSetCancelRoutine(irp, write_ring_cancel);

      dev_unmask_irqs(xsp, mask);
}

/*
 * The pending_write_dpc function is called when the interrupt handler
 * detects a change in the table state, or as a result of the initial
 * queue by the wait_for_write_ring. The dpc pulls IRPS of the pending
 * list and checks to see if their channel has space. If it does, the
 * IRP is run with its associated callback. Otherwise, it is replaced
 * on the queue. The ordering within the queue is preserved, so it is
 * impossible for an IRP to starve another.
 */
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
	    ring_test_t test_fun;

	    qe  = RemoveHeadList(&tmp);
	    irp = CONTAINING_RECORD(qe, IRP, Tail.Overlay.ListEntry);
	    xpd = get_channel(irp);

	    test_fun = (ring_test_t) irp->Tail.Overlay.DriverContext[3];
	    if ((*test_fun)(xpd)) {
		    /* If the IRP is ready to continue, then call the
		       main callback. */
		  callback_t call_fun = (callback_t)
			irp->Tail.Overlay.DriverContext[0];
		  irp->Tail.Overlay.DriverContext[0] = 0;
		  irp->Tail.Overlay.DriverContext[2] = 0;
		  irp->Tail.Overlay.DriverContext[3] = 0;

		  (*call_fun) (xsp, irp);

	    } else if (irp->Cancel) {
		    /* If the IRP was cancelled, then call the cancel
		       callback instead. */
		  callback_t call_fun = (callback_t)
			irp->Tail.Overlay.DriverContext[2];
		  irp->Tail.Overlay.DriverContext[0] = 0;
		  irp->Tail.Overlay.DriverContext[2] = 0;
		  irp->Tail.Overlay.DriverContext[3] = 0;
		  (*call_fun)(xsp, irp);

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
static NTSTATUS flush_channel_2(struct instance_t*xsp, IRP*irp);

NTSTATUS flush_channel(struct instance_t*xsp, IRP*irp,
		       callback_t callback, vcallback_t cancel)
{
      int rc;
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct channel_t*xpd = get_channel(irp);

      if (xpd->out_off == 0) {
	    return (*callback)(xsp, irp);
      }

      if (irp->Tail.Overlay.DriverContext[1] != 0) {
	    printk("ise%u.%u: warning: DriverContext[1] overrun"
		   " in flush_channel\n", xsp->id, xpd->channel);
      }

      irp->Tail.Overlay.DriverContext[1] = callback;
      wait_for_write_ring(xsp, xpd, irp, flush_channel_2, cancel,
			  write_ring_space_available);
      return STATUS_PENDING;
}

/*
 * The pending_write_dpc calls this function (at the request of the
 * flush_channel function) when there is at least one buffer of space
 * available in the write ring. Shift the current buffer into that
 * space and start a new empty buffer.
 */
static NTSTATUS flush_channel_2(struct instance_t*xsp, IRP*irp)
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

      return (*callback)(xsp, irp);
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
static NTSTATUS sync_channel_2(struct instance_t*xsp, IRP*irp);

static NTSTATUS sync_channel(struct instance_t*xsp, IRP*irp,
			     callback_t callback)
{
      struct channel_t*xpd = get_channel(irp);

      if (xpd->table->first_out_idx == xpd->table->next_out_idx) {
	    return (*callback)(xsp, irp);
      }

      if (irp->Tail.Overlay.DriverContext[1] != 0) {
	    printk("ise%u.%u: warning: DriverContext[1] overrun"
		   " in sync_channel\n", xsp->id, xpd->channel);
      }

      irp->Tail.Overlay.DriverContext[1] = callback;
      wait_for_write_ring(xsp, xpd, irp, sync_channel_2,
			  simple_flush_cancel,
			  write_ring_space_empty);
      return STATUS_PENDING;
}

/*
 * This function is called after a change in write ring status. If the
 * ring is now really empty, then call the callback. Otherwise, retry
 * the wait.
 */
static NTSTATUS sync_channel_2(struct instance_t*xsp, IRP*irp)
{
      struct channel_t*xpd = get_channel(irp);

      callback_t callback = (callback_t)irp->Tail.Overlay.DriverContext[1];
      irp->Tail.Overlay.DriverContext[1] = 0;

      return (*callback)(xsp, irp);
}

/*
 * The dev_ioctl_flush function handles the DeviceIoControl directly
 * from the user. In this case is simply makes up the parameters and
 * calls the flush_channel. There is nothing special to do if the
 * operation is cancelled.
 */
static NTSTATUS dev_ioctl_flush(DEVICE_OBJECT*dev, IRP*irp)
{
      struct instance_t*xsp = (struct instance_t*)dev->DeviceExtension;
      return flush_channel(xsp, irp, complete_success, simple_flush_cancel);
}

static NTSTATUS dev_ioctl_sync_2(struct instance_t*xsp, IRP*irp);

static NTSTATUS dev_ioctl_sync(DEVICE_OBJECT*dev, IRP*irp)
{
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct instance_t*xsp = (struct instance_t*)dev->DeviceExtension;
      struct channel_t*xpd = get_channel(irp);

      return flush_channel(xsp, irp, dev_ioctl_sync_2, simple_flush_cancel);
}

static NTSTATUS dev_ioctl_sync_2(struct instance_t*xsp, IRP*irp)
{
      return sync_channel(xsp, irp, complete_success);
}

/*
 * Change the channel number for this channel. This sends the channel
 * through several states on the way, so there are lots of callbacks
 * that lead to me.
 */
static NTSTATUS dev_ioctl_channel_2(struct instance_t*xsp, IRP*irp);

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

      if (debug_flag & UCR_TRACE_CHAN)
	    printk("ise%u: ioctl switch from channel %u to %u\n",
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

      return flush_channel(xsp, irp, dev_ioctl_channel_2,
			   simple_flush_cancel);
}

static NTSTATUS dev_ioctl_channel_3(struct instance_t*xsp, IRP*irp);

static NTSTATUS dev_ioctl_channel_2(struct instance_t*xsp, IRP*irp)
{
      if (debug_flag & UCR_TRACE_CHAN)
	    printk("ise%u: ioctl switch flush complete, "
		   "starting sync...\n", xsp->id);

      return sync_channel(xsp, irp, dev_ioctl_channel_3);
}

static NTSTATUS dev_ioctl_channel_3(struct instance_t*xsp, IRP*irp)
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
	    return STATUS_NO_MEMORY;
      }

      if (debug_flag & UCR_TRACE_CHAN)
	    printk("ise%u: Commencing switch from channel %u to %u\n",
		   xsp->id, xpd->channel, arg);

      newroot->chan[arg].ptr   = newroot->chan[xpd->channel].ptr;
      newroot->chan[arg].magic = newroot->chan[xpd->channel].magic;

      newroot->chan[xpd->channel].ptr = 0;
      newroot->chan[xpd->channel].magic = 0;

      xpd->channel = (unsigned short)arg;

      root_to_board(xsp, irp, newroot, newrootl, complete_success);
      return STATUS_PENDING;
}

static NTSTATUS dev_ioctl_make_frame(DEVICE_OBJECT*dev, IRP*irp)
{
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct instance_t*xsp = (struct instance_t*)dev->DeviceExtension;
      struct channel_t*xpd = get_channel(irp);

      unsigned long arg, fidx, fsiz;

      if (stp->Parameters.DeviceIoControl.InputBufferLength != sizeof arg) {
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

      arg = *(unsigned long*)irp->AssociatedIrp.SystemBuffer;
      fidx = (arg >> 28) & 0x0f;
      fsiz = arg & 0x0fffffffUL;

      fsiz = ise_make_frame(xsp, fidx, fsiz);

      if (stp->Parameters.DeviceIoControl.OutputBufferLength >= sizeof arg) {
	    arg = fsiz;
	    *(unsigned long*)irp->AssociatedIrp.SystemBuffer = arg;
	    irp->IoStatus.Information = sizeof arg;

      } else {
	    irp->IoStatus.Information = 0;
      }

      irp->IoStatus.Status = STATUS_SUCCESS;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return STATUS_SUCCESS;
}

static NTSTATUS dev_ioctl_free_frame(DEVICE_OBJECT*dev, IRP*irp)
{
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct instance_t*xsp = (struct instance_t*)dev->DeviceExtension;
      struct channel_t*xpd = get_channel(irp);

      unsigned long arg, fidx;

      if (stp->Parameters.DeviceIoControl.InputBufferLength != sizeof arg) {
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

      arg = *(unsigned long*)irp->AssociatedIrp.SystemBuffer;
      fidx = (arg >> 28) & 0x0f;

      if (xsp->frame_map[fidx].base) {
	    printk("ise%u.%u: Attempt to release mapped frame %u.\n",
		   xsp->id, xpd->channel, fidx);
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }


      ise_free_frame(xsp, fidx);

      irp->IoStatus.Status = STATUS_SUCCESS;
      irp->IoStatus.Information = 0;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return STATUS_SUCCESS;
}

static NTSTATUS dev_ioctl_mmap(DEVICE_OBJECT*dev, IRP*irp)
{
      NTSTATUS status;
      unsigned long fidx;
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct instance_t*xsp = (struct instance_t*)dev->DeviceExtension;
      struct channel_t*xpd = get_channel(irp);

      struct UcrMmapInfo*mapinfo;

      if (stp->Parameters.DeviceIoControl.InputBufferLength != sizeof*mapinfo){
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

      mapinfo = (struct UcrMmapInfo*)irp->AssociatedIrp.SystemBuffer;

	/* Check that the frame id is valid. */
      if (mapinfo->frame_id >= 16) {
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

      fidx = mapinfo->frame_id;

	/* Check that the frame exists. */
      if (xsp->frame_mdl[fidx] == 0) {
	    irp->IoStatus.Status = STATUS_NO_MEMORY;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_NO_MEMORY;
      }

	/* Check that the frame is not already mapped. */
      if (xsp->frame_map[fidx].base != 0) {
	    irp->IoStatus.Status = STATUS_NO_MEMORY;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

	/* Map the pages of the frame into user mode. Ignore the size
	   that the user passes, and return the size chosen, along
	   with the base address, to the caller. */
      mapinfo->base = MmMapLockedPagesSpecifyCache(xsp->frame_mdl[fidx],
			  UserMode, MmNonCached, 0, FALSE, NormalPagePriority);
      mapinfo->size = xsp->frame_tab[fidx]->page_count * PAGE_SIZE;

	/* Save the mapping. */
      xsp->frame_map[fidx].proc = IoGetCurrentProcess();
      xsp->frame_map[fidx].base = mapinfo->base;

	/* All done, it seems. */
      irp->IoStatus.Status = STATUS_SUCCESS;
      irp->IoStatus.Information = sizeof(*mapinfo);
      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return STATUS_SUCCESS;
}

static NTSTATUS dev_ioctl_munmap(DEVICE_OBJECT*dev, IRP*irp)
{
      NTSTATUS status;
      unsigned long fidx;
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct instance_t*xsp = (struct instance_t*)dev->DeviceExtension;
      struct channel_t*xpd = get_channel(irp);

      struct UcrMmapInfo*mapinfo;


      if (stp->Parameters.DeviceIoControl.InputBufferLength != sizeof*mapinfo){
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

      mapinfo = (struct UcrMmapInfo*)irp->AssociatedIrp.SystemBuffer;

	/* Check that the frame id is valid. */
      if (mapinfo->frame_id >= 16) {
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

      fidx = mapinfo->frame_id;

	/* Check that the frame exists. */
      if (xsp->frame_mdl[fidx] == 0) {
	    irp->IoStatus.Status = STATUS_NO_MEMORY;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_NO_MEMORY;
      }

	/* Check that there is a mapping to unmap. */
      if (xsp->frame_map[fidx].base == 0){
	    printk("ise%u: unmap frame %u, that is not mapped\n",
		   xsp->id, fidx);

	    irp->IoStatus.Status = STATUS_NO_MEMORY;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_NO_MEMORY;
      }

	/* ... and that it belongs to me */
      if (xsp->frame_map[fidx].proc != IoGetCurrentProcess()){
	    printk("ise%u: unmap frame %u, that doesn't belong to me\n",
		   xsp->id, fidx);

	    irp->IoStatus.Status = STATUS_NO_MEMORY;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_NO_MEMORY;
      }

	/* Unmap the mapping, and clear it from the device driver. */
      MmUnmapLockedPages(xsp->frame_map[fidx].base, xsp->frame_mdl[fidx]);

      xsp->frame_map[fidx].proc = 0;
      xsp->frame_map[fidx].base = 0;

      irp->IoStatus.Status = STATUS_SUCCESS;
      irp->IoStatus.Information = 0;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return STATUS_SUCCESS;
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

	  case UCR_MAKE_FRAME:
	    status = dev_ioctl_make_frame(dev, irp);
	    break;

	  case UCR_FREE_FRAME:
	    status = dev_ioctl_free_frame(dev, irp);
	    break;

	  case UCR_MMAP_FRAME:
	    status = dev_ioctl_mmap(dev, irp);
	    break;

	  case UCR_MUNMAP_FRAME:
	    status = dev_ioctl_munmap(dev, irp);
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
 * Revision 1.6  2001/09/06 22:53:56  steve
 *  Flush can be cancelled.
 *
 * Revision 1.5  2001/09/06 18:28:43  steve
 *  Read timeouts.
 *
 * Revision 1.4  2001/09/05 22:05:55  steve
 *  protect mappings from misused or forgotten unmaps.
 *
 * Revision 1.3  2001/09/04 02:47:09  steve
 *  Add frame allocate/free/map/unmap controls.
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

