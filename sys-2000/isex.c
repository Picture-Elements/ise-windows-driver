/*
 * Copyright (c) 2001-2005 Picture Elements, Inc.
 *    Stephen Williams (steve@picturel.com)
 *
 * $Id$
 */

# include  "ise_sys.h"
# include  "isem.h"
# include  "ucrif.h"

/*
 * The ISEX device nodes are an alternative interface into the device
 * board that is used for control access. It is used for such tasks as
 * resetting the board or pressing diagnostic buttons. A device object
 * is created along with each real fdo.
 */

static const wchar_t devname[] = L"\\Device\\isex";
static const wchar_t dosname[] = L"\\DosDevices\\ISEX";

static void frame_cleanup_workitem(DEVICE_OBJECT*dev, void*ctx);

static NTSTATUS ucrx_restart_board(DEVICE_OBJECT*dev, IRP*irp)
{
      KIRQL save_irql;
      struct instance_t*xsp = *((struct instance_t**)dev->DeviceExtension);

	/* Cannot restart the board if there are any channels open. */
      if (xsp->channels != 0 || xsp->frame_done_flags) {
	    IO_ERROR_LOG_PACKET*event;
	    unsigned psize = sizeof(IO_ERROR_LOG_PACKET) + 2*sizeof(ULONG);
	    event = IoAllocateErrorLogEntry(xsp->fdo, (UCHAR)psize);
	    event->ErrorCode = ISE_RESTART_BUSY_BOARD;
	    event->UniqueErrorValue = 0;
	    event->FinalStatus = STATUS_DEVICE_ALREADY_ATTACHED;
	    event->MajorFunctionCode = 0;
	    event->IoControlCode = 0;
	    event->DumpDataSize = 2*sizeof(ULONG);
	    event->DumpData[0] = xsp->channels? 1 : 0;
	    event->DumpData[1] = xsp->frame_done_flags;
	    IoWriteErrorLogEntry(event);

	    irp->IoStatus.Status = STATUS_DEVICE_ALREADY_ATTACHED;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_DEVICE_ALREADY_ATTACHED;
      }

      if (debug_flag&UCR_TRACE_UCRX)
	    printk("ucrx%u: restart ise%u\n", xsp->id, xsp->id);

	/* Clear the table pointers and disable interrupts. */
      KeAcquireSpinLock(&xsp->mutex, &save_irql);
      dev_init_hardware(xsp);
      dev_mask_irqs(xsp);

	/* restart doorbell to the processor. Code on the processor
	   should notice this interrupt and restart itself. */
      dev_set_bells(xsp, 0x40000000);


	/* Make sure the table pointers are still clear, and re-enable
	   the interrupts. */
      dev_init_hardware(xsp);
      KeReleaseSpinLock(&xsp->mutex, save_irql);

      irp->IoStatus.Status = STATUS_SUCCESS;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return STATUS_SUCCESS;
}

static NTSTATUS isex_diagnose(DEVICE_OBJECT*dev, IRP*irp)
{
      unsigned idx;
      struct instance_t*xsp = *((struct instance_t**)dev->DeviceExtension);
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      unsigned long arg;

      if (stp->Parameters.DeviceIoControl.InputBufferLength != sizeof arg) {
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

      arg = *(unsigned long*)irp->AssociatedIrp.SystemBuffer;

      printk("isex%u: Diagnose %u\n", xsp->id, arg);

      switch (arg) {

	  case 0:
	    if (xsp->bar0_size == 0)
		  printk("ise%u: <** Device Not Mapped **>\n", xsp->id);
	    else
		  xsp->dev_ops->diagnose0_dump(xsp);
	    break;

	  case 1:
	    if (xsp->root_irp) {
		  printk("ise%u: root table pending MAGIC=[%x:%x]\n", xsp->id,
			 xsp->root2? xsp->root2->self  : 0x00000000,
			 xsp->root2? xsp->root2->magic : 0x00000000);
	    }

	    printk("ise%u: reads scheduled=%u, completed=%u, "
		   "cancelled=%u\n", xsp->id,
		   xsp->pending_read_count.scheduled,
		   xsp->pending_read_count.complete,
		   xsp->pending_read_count.cancelled);

	    xsp->dev_ops->diagnose1_dump(xsp);

	    printk("ise%u: ROOT TABLE at %p(%x) "
		   "MAGIC=[%x:%x] standby_list_fill=%u\n", xsp->id, xsp->root,
		   xsp->root? xsp->root->self  : 0x00000000,
		   xsp->root? xsp->root->magic : 0x00000000,
		   xsp->root? xsp->root->self  : 0x00000000,
		   xsp->root_standby_leak);

	    if (xsp->channels) {
		  struct channel_t*xpd = xsp->channels;
		  do {
			printk("ise%u.%u: CHANNEL TABLE "
			       "MAGIC=[%x:%x]\n", xsp->id,
			       xpd->channel, xpd->table->magic,
			       xpd->table->self);

			printk("ise%u.%u: OUT (first=%u, next=%u, off=%u) "
			       "IN (first=%u, next=%u, off=%u)\n", xsp->id,
			       xpd->channel, xpd->table->first_out_idx,
			       xpd->table->next_out_idx, xpd->out_off,
			       xpd->table->first_in_idx,
			       xpd->table->next_in_idx, xpd->in_off);

			printk("ise%u.%u: read_pstate=%u read_timeout=%u\n",
			       xsp->id, xpd->channel,
			       xpd->read_pstate, xpd->read_timeout);

			for (idx = 0 ;  idx < CHANNEL_OBUFS ;  idx += 1)
			      printk("ise%u.%u: obuf %u: "
				     "ptr=%x, count=%u\n",
				     xsp->id, xpd->channel, idx,
				     xpd->table->out[idx].ptr,
				     xpd->table->out[idx].count);

			for (idx = 0 ;  idx < CHANNEL_IBUFS ;  idx += 1)
			      printk("ise%u.%u: ibuf %u: "
				     "ptr=%x, count=%u\n",
				     xsp->id, xpd->channel, idx,
				     xpd->table->in[idx].ptr,
				     xpd->table->in[idx].count);


			xpd = xpd->next;
		  } while (xsp->channels != xpd);
	    }

	    if (xsp->root) {
		  for (idx = 0 ;  idx < ROOT_TABLE_CHANNELS ;  idx += 1) {
			if (xsp->root->chan[idx].ptr == 0)
			      continue;

			printk("ise%u chan %u: ptr=0x%x, magic=%x\n",
			       xsp->id, idx,
			       xsp->root->chan[idx].ptr,
			       xsp->root->chan[idx].magic);
		  }

		  for (idx = 0 ;  idx < 16 ;  idx += 1) {
			printk("ise%u frame %u: ptr=0x%x, magic=0x%x\n",
			       xsp->id, idx,
			       xsp->root->frame_table[idx].ptr,
			       xsp->root->frame_table[idx].magic);
		  }
	    } else {
		  printk("ise%u: No root table, so no frames.\n", xsp->id);
	    }

	    break;

      }

      irp->IoStatus.Status = STATUS_SUCCESS;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return STATUS_SUCCESS;
}

static NTSTATUS isex_timeout(DEVICE_OBJECT*dev, IRP*irp)
{
      struct instance_t*xsp = *((struct instance_t**)dev->DeviceExtension);
      struct channel_t*xpd;
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);

      struct ucrx_timeout_s*arg;

      if (stp->Parameters.DeviceIoControl.InputBufferLength != sizeof(*arg)) {
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

      arg = (struct ucrx_timeout_s*)irp->AssociatedIrp.SystemBuffer;

      xpd = channel_by_id(xsp, arg->id);

	/* Make sure the channel really does exist. */
      if (xpd == 0) {
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

	/* If this is any value other then force, then just write the
	   new timeout into the channel structure. It will take effect
	   automatically when the next read is started. */
      if (arg->read_timeout != UCRX_TIMEOUT_FORCE) {
	    xpd->read_timeout = arg->read_timeout;
	    irp->IoStatus.Status = STATUS_SUCCESS;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_SUCCESS;
      }

      
	/*  Invoke the read timeout explicitly for the selected
	    channel. The read_timeout function handles synchronization
	    with the reads and read cancels. */
      KeCancelTimer(&xpd->read_timer);
      read_timeout(0, xpd, 0, 0);


      irp->IoStatus.Status = STATUS_SUCCESS;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return STATUS_SUCCESS;
}

static NTSTATUS isex_get_trace(DEVICE_OBJECT*dev, IRP*irp)
{
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);

      if (stp->Parameters.DeviceIoControl.OutputBufferLength
	            != sizeof debug_flag) {
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

      *(unsigned long*)irp->AssociatedIrp.SystemBuffer = debug_flag;

      irp->IoStatus.Status = STATUS_SUCCESS;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return STATUS_SUCCESS;
}

static NTSTATUS isex_set_trace(DEVICE_OBJECT*dev, IRP*irp)
{
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      unsigned long arg;

      if (stp->Parameters.DeviceIoControl.InputBufferLength != sizeof arg) {
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

      arg = *(unsigned long*)irp->AssociatedIrp.SystemBuffer;

      printk("ise: Set trace %x\n", arg);
      debug_flag = arg;

      irp->IoStatus.Status = STATUS_SUCCESS;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return STATUS_SUCCESS;
}

static NTSTATUS isex_board_type(DEVICE_OBJECT*dev, IRP*irp)
{
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct instance_t*xsp = *((struct instance_t**)dev->DeviceExtension);

      if (stp->Parameters.DeviceIoControl.OutputBufferLength != sizeof(int)) {
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

      *(int*)irp->AssociatedIrp.SystemBuffer = UCRX_BOARD_TYPE_ISE;

      if (xsp->dev_ops == &jse_operations)
	    *(int*)irp->AssociatedIrp.SystemBuffer = UCRX_BOARD_TYPE_JSE;
      if (xsp->dev_ops == &ejse_operations)
	    *(int*)irp->AssociatedIrp.SystemBuffer = UCRX_BOARD_TYPE_EJSE;

      irp->IoStatus.Information = sizeof(int);
      irp->IoStatus.Status = STATUS_SUCCESS;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return STATUS_SUCCESS;
}

/*
 * Handle Cancel of a MAKE_MAP_FRAME IRP. Figure out the frame that
 * this IRP is referring to, make that frame as ready to be cleaned
 * up, and schedule a workitem to do all the work. Note that we do not
 * actually cancel the IRP here, we arrange for the cancel to happen.
 */
static void map_cancel(DEVICE_OBJECT*dev, IRP*irp)
{
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct instance_t*xsp = *((struct instance_t**)dev->DeviceExtension);

      int fidx;

      IoReleaseCancelSpinLock(irp->CancelIrql);

      for (fidx = 0 ;  fidx < 16 ;  fidx += 1) {
	    if (xsp->frame_mdl_irp[fidx] == irp) {
		  xsp->frame_cleanup_mask |= 1 << fidx;
		  break;
	    }
      }

	/* This shouldn't happen. This Cancel should only happen to
	   IRPs that are listed as an mdl_irp for a frame. */

      if (fidx >= 16) {
	    printk("isex%u: map_cancel doesn't match any frame?\n", xsp->id);
	      /* This IoSetCancelRoutine should not be necessary, but
		 we are in a place where we shouldn't be, so make sure
		 there really in no cancel routine. */
	    IoSetCancelRoutine(irp, 0);
	    irp->IoStatus.Status = STATUS_CANCELLED;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return;
      }

	/* If cleanup is not already in progress, then allocate and
	   queue a cleanup work item. Do *not* complete the IRP here,
	   leave that to the frame_cleanup workitem. */

      if (! xsp->frame_cleanup_work) {
	    printk("isex%u: map_cancel Activating cancel, mask=0x%x\n",
		   xsp->id, xsp->frame_cleanup_mask);
	    xsp->frame_cleanup_work = IoAllocateWorkItem(dev);
	    IoQueueWorkItem(xsp->frame_cleanup_work,
			    frame_cleanup_workitem,
			    DelayedWorkQueue, 0);
      }
}


/*
 * This is a root_to_board completion routine that the map_frame uses
 * to *not* complete the IRP. We leave the map IRP perpetually pending
 * in order to hold the user pages down.
 */
static NTSTATUS map_complete(struct instance_t*xsp, IRP*irp)
{
      KIRQL save_irql;
      int frame_id;

      printk("isex%u: mapping complete.\n", xsp->id);

	/* Map the IRP back to the frame that is being mapped. */
      for (frame_id = 0 ;  frame_id < 16 ;  frame_id += 1) {
	    if (xsp->frame_mdl_irp[frame_id] == irp)
		  break;
      }

      if (frame_id >= 16) {
	    printk("isex%u: No frame to match map irp?!\n", xsp->id);
	    return STATUS_SUCCESS;
      }

	/* Look to see if there is an IRP waiting for the map to
	   complete. If there is, complete it and clear it from the
	   wait table. */

      KeAcquireSpinLock(&xsp->mutex, &save_irql);

	/* XXXX */
      IoSetCancelRoutine(irp, &map_cancel);

      xsp->frame_done_flags |= 1 << frame_id;
      if (xsp->frame_wait_irp[frame_id]) {
	    IRP*wait_irp = xsp->frame_wait_irp[frame_id];
	    xsp->frame_wait_irp[frame_id] = 0;
	    wait_irp->IoStatus.Status = STATUS_SUCCESS;
	    wait_irp->IoStatus.Information = 0;
	    IoCompleteRequest(wait_irp, IO_NO_INCREMENT);
      }

      KeReleaseSpinLock(&xsp->mutex, save_irql);

      return STATUS_SUCCESS;
}

static NTSTATUS isex_make_map_frame(DEVICE_OBJECT*dev, IRP*irp)
{
      int rc;
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct instance_t*xsp = *((struct instance_t**)dev->DeviceExtension);

      struct IsexMmapInfo*arg;

      if (stp->Parameters.DeviceIoControl.InputBufferLength != sizeof(*arg)) {
	    printk("isex%u: Invalid input buffer? length=%u, expecting %u\n",
		   xsp->id, stp->Parameters.DeviceIoControl.InputBufferLength,
		   sizeof(*arg));
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

      if (irp->AssociatedIrp.SystemBuffer == 0) {
	    printk("isex%u: Invalid input buffer?\n", xsp->id);
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

      arg = (struct IsexMmapInfo*)irp->AssociatedIrp.SystemBuffer;

      if (arg->frame_id >= 16) {
	    printk("isex%u: Invalid frame id: %d\n", xsp->id, arg->frame_id);
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

	/* Check if frame is already present. */
      if (xsp->frame_mdl_irp[arg->frame_id]) {
	    printk("isex%u: frame %u already present.\n",
		   xsp->id, arg->frame_id);
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

	/* Frame must be page aligned. */
      if (MmGetMdlByteOffset(irp->MdlAddress) != 0) {
	    printk("isex%u: frame %u page invalid offset=%u.\n",
		   xsp->id, arg->frame_id,
		   MmGetMdlByteOffset(irp->MdlAddress));
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

	/* Save the pended IRP for the frame. */
      xsp->frame_mdl_irp[arg->frame_id] = irp;


	/* Report the frame to the board by mapping it and filling in
	   the a frame table. This function takes the MdlAddress of
	   the IRP, which Windows already pinned into memory, and maps
	   all the pages. The result is the frame_tab filled in for
	   the named frame. */
      rc = ise_map_frame(xsp, arg->frame_id, irp->MdlAddress);

      if (rc < 0) {
	    IO_ERROR_LOG_PACKET*event;
	    unsigned psize = sizeof(IO_ERROR_LOG_PACKET) + 2*sizeof(ULONG);

	    event = IoAllocateErrorLogEntry(xsp->fdo, (UCHAR)psize);
	    event->ErrorCode = ISE_FRAME_MAP_FAILED;
	    event->UniqueErrorValue = 0;
	    event->FinalStatus = STATUS_UNSUCCESSFUL;
	    event->MajorFunctionCode = 0;
	    event->IoControlCode = 0;
	    event->DumpDataSize = 3*sizeof(ULONG);
	    event->DumpData[0] = arg->frame_id;
	    event->DumpData[1] = MmGetMdlByteCount(irp->MdlAddress);
	    event->DumpData[2] = 0;
	    IoWriteErrorLogEntry(event);

	    printk("isex%u: frame %u failed to map.\n",
		   xsp->id, arg->frame_id);

	    xsp->frame_mdl_irp[arg->frame_id] = 0;

	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

      printk("isex%u: Frame %d mapped %d pages. Updating root table.\n",
	     xsp->id, arg->frame_id, rc);

	/* Report the frame to the board by updating the root
	   table. Note that the root_to_board function marks the IRP
	   as pending, so we don't need to mark it ourselves. */
      { NTSTATUS rc;
        PHYSICAL_ADDRESS newrootl;
	struct root_table*newroot = duplicate_root(xsp, &newrootl);
	if (newroot == 0) {
	      irp->IoStatus.Status = STATUS_NO_MEMORY;
	      irp->IoStatus.Information = 0;
	      IoCompleteRequest(irp, IO_NO_INCREMENT);
	      return STATUS_NO_MEMORY;
	}
	newroot->frame_table[arg->frame_id].ptr
	      = xsp->frame_tab[arg->frame_id]->self;
	newroot->frame_table[arg->frame_id].magic
	      = xsp->frame_tab[arg->frame_id]->magic;

	rc = root_to_board(xsp, irp, newroot, newrootl, map_complete, 0);
	if (rc != STATUS_PENDING) {
	      printk("ise%u: warning: root_to_board did not return PENDING?\n",
		     xsp->id);
	}
      }

	/* Leave the irp perpetually pending. This holds the pages of
	   the MDL pinned into memory. The unmap will complete the IRP
	   after the frame is unmapped. */
      return STATUS_PENDING;
}

static NTSTATUS isex_wait_map_frame(DEVICE_OBJECT*dev, IRP*irp)
{
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct instance_t*xsp = *((struct instance_t**)dev->DeviceExtension);

      KIRQL save_irql;
      struct IsexMmapInfo*arg;

      if (stp->Parameters.DeviceIoControl.InputBufferLength != sizeof(*arg)) {
	    printk("wait_map_frame Input buffer length %d != %d\n",
		   stp->Parameters.DeviceIoControl.InputBufferLength,
		   sizeof(*arg));
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

      arg = (struct IsexMmapInfo*)irp->AssociatedIrp.SystemBuffer;

      if (arg->frame_id >= 16) {
	    printk("wait_map_frame Invalid frame_id %d\n",
		   arg->frame_id);
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }


	/* Check that this frame is active. If there is no IRP for the
	   frame mapping, then this frame doesn't exist. */

      if (xsp->frame_mdl_irp[arg->frame_id] == 0) {
	    printk("wait_map_frame Invalid frame_id %d\n",
		   arg->frame_id);
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

      KeAcquireSpinLock(&xsp->mutex, &save_irql);

      if (xsp->frame_wait_irp[arg->frame_id] != 0) {
	    printk("wait_map_frame already waiting for frame_id %d\n",
		   arg->frame_id);
	    KeReleaseSpinLock(&xsp->mutex, save_irql);
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

	/* If we know the frame is already done, then complete the
	   wait now with SUCCESS. */

      if (xsp->frame_done_flags & (1 << arg->frame_id)) {
	    KeReleaseSpinLock(&xsp->mutex, save_irql);
	    irp->IoStatus.Status = STATUS_SUCCESS;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_SUCCESS;
      }

      xsp->frame_wait_irp[arg->frame_id] = irp;
      IoMarkIrpPending(irp);

      KeReleaseSpinLock(&xsp->mutex, save_irql);

      return STATUS_PENDING;
};

static NTSTATUS unmap_unmake_frame_2(struct instance_t*xsp, IRP*irp);

static NTSTATUS isex_unmap_unmake_frame(DEVICE_OBJECT*dev, IRP*irp)
{
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct instance_t*xsp = *((struct instance_t**)dev->DeviceExtension);

      struct IsexMmapInfo*arg;

      if (stp->Parameters.DeviceIoControl.InputBufferLength != sizeof(*arg)) {
	    printk("unmap_unmake_frame Input buffer length %d != %d\n",
		   stp->Parameters.DeviceIoControl.InputBufferLength,
		   sizeof(*arg));
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

      arg = (struct IsexMmapInfo*)irp->AssociatedIrp.SystemBuffer;

      if (arg->frame_id >= 16) {
	    printk("unmap_unmake_frame Invalid frame_id %d\n",
		   arg->frame_id);
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

	/* Check if frame is actually present. */
      if (xsp->frame_mdl_irp[arg->frame_id] == 0) {
	    printk("unmap_unmake_frame frame_id %d is not mapped\n",
		   arg->frame_id);
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

	/* Edit root table. */
      { NTSTATUS rc;
        PHYSICAL_ADDRESS newrootl;
	struct root_table*newroot = duplicate_root(xsp, &newrootl);
	if (newroot == 0) {
	      irp->IoStatus.Status = STATUS_NO_MEMORY;
	      irp->IoStatus.Information = 0;
	      IoCompleteRequest(irp, IO_NO_INCREMENT);
	      return STATUS_NO_MEMORY;
	}
	newroot->frame_table[arg->frame_id].ptr = 0;
	newroot->frame_table[arg->frame_id].magic = 0;

	rc = root_to_board(xsp, irp, newroot, newrootl,
			   unmap_unmake_frame_2, 0);
	if (rc != STATUS_PENDING) {
	      printk("ise%u: warning: root_to_board did not return PENDING?\n",
		     xsp->id);
	      irp->IoStatus.Status = rc;
	      irp->IoStatus.Information = 0;
	      IoCompleteRequest(irp, IO_NO_INCREMENT);
	      return rc;
	}
      }

	/* Wait for the root table to update before continuing. */
      return STATUS_PENDING;
}

/*
 * The root_to_board function calls this function when the root table
 * update is complete. I can finaly unmap the frame and release the
 * IRP that is held in place.
 */
static NTSTATUS unmap_unmake_frame_2(struct instance_t*xsp, IRP*irp)
{
      struct IsexMmapInfo*arg
	    = (struct IsexMmapInfo*)irp->AssociatedIrp.SystemBuffer;

      ise_unmap_frame(xsp, arg->frame_id);

	/* Complete the IRP of the map request. This releases the
	   memory for the process. */
      IoSetCancelRoutine(xsp->frame_mdl_irp[arg->frame_id], 0);
      xsp->frame_mdl_irp[arg->frame_id]->IoStatus.Status = STATUS_SUCCESS;
      xsp->frame_mdl_irp[arg->frame_id]->IoStatus.Information = 0;
      IoCompleteRequest(xsp->frame_mdl_irp[arg->frame_id], IO_NO_INCREMENT);

      xsp->frame_mdl_irp[arg->frame_id] = 0;

	/* Mark the frame as no longer in use. */
      xsp->frame_done_flags &= ~( 1 << arg->frame_id );
	/* Make extra sure the frame is not marked for cleanup. */
      xsp->frame_cleanup_mask &= ~( 1 << arg->frame_id );

	/* The UNMAP itself is now done. */
      irp->IoStatus.Status = STATUS_SUCCESS;
      irp->IoStatus.Information = 0;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return STATUS_SUCCESS;
}

NTSTATUS isex_ioctl(DEVICE_OBJECT*dev, IRP*irp)
{
      IO_STACK_LOCATION*iop = IoGetCurrentIrpStackLocation(irp);

      switch (iop->Parameters.DeviceIoControl.IoControlCode) {

	  case UCRX_RESTART_BOARD:
	    return ucrx_restart_board(dev, irp);

	  case UCRX_RUN_PROGRAM:
	    return isex_run_program(dev, irp);

	  case UCRX_TIMEOUT:
	    return isex_timeout(dev, irp);

	  case UCRX_GET_TRACE:
	    return isex_get_trace(dev, irp);

	  case UCRX_SET_TRACE:
	    return isex_set_trace(dev, irp);

	  case UCRX_DIAGNOSE:
	    return isex_diagnose(dev, irp);

	  case UCRX_BOARD_TYPE:
	    return isex_board_type(dev, irp);

	  case ISEX_MAKE_MAP_FRAME:
	    return isex_make_map_frame(dev, irp);

	  case ISEX_WAIT_MAP_FRAME:
	    return isex_wait_map_frame(dev, irp);

	  case ISEX_UNMAP_UNMAKE_FRAME:
	    return isex_unmap_unmake_frame(dev, irp);

      }

      irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
      irp->IoStatus.Information = 0;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return STATUS_UNSUCCESSFUL;
}

static NTSTATUS frame_cleanup_2(struct instance_t*xsp, IRP*irp);

/*
 * This function is called in IRQL==PASSIVE from the IRP_MJ_CLEANUP
 * handler or from work items of the Cancel function for involved
 * IRPs. Start the process of cleaning up marked frames, and arrange
 * for the cancellation of the frame's IRP.
 */
static void frame_cleanup_workitem(DEVICE_OBJECT*dev, void*ctx)
{
      struct instance_t*xsp = *((struct instance_t**)dev->DeviceExtension);
      IRP*irp = (IRP*)ctx;

      PHYSICAL_ADDRESS newrootl;
      struct root_table*newroot;

      NTSTATUS rc;
      int fidx;
      int open_channels = 0;

      for (fidx = 0 ;  fidx < ROOT_TABLE_CHANNELS ;  fidx += 1) {
	    if (xsp->root && (xsp->root->chan[fidx].ptr != 0))
		  open_channels += 1;
      }

	/* If there are no open channels and this cleans up all the
	   remaining frames, then detach from the board
	   completely. Otherwise, edit the root table to remove the
	   now defunct frames. */

      if ((open_channels == 0)
	  && (xsp->frame_cleanup_mask == xsp->frame_done_flags)) {
	    newroot = 0;
	    newrootl.LowPart = 0;
	    newrootl.HighPart = 0;

	    printk("isex%u: arranging to clean up ALL frames.\n", xsp->id);

      } else {
	    newroot = duplicate_root(xsp, &newrootl);
	    if (newroot == 0) {
		  irp->IoStatus.Status = STATUS_NO_MEMORY;
		  irp->IoStatus.Information = 0;
		  IoCompleteRequest(irp, IO_NO_INCREMENT);
		  return;
	    }
	    for (fidx = 0 ;  fidx < 16 ;  fidx += 1) {

		  if (! (xsp->frame_cleanup_mask & (1 << fidx)))
			continue;

		  printk("isex%u: arranging to clean up frame %d.\n",
			 xsp->id, fidx);

		  newroot->frame_table[fidx].ptr = 0;
		  newroot->frame_table[fidx].magic = 0;
	    }
      }

	/* Send off the edits (or detach) to the board. */
      rc = root_to_board(xsp, irp, newroot, newrootl, frame_cleanup_2, 0);
      if (rc != STATUS_PENDING) {
	    printk("ise%u: warning: root_to_board did not return PENDING?\n",
		   xsp->id);
      }
}

/*
 * Continue with edits of the root table by unmapping the frames that
 * I edited out. Unlike with a simple unmap, here we *CANCELLED* the
 * irps that hold the mdl.
 */
static NTSTATUS frame_cleanup_2(struct instance_t*xsp, IRP*irp)
{
      int fidx;
#if 0
      printk("XXXX frame_cleanup_2\n");
#endif
      for (fidx = 0 ;  fidx < 16 ;  fidx += 1) {
	    IRP*irpf = xsp->frame_mdl_irp[fidx];

	    if (irpf == 0)
		  continue;

	      /* Detect that this is not one of the frames being
		 cleaned up. If the frame is still in the root table,
		 then it's cleanup is not in progress so skip it. */
	    if (xsp->root && xsp->root->frame_table[fidx].ptr != 0)
		  continue;

	    printk("isex%u: frame_cleanup_2 cancel frame %d\n", xsp->id, fidx);
	    ise_unmap_frame(xsp, fidx);

	      /* Explicitly remove the cancel routine of the IRP. */
	    IoSetCancelRoutine(irpf, 0);
	      /* Explicitly mark the IRP as cancelled. */
	    irpf->IoStatus.Status = STATUS_CANCELLED;
	    irpf->IoStatus.Information = 0;
	    IoCompleteRequest(irpf, IO_NO_INCREMENT);
	      /* Clear all mention of the IRP from the table list. */
	    xsp->frame_mdl_irp[fidx] = 0;
	      /* Clear the frame map done and cleanup_mask flags. */
	    xsp->frame_done_flags   &= ~( 1 << fidx );
	    xsp->frame_cleanup_mask &= ~( 1 << fidx );
      }

      if (irp) {
	      /* The CLEANUP itself is now done. */
	    irp->IoStatus.Status = STATUS_SUCCESS;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
      }

	/* If after this there are still some frames to clean up, then
	   reschedule the workitem for another pass. Otherwise, delete
	   the workitem. */
      if (xsp->frame_cleanup_mask) {
	    printk("isex%u: Residual pending frame cleanup, mask=0x%x\n",
		   xsp->id, xsp->frame_cleanup_mask);
	    IoQueueWorkItem(xsp->frame_cleanup_work,
			    frame_cleanup_workitem,
			    DelayedWorkQueue, 0);
      } else {
	    IoFreeWorkItem(xsp->frame_cleanup_work);
	    xsp->frame_cleanup_work = 0;
      }

      return STATUS_SUCCESS;
}

/*
 * This creates the DEVICE_OBJECT for the virtual device that is the
 * control port for the specific board. All we need to save access to
 * the real board is save a pointer to the instance_t structure. We
 * are guaranteed by design that the ``fdx'' object will live no
 * longer then the fdo object that holds the instance.
 *
 * Also create the object links in the name space that make this
 * device available to the programmer.
 */
DEVICE_OBJECT *create_isex(DRIVER_OBJECT*drv, struct instance_t*xsp)
{
      NTSTATUS status;
      wchar_t dev_buf[sizeof(devname)/sizeof(devname[0]) + 4];
      wchar_t dos_buf[sizeof(dosname)/sizeof(dosname[0]) + 4];
      wchar_t tmp_buf[8];
      UNICODE_STRING dev_str, dos_str, tmp_str;

      DEVICE_OBJECT*fdx;

      dev_str.Buffer = dev_buf;
      dev_str.MaximumLength = sizeof(dev_buf);
      dev_str.Length = 0;

      RtlAppendUnicodeToString(&dev_str, (wchar_t*)devname);

      tmp_str.Buffer = tmp_buf;
      tmp_str.MaximumLength = sizeof(tmp_buf);
      tmp_str.Length = 0;
      RtlIntegerToUnicodeString(xsp->id, 10, &tmp_str);

      RtlAppendUnicodeStringToString(&dev_str, &tmp_str);

      status = IoCreateDevice(drv, sizeof(struct instance_t*), &dev_str,
			      FILE_DEVICE_UCRX, 0, FALSE, &fdx);

      if (!NT_SUCCESS(status))
	    return 0;
      dos_str.Buffer = dos_buf;
      dos_str.MaximumLength = sizeof(dos_buf);
      dos_str.Length = 0;

      RtlAppendUnicodeToString(&dos_str, (wchar_t*)dosname);

      tmp_str.Buffer = tmp_buf;
      tmp_str.MaximumLength = sizeof(tmp_buf);
      tmp_str.Length = 0;
      RtlIntegerToUnicodeString(xsp->id, 10, &tmp_str);

      RtlAppendUnicodeStringToString(&dos_str, &tmp_str);

      IoCreateSymbolicLink(&dos_str, &dev_str);


	/* The DeviceExtension only needs a pointer to the xsp, which is
	   the DeviceExtension in the real device object. */
      *((struct instance_t**)fdx->DeviceExtension) = xsp;

      fdx->Flags |= DO_BUFFERED_IO;
      fdx->Flags &= ~DO_DEVICE_INITIALIZING;

      return fdx;
}

/*
 * Remove all the access links for the device object, and remove the
 * device object as well. This is the opposite of the create_isex
 * function above.
 */
void remove_isex(DEVICE_OBJECT*fdx)
{
      struct instance_t*xsp = *((struct instance_t**)fdx->DeviceExtension);

	/* Delete the symbolic links for this device. */

      wchar_t dos_buf[sizeof(dosname)/sizeof(dosname[0]) + 4];
      wchar_t tmp_buf[8];
      UNICODE_STRING dos_str, tmp_str;

      dos_str.Buffer = dos_buf;
      dos_str.MaximumLength = sizeof(dos_buf);
      dos_str.Length = 0;

      RtlAppendUnicodeToString(&dos_str, (wchar_t*)dosname);

      tmp_str.Buffer = tmp_buf;
      tmp_str.MaximumLength = sizeof(tmp_buf);
      tmp_str.Length = 0;
      RtlIntegerToUnicodeString(xsp->id, 10, &tmp_str);

      RtlAppendUnicodeStringToString(&dos_str, &tmp_str);

      IoDeleteSymbolicLink(&dos_str);

      IoDeleteDevice(fdx);
}

/*
 * $Log$
 * Revision 1.20  2009/04/07 16:42:36  steve
 *  Remove useless and nonportable message dump.
 *
 * Revision 1.19  2009/04/03 18:21:17  steve
 *  Implement frame64 support in Windows driver.
 *  More robust error handling around root tables.
 *  Keep a deeper root standby list to prevent leaks.
 *
 * Revision 1.18  2008/12/10 21:21:41  steve
 *  Report EJSE boards as EJSE boards.
 *
 * Revision 1.17  2006/08/10 00:13:44  steve
 *  Remember to clear cancel function from map ioctl on cleanup.
 *
 * Revision 1.16  2005/09/15 22:03:42  steve
 *  Protect IoMarkPending of WAIT ioctl from frame completion.
 *
 * Revision 1.15  2005/09/12 21:52:46  steve
 *  More error checking.
 *
 * Revision 1.14  2005/07/26 01:17:32  steve
 *  New method of mapping frames for version 2.5
 *
 * Revision 1.13  2005/03/02 15:25:57  steve
 *  Dump channels in diagnose1.
 *
 * Revision 1.12  2004/08/02 23:45:49  steve
 *  Add UCRX_BOARD_TYPE control
 *
 * Revision 1.11  2004/07/15 04:19:26  steve
 *  Extend to support JSE boards.
 *
 * Revision 1.10  2002/05/13 20:07:52  steve
 *  More diagnostic detail, and check registers.
 *
 * Revision 1.9  2002/04/11 00:49:30  steve
 *  Move FreeCommonBuffers to PASSIVE_MODE using standby lists.
 */

