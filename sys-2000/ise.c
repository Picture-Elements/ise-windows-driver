/*
 * Copyright (c) 2001 Picture Elements, Inc.
 *    Stephen Williams (steve@picturel.com)
 *
 * $Id$
 */

# include  "ise_sys.h"
# include  "isem.h"

static const wchar_t devname[] = L"\\Device\\ise";
static const wchar_t dosname[] = L"\\DosDevices\\ISE";

/*
 * Return the open channel structure that matches the given ID, if the
 * channel is open. Otherwise, return 0.
 */
struct channel_t* channel_by_id(struct instance_t*xsp, unsigned short id)
{
      struct channel_t*cur;

      if (xsp->channels == 0)
	    return 0;

      cur = xsp->channels;
      do {
	    cur = cur->next;
	    if (cur->channel == id)
		  return cur;
      } while (cur != xsp->channels);

      return 0;
}

/*
 * This function is called periodically to remove saved channel
 * tables. These tables are save by the close when it completes,
 * because it needs to leave the tables about for a little while to
 * guard against fishy target board accesses.
 */
static void cleanup_channel_standby_list(struct instance_t*xsp)
{
      struct channel_t*xpd;

      while (xpd = xsp->channel_standby_list) {
	    xsp->channel_standby_list = xpd->next;

	    if (xpd->table) {
		  unsigned idx;
		    /* Release the in and out buffers. */
		  for (idx = 0 ;  idx < CHANNEL_IBUFS ;  idx += 1) {
			if (xpd->in[idx].ptr)
			      xsp->dma->DmaOperations->FreeCommonBuffer(
						   xsp->dma,
						   PAGE_SIZE,
						   xpd->in[idx].ptrl,
						   xpd->in[idx].ptr,
						   FALSE);
		  }

		  for (idx = 0 ;  idx < CHANNEL_OBUFS ;  idx += 1) {
			if (xpd->out[idx].ptr)
			      xsp->dma->DmaOperations->FreeCommonBuffer(
						   xsp->dma,
						   PAGE_SIZE,
						   xpd->out[idx].ptrl,
						   xpd->out[idx].ptr,
						   FALSE);
		  }

		    /* Release the table itself. Poison the memory first. */
		  xpd->table->self = 0;
		  xpd->table->magic = 0x11111111;
		  xsp->dma->DmaOperations->FreeCommonBuffer(xsp->dma,
				  sizeof(*xpd->table), xpd->tablel,
				  xpd->table, FALSE);
	    }

	      /* All done, release the channel_t object. */
	    ExFreePool(xpd);
      }
}

/*
 * NOTE: This function must be called with the xsp->mutex spin lock
 * already acquired.
 */
static void release_root(struct instance_t*xsp,
			 struct root_table*ptr, PHYSICAL_ADDRESS ptrl)
{
      int idx = 0;

      while (xsp->root_standby_table[idx].root && idx < ROOT_STANDBY_TABLE_MAX)
	    idx += 1;

      if (idx >= ROOT_STANDBY_TABLE_MAX) {
	    IO_ERROR_LOG_PACKET*event;
	    unsigned psize = sizeof(IO_ERROR_LOG_PACKET) + 0*sizeof(ULONG);

	    xsp->root_standby_leak += 1;

	    event = IoAllocateErrorLogEntry(xsp->fdo, (UCHAR)psize);
	    event->ErrorCode = ISE_ROOT_STANDBY_LEAK;
	    event->UniqueErrorValue = 0;
	    event->FinalStatus = STATUS_SUCCESS;
	    event->MajorFunctionCode = 0;
	    event->IoControlCode = 0;
	    event->DumpDataSize = 1*sizeof(ULONG);
	    event->DumpData[0] = xsp->root_standby_leak;
	    IoWriteErrorLogEntry(event);
	    return;
      }

      xsp->root_standby_table[idx].root = ptr;
      xsp->root_standby_table[idx].rootl = ptrl;
}

/*
 * Make a copy of the current root table, with the magic numbers
 * twisted so that this is a valid table. The caller can then do minor
 * modifications to this copy before installing it.
 */
struct root_table*duplicate_root(struct instance_t*xsp, PHYSICAL_ADDRESS*ptrl)
{
      int idx;
      KIRQL save_irql;
      struct root_table*newroot;

      KeAcquireSpinLock(&xsp->mutex, &save_irql);
      idx = 0;
      while (xsp->root_standby_table[idx].root == 0 && idx < ROOT_STANDBY_TABLE_MAX)
	    idx += 1;

      if (idx < ROOT_STANDBY_TABLE_MAX) {
	    newroot = xsp->root_standby_table[idx].root;
	    *ptrl = xsp->root_standby_table[idx].rootl;
	    xsp->root_standby_table[idx].root = 0;

	    KeReleaseSpinLock(&xsp->mutex, save_irql);

      } else {
	    KeReleaseSpinLock(&xsp->mutex, save_irql);
	    newroot = (struct root_table*)
		  xsp->dma->DmaOperations->AllocateCommonBuffer(xsp->dma,
			      sizeof(struct root_table), ptrl, FALSE);

	    if (newroot == 0) {
		  IO_ERROR_LOG_PACKET*event;
		  unsigned psize = sizeof(IO_ERROR_LOG_PACKET) + 0*sizeof(ULONG);

		  event = IoAllocateErrorLogEntry(xsp->fdo, (UCHAR)psize);
		  event->ErrorCode = ISE_GET_ROOT_TABLE_FAILED;
		  event->UniqueErrorValue = 0;
		  event->FinalStatus = STATUS_UNSUCCESSFUL;
		  event->MajorFunctionCode = 0;
		  event->IoControlCode = 0;
		  event->DumpDataSize = 0*sizeof(ULONG);
		  event->DumpData[0] = 0;
		  IoWriteErrorLogEntry(event);
		  return 0;
	    }

	    if (ptrl->HighPart != 0) {
		  IO_ERROR_LOG_PACKET*event;
		  unsigned psize = sizeof(IO_ERROR_LOG_PACKET) + 1*sizeof(ULONG);

		  event = IoAllocateErrorLogEntry(xsp->fdo, (UCHAR)psize);
		  event->ErrorCode = ISE_ROOT_TAB_64BITS;
		  event->UniqueErrorValue = 0;
		  event->FinalStatus = STATUS_SUCCESS;
		  event->MajorFunctionCode = 0;
		  event->IoControlCode = 0;
		  event->DumpDataSize = 2*sizeof(ULONG);
		  event->DumpData[0] = ptrl->LowPart;
		  event->DumpData[1] = ptrl->HighPart;
		  IoWriteErrorLogEntry(event);
		  return 0;
	    }
      }

      if (xsp->root) {
	    RtlCopyMemory(newroot, xsp->root, sizeof (struct root_table));
	    newroot->self = ptrl->LowPart;
      } else {
	    RtlZeroMemory(newroot, sizeof(struct root_table));
	    newroot->magic = ROOT_TABLE_MAGIC;
	    newroot->self = ptrl->LowPart;
      }

      return newroot;
}


/*
 * These functions handle sending the root table to the target
 * board. The root_to_board function takes a pointer to a new root
 * table and schedules it for installation. It saves the parameters
 * into a standby space, marks the IRP pending, and tickles the device
 * to interrupt me when it is done.
 *
 * The interrupt handler will schedule the root_to_board_dpc, which
 * suspects (but checks) that the new root has been accepted by the
 * hardware. It releases the old table and saves the new root
 * information in the xsp structure.
 *
 * There is the special case where the new table is a pointer to
 * 0. This causes the hardware to be detached from the table, but the
 * existing table is saved.
 *
 * Synchronization is managed by masking the ISE board interrupts. The
 * ISR cannot happen during the root_to_board function, so a DPC
 * caused by the ISR cannot b scheduled. The root_to_board elicits an
 * interrupt response which in unmasks when the function exits, so the
 * DPC will be scheduled after the call to dev_unmask_irqs. This
 * assures that root_to_board is safe from the dpc, even on
 * multi-processor systems.
 */
static void root_to_board_dpc(KDPC*dpc, void*ctx, void*arg1, void*arg2);

NTSTATUS root_to_board(struct instance_t*xsp, IRP*irp,
		   struct root_table*root, PHYSICAL_ADDRESS rootl,
		   callback_t fun, callback_t timeout_fun)
{
      KIRQL save_irql;
      unsigned long mask;

      KeAcquireSpinLock(&xsp->mutex, &save_irql);
      mask = dev_mask_irqs(xsp);

      if (xsp->root_callback != 0) {
	    IO_ERROR_LOG_PACKET*event;
	    unsigned psize = sizeof(IO_ERROR_LOG_PACKET) + 2*sizeof(ULONG);

	    printk("ise%u: warning: root_to_board is busy.\n", xsp->id);
	    dev_unmask_irqs(xsp, mask);
	    KeReleaseSpinLock(&xsp->mutex, save_irql);

	    event = IoAllocateErrorLogEntry(xsp->fdo, (UCHAR)psize);
	    event->ErrorCode = ISE_BUSY_ROOT_TO_BOARD_DPC;
	    event->UniqueErrorValue = 0;
	    event->FinalStatus = STATUS_SUCCESS;
	    event->MajorFunctionCode = 0;
	    event->IoControlCode = 0;
	    event->DumpDataSize = 2*sizeof(ULONG);
	    event->DumpData[0] = 0;
	    event->DumpData[1] = 0;
	    IoWriteErrorLogEntry(event);

	    return STATUS_DEVICE_NOT_READY;
      }

	/* Stash the pointers, the IRP and the callback into a standby
	   area and mark the IRP pending. Note that the IRP is
	   optional, as root table changes may be initiated without an
	   IRP. All the other values are required. */
      xsp->root2 = root;
      xsp->rootl2 = rootl;
      xsp->root_irp = irp;
      xsp->root_callback = fun;
      xsp->root_timeout_callback = timeout_fun;
      if (irp) {
	    IoMarkIrpPending(irp);
	    irp->IoStatus.Status = STATUS_PENDING;
      }

	/* If the timeout is enabled, then set a fixed timeout of 2
	   seconds for waiting for the root table. */
      KeSetTimer(&xsp->root_timer,
		 RtlConvertLongToLargeInteger(-10000000 * 2),
		 &xsp->root_timer_dpc);
		       
	/* Make sure the resp register is different from what I'm
	   about to write into the base register, so that I can
	   properly wait for the resp to change. */
      if (root) dev_set_root_table_resp(xsp, 0);

	/* Set the pointer to the root table. */
      dev_set_root_table_base(xsp, root? root->self : 0);

	/* Signal the hardware that I'm sending a root table. */
      dev_set_bells(xsp, ROOT_TABLE_BELLMASK);

      dev_unmask_irqs(xsp, mask);
      KeReleaseSpinLock(&xsp->mutex, save_irql);
      return STATUS_PENDING;
}

NTSTATUS complete_success(struct instance_t*xsp, IRP*irp)
{
      irp->IoStatus.Status = STATUS_SUCCESS;
      irp->IoStatus.Information = 0;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return STATUS_SUCCESS;
}

/*
 * This function actually implements both the root_to_board DPC and
 * the root_timer DPC. The arg1 is a flag that is true if this is a
 * timeout.
 */
static void root_to_board_dpc(KDPC*dpc, void*ctx, void*arg1, void*arg2)
{
      struct instance_t*xsp = (struct instance_t*)ctx;
      IRP*irp;

      int timeout_flag = (arg1 != 0);

      KeAcquireSpinLockAtDpcLevel(&xsp->mutex);

      irp = xsp->root_irp;

      KeCancelTimer(&xsp->root_timer);

	/* Detect possible spurious DPC calls. Maybe there is no IRP
	   waiting, or maybe the ISE board is not yet finished the
	   change. */
      if (xsp->root_callback == 0) {
	    IO_ERROR_LOG_PACKET*event;
	    unsigned psize = sizeof(IO_ERROR_LOG_PACKET) + 4*sizeof(ULONG);

	    KeReleaseSpinLockFromDpcLevel(&xsp->mutex);
	    printk("ise%u: Spurious root_to_board_dpc?\n", xsp->id);

	    event = IoAllocateErrorLogEntry(xsp->fdo, (UCHAR)psize);
	    event->ErrorCode = ISE_SPURIOUS_ROOT_TO_BOARD_DPC;
	    event->UniqueErrorValue = 0;
	    event->FinalStatus = STATUS_SUCCESS;
	    event->MajorFunctionCode = 0;
	    event->IoControlCode = 0;
	    event->DumpDataSize = 5*sizeof(ULONG);
	    event->DumpData[0] = xsp->root? xsp->root->self : 0;
	    event->DumpData[1] = dev_get_root_table_resp(xsp);
	    event->DumpData[2] = xsp->root2? xsp->root2->self : 0;
	    event->DumpData[3] = irp != 0;
	    event->DumpData[4] = timeout_flag;
	    IoWriteErrorLogEntry(event);

	    return;
      }

      if (timeout_flag) {
	    IO_ERROR_LOG_PACKET*event;
	    unsigned psize = sizeof(IO_ERROR_LOG_PACKET) + 2*sizeof(ULONG);
	    event = IoAllocateErrorLogEntry(xsp->fdo, (UCHAR)psize);
	    event->ErrorCode = ISE_ROOT_TIMEOUT;
	    event->UniqueErrorValue = 0;
	    event->FinalStatus = STATUS_SUCCESS;
	    event->MajorFunctionCode = 0;
	    event->IoControlCode = 0;
	    event->DumpDataSize = 3*sizeof(ULONG);
	    event->DumpData[0] = xsp->root? xsp->root->self : 0;
	    event->DumpData[1] = dev_get_root_table_resp(xsp);
	    event->DumpData[2] = xsp->root2? xsp->root2->self : 0;
	    IoWriteErrorLogEntry(event);

	      /* The timeout expired, but this seems to be a special
		 case that we are clearing the root table pointer. If
		 the board is not responding, then *force* the response
		 pointer to 0 and hope for the best. */
	    if (xsp->root2 == 0) {
		  dev_set_root_table_resp(xsp, 0);
	    }

      }

      if (dev_get_root_table_resp(xsp) != (xsp->root2? xsp->root2->self : 0)) {

	      /* The board is not responsing to the root table. So
		 give up, poison the new root table and release it. */
	    IO_ERROR_LOG_PACKET*event;
	    unsigned psize = sizeof(IO_ERROR_LOG_PACKET) + 2*sizeof(ULONG);
	    event = IoAllocateErrorLogEntry(xsp->fdo, (UCHAR)psize);
	    event->ErrorCode = ISE_ROOT_STUCK;
	    event->UniqueErrorValue = 0;
	    event->FinalStatus = STATUS_SUCCESS;
	    event->MajorFunctionCode = 0;
	    event->IoControlCode = 0;
	    event->DumpDataSize = 3*sizeof(ULONG);
	    event->DumpData[0] = xsp->root? xsp->root->self : 0;
	    event->DumpData[1] = dev_get_root_table_resp(xsp);
	    event->DumpData[2] = xsp->root2? xsp->root2->self : 0;
	    IoWriteErrorLogEntry(event);

	    printk("ise%u: warning: root not received yet: "
		   "resp=%x, self=%x.\n", xsp->id,
		   dev_get_root_table_resp(xsp),
		   (xsp->root2? xsp->root2->self : 0));

	    if (xsp->root2) {
		  RtlZeroMemory(xsp->root2, sizeof (struct root_table));
		  release_root(xsp, xsp->root2, xsp->rootl2);
		  xsp->root2 = 0;
	    }

      } else {

	      /* Poison then release the old root table, and set the
		 pointers to point to the new root table. Save the
		 existing root for possible reuse later. */
	    if (xsp->root) {
		  RtlZeroMemory(xsp->root, sizeof (struct root_table));
		  release_root(xsp, xsp->root, xsp->rootl);
	    }

	    xsp->root  = xsp->root2;
	    xsp->rootl = xsp->rootl2;

	    xsp->root2 = 0;
      }

      xsp->root_irp = 0;

      if (timeout_flag && xsp->root_timeout_callback) {
	    callback_t callback = xsp->root_timeout_callback;
	    xsp->root_callback = 0;
	    xsp->root_timeout_callback = 0;
	    KeReleaseSpinLockFromDpcLevel(&xsp->mutex);
	    (*callback)(xsp, irp);
      } else {
	    callback_t callback = xsp->root_callback;
	    xsp->root_callback = 0;
	    xsp->root_timeout_callback = 0;
	    KeReleaseSpinLockFromDpcLevel(&xsp->mutex);
	    (*callback)(xsp, irp);
      }
}


static void root_timer_dpc(KDPC*dpc, void*ctx, void*arg1, void*arg2)
{
      root_to_board_dpc(dpc, ctx, (void*)1, 0);
}

/*
 * This entry is in response to a CreateFile. We create a new channel
 * to the device and adjust the tables as needed. Remember to tell the
 * ISE board about the change.
 */
NTSTATUS dev_create(DEVICE_OBJECT*dev, IRP*irp)
{
      KIRQL save_irql;
      unsigned idx;
      struct instance_t*xsp = (struct instance_t*)dev->DeviceExtension;
      struct channel_t*xpd;

	/* Allocate the new channel_t structure t represent the open
	   channel. This will last as long as the handle is open. */
      xpd = ExAllocatePool(NonPagedPool, sizeof(struct channel_t));
      if (xpd == 0) {
	    printk("ise%u: No memory for channel structure\n", xsp->id);
	    irp->IoStatus.Status = STATUS_NO_MEMORY;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_NO_MEMORY;
      }

      RtlZeroMemory(xpd, sizeof(*xpd));

	/* Stash the channel_t structure into the FsContext. */
      get_channel(irp) = xpd;

	/* Prevent a duplicate open of channel 0. */
      if (channel_by_id(xsp, 0) != 0) {
	    printk("ise%u: Channel 0 busy during create.\n", xsp->id);
	    irp->IoStatus.Status = STATUS_DEVICE_ALREADY_ATTACHED;
	    goto error_cleanup;
      }

      xpd->channel = 0;
      xpd->xsp = xsp;

	/* Initialize members related to read timeouts. */
      xpd->read_timeout = UCRX_TIMEOUT_OFF;
      KeInitializeTimer(&xpd->read_timer);
      KeInitializeDpc(&xpd->read_timer_dpc, &read_timeout, xpd);
      xpd->read_pending = 0;

      xpd->table = (struct channel_table*)
	    xsp->dma->DmaOperations->AllocateCommonBuffer(xsp->dma,
					   sizeof(struct channel_table),
					   &xpd->tablel, FALSE);
      if (xpd->table == 0) {
	    printk("ise%u: No memory for channel table\n", xsp->id);
	    irp->IoStatus.Status = STATUS_NO_MEMORY;
	    goto error_cleanup;
      }

      RtlZeroMemory(xpd->table, sizeof (*xpd->table));
      xpd->table->magic = CHANNEL_TABLE_MAGIC;
      xpd->table->self = xpd->tablel.LowPart;
      xpd->table->frame = 0;
      xpd->table->first_out_idx = 0;
      xpd->table->next_out_idx  = 0;
      xpd->table->first_in_idx  = 0;
      xpd->table->next_in_idx   = 0;


      for (idx = 0 ;  idx < CHANNEL_IBUFS ;  idx += 1) {
	    xpd->in[idx].ptr = xsp->dma->DmaOperations->AllocateCommonBuffer(
				       xsp->dma, PAGE_SIZE,
				       &xpd->in[idx].ptrl, FALSE);
	    xpd->table->in[idx].ptr = xpd->in[idx].ptrl.LowPart;
	    xpd->table->in[idx].count = PAGE_SIZE;
      }

      for (idx = 0 ;  idx < CHANNEL_OBUFS ;  idx += 1) {
	    xpd->out[idx].ptr = xsp->dma->DmaOperations->AllocateCommonBuffer(
				       xsp->dma, PAGE_SIZE,
				       &xpd->out[idx].ptrl, FALSE);
	    xpd->table->out[idx].ptr = xpd->out[idx].ptrl.LowPart;
	    xpd->table->out[idx].count = PAGE_SIZE;
      }


      cleanup_channel_standby_list(xsp);


	/* Put the channel information into the channel list so that
	   arriving packets can be properly dispatched to the
	   process. */

      KeAcquireSpinLock(&xsp->mutex, &save_irql);
      if (xsp->channels == 0) {
	    xsp->channels = xpd;
	    xpd->next = xpd;
	    xpd->prev = xpd;
      } else {
	    xpd->next = xsp->channels;
	    xpd->prev = xsp->channels->prev;
	    xpd->next->prev = xpd;
	    xpd->prev->next = xpd;
      }
      KeReleaseSpinLock(&xsp->mutex, save_irql);

      if (debug_flag & UCR_TRACE_CHAN)
	    printk("ise%u: create channel\n", xsp->id);


	/* Make a new root table with this new channel, then start the
	   transfer to the target board. This thread goes pending at
	   this point. */
      { NTSTATUS rc;
        PHYSICAL_ADDRESS newrootl;
        struct root_table*newroot = duplicate_root(xsp, &newrootl);
	if (newroot == 0) {
	      irp->IoStatus.Status = STATUS_NO_MEMORY;
	      goto error_cleanup;
	}

        newroot->chan[0].magic = xpd->table->magic;
	newroot->chan[0].ptr   = xpd->table->self;

	rc = root_to_board(xsp, irp, newroot, newrootl, &complete_success, 0);
	if (rc != STATUS_PENDING) {
	      irp->IoStatus.Status = rc;
	      printk("ise%u: error from root_to_board\n", xsp->id);
	      goto error_cleanup;
	}

	return STATUS_PENDING;
      }

 error_cleanup:
      ExFreePool(xpd);
      get_channel(irp) = 0;

      { NTSTATUS status = irp->IoStatus.Status;
        irp->IoStatus.Information = 0;
	IoCompleteRequest(irp, IO_NO_INCREMENT);
	return status;
      }
}


/*
 * A CloseHandle calls this function, causing a channel to be
 * removed. First, I change the root table to remove the channel from
 * the view of the target board. After that completes, the dev_close_2
 * function will be called to clean up the rest of the way.
 */
static NTSTATUS dev_close_2(struct instance_t*xsp, IRP*irp);

NTSTATUS dev_close(DEVICE_OBJECT*dev, IRP*irp)
{
      struct instance_t*xsp = (struct instance_t*)dev->DeviceExtension;
      struct channel_t*xpd = get_channel(irp);

      PHYSICAL_ADDRESS newrootl;
      struct root_table*newroot = 0;

      int channels_remaining_in_table = 0;
      int frames_remaining_in_table = 0;

	/* This shouldn't be necessary, but is defensive. */
      KeCancelTimer(&xpd->read_timer);

	/* Count the number of channels remaning all together. This
	   includes the channels held by other processes. We need this
	   number to know if it is time to release the root table. */
      if (xsp->root != 0) {
	    unsigned idx;
	    for (idx = 0 ;  idx < ROOT_TABLE_CHANNELS ;  idx += 1) {
		  if (xsp->root->chan[idx].ptr != 0)
			channels_remaining_in_table += 1;
	    }
      }

	/* If this is the last channel for this process, then remove
	   any frame mappings first. */
      { unsigned fidx;
        for (fidx = 0 ;  fidx < 16 ;  fidx += 1) {
	      if (xsp->frame_tab[fidx])
		    frames_remaining_in_table += 1;
	}
      }

      cleanup_channel_standby_list(xsp);

	/* Now start detaching the channel from the board. This is
	   done by making a new root that writes out the channel
	   tables. If this is the last channel, then removing the root
	   table completely also has the effect of sending a "detach"
	   to the target firmware. */
      if ((channels_remaining_in_table > 1)||(frames_remaining_in_table > 0)) {
	    newroot = duplicate_root(xsp, &newrootl);
	    if (newroot == 0) {
		  irp->IoStatus.Status = STATUS_NO_MEMORY;
		  irp->IoStatus.Information = 0;
		  printk("ise%u: close error from root_to_board\n", xsp->id);
		  IoCompleteRequest(irp, IO_NO_INCREMENT);
		  return STATUS_NO_MEMORY;
	    }
	    newroot->chan[xpd->channel].magic = 0;
	    newroot->chan[xpd->channel].ptr = 0;
      } else {
	    newroot = 0;
      }

      { NTSTATUS rc;
        rc = root_to_board(xsp, irp, newroot, newrootl, &dev_close_2, 0);
	if (rc != STATUS_PENDING) {
	      irp->IoStatus.Status = rc;
	      irp->IoStatus.Information = 0;
	      printk("ise%u: close error from root_to_board\n", xsp->id);
	      IoCompleteRequest(irp, IO_NO_INCREMENT);
	      return rc;
	}
      }
      return STATUS_PENDING;
}

static NTSTATUS dev_close_2(struct instance_t*xsp, IRP*irp)
{
      KIRQL save_irql;
      struct channel_t*xpd = get_channel(irp);

      if (debug_flag & UCR_TRACE_CHAN)
	    printk("ise%u.%u: Close the channel\n", xsp->id, xpd->channel);

	/* No more communication with the target channel, so remove
	   the channel_t structure from the channel list. */

      KeAcquireSpinLock(&xsp->mutex, &save_irql);
      if (xsp->channels == xpd)
	    xsp->channels = xsp->channels->next;
      if (xsp->channels == xpd)
	    xsp->channels = 0;
      else {
	    xpd->prev->next = xpd->next;
	    xpd->next->prev = xpd->prev;
      }
      KeReleaseSpinLock(&xsp->mutex, save_irql);

	/* RReady to release the table itself. Poison the memory
	   first, then drop the channel structure in the the
	   channel_standby_list. This allows the Free functions
	   to be called at more opportune times. */
      KeAcquireSpinLock(&xsp->mutex, &save_irql);
      xpd->table->self = 0;
      xpd->table->magic = 0x11111111;
      xpd->next = xsp->channel_standby_list;
      xsp->channel_standby_list = xpd;
      KeReleaseSpinLock(&xsp->mutex, save_irql);

      irp->IoStatus.Status = STATUS_SUCCESS;
      irp->IoStatus.Information = 0;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return STATUS_SUCCESS;
}

/*
 * This is the interrupt handler that connects to the ISE board.
 */
static BOOLEAN xxisr(PKINTERRUPT irq, void*dev_id)
{
      DEVICE_OBJECT*dev = (DEVICE_OBJECT*)dev_id;
      struct instance_t*xsp = (struct instance_t*)dev->DeviceExtension;

      unsigned long mask = dev_get_bells(xsp);

      if (mask & ROOT_TABLE_BELLMASK)
	    KeInsertQueueDpc(&xsp->root_to_board_dpc, 0, 0);

	/* This bell happens when the target board responds to my
	   sending a status signal. */
      if (mask & STATUS_BELLMASK)
	    ;

	/* This bell happens when it tells me that *it* has changed a
	   channel table. */
      if (mask & CHANGE_BELLMASK) {
	    KeInsertQueueDpc(&xsp->pending_write_dpc, 0, 0);
	    KeInsertQueueDpc(&xsp->pending_read_dpc, 0, 0);
      }


      return mask != 0;
}

/*
 * The start function needs to know detailed device ids so that we can
 * select device access functions. This function gets the entire PCI
 * config space, which can be used by the caller as it sees fit.
 */
static NTSTATUS read_pci_config(DEVICE_OBJECT*fdo, PCI_COMMON_CONFIG*pci)
{
      KEVENT event;
      NTSTATUS status;
      IO_STATUS_BLOCK iostatus;
      IRP*irp;
      IO_STACK_LOCATION*irp_stack;
      DEVICE_OBJECT*pdo;

      KeInitializeEvent(&event, NotificationEvent, FALSE);
      pdo = IoGetAttachedDeviceReference(fdo);

      irp = IoBuildSynchronousFsdRequest(IRP_MJ_PNP, pdo, NULL, 0,
					 NULL, &event, &iostatus);
      if (irp == 0) {
	    status = STATUS_INSUFFICIENT_RESOURCES;
	    goto out;
      }

      irp_stack = IoGetNextIrpStackLocation(irp);
      irp_stack->MinorFunction = IRP_MN_READ_CONFIG;
      irp_stack->Parameters.ReadWriteConfig.WhichSpace = PCI_WHICHSPACE_CONFIG;
      irp_stack->Parameters.ReadWriteConfig.Buffer = pci;
      irp_stack->Parameters.ReadWriteConfig.Offset = 0;
      irp_stack->Parameters.ReadWriteConfig.Length = sizeof(*pci);

      irp->IoStatus.Status = STATUS_NOT_SUPPORTED;
      status = IoCallDriver(pdo, irp);
      if (status == STATUS_PENDING) {
	    KeWaitForSingleObject(&event, Executive, KernelMode, FALSE, NULL);
	    status = iostatus.Status;
      }

 out:
      ObDereferenceObject(pdo);
      return status;
}

static IO_ALLOCATION_ACTION dma_adapter_stub(DEVICE_OBJECT*fdo,
					     IRP*irp, void*map_base,
					     void*context)
{
      return DeallocateObjectKeepRegisters;
}

/*
 * This function is called by a PNP_MN_START_DEVICE irp to handle the
 * allocation of resources from the kernel. I get all my I/O memory
 * and IRQ assignments this way. This is separate from the create_ise
 * bit that allocates the windows objects to represent the device.
 */
NTSTATUS pnp_start_ise(DEVICE_OBJECT*fdo, IRP*irp)
{
      NTSTATUS status;
      unsigned rdx;
      IO_STACK_LOCATION*iop;
      CM_RESOURCE_LIST*res, *raw;
      PCI_COMMON_CONFIG pci;
      struct instance_t*xsp = (struct instance_t*)fdo->DeviceExtension;

      xsp->fdo = fdo;

      printk("ise%u: pnp_start_ise)\n", xsp->id);

      iop = IoGetCurrentIrpStackLocation(irp);
      raw = iop->Parameters.StartDevice.AllocatedResources;
      res = iop->Parameters.StartDevice.AllocatedResourcesTranslated;

      if (res == 0) {
	    IO_ERROR_LOG_PACKET*event;
	    unsigned psize = sizeof(IO_ERROR_LOG_PACKET);
	    event = IoAllocateErrorLogEntry(fdo, (UCHAR)psize);
	    event->ErrorCode = ISE_NO_RESOURCE_MAP;
	    event->UniqueErrorValue = 0;
	    event->FinalStatus = STATUS_UNSUCCESSFUL;
	    event->MajorFunctionCode = 0;
	    event->IoControlCode = 0;
	    event->DumpData[0] = 0;
	    IoWriteErrorLogEntry(event);

	    printk("ise%u: no resources?!\n", xsp->id);
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

      status = read_pci_config(fdo, &pci);
      if (!NT_SUCCESS(status)) {
	    IO_ERROR_LOG_PACKET*event;
	    unsigned psize = sizeof(IO_ERROR_LOG_PACKET);
	    event = IoAllocateErrorLogEntry(fdo, (UCHAR)psize);
	    event->ErrorCode = ISE_NO_RESOURCE_MAP;
	    event->UniqueErrorValue = 1;
	    event->FinalStatus = STATUS_UNSUCCESSFUL;
	    event->MajorFunctionCode = 0;
	    event->IoControlCode = 0;
	    event->DumpData[0] = 0;
	    IoWriteErrorLogEntry(event);

	    printk("ise%u: No PCI config?!\n", xsp->id);
	    return STATUS_UNSUCCESSFUL;
      }

      if ((pci.VendorID==0x12c5) && (pci.DeviceID==0x007f)) {
	    xsp->dev_ops = &ise_operations;
	    printk("ise%u: Detected %s board\n",
		   xsp->id, xsp->dev_ops->full_name);

	    { IO_ERROR_LOG_PACKET*event;
	      unsigned psize = sizeof(IO_ERROR_LOG_PACKET);
	      event = IoAllocateErrorLogEntry(fdo, (UCHAR)psize);
	      event->ErrorCode = ISE_DETECTED_ISE;
	      event->UniqueErrorValue = 0;
	      event->FinalStatus = STATUS_SUCCESS;
	      event->MajorFunctionCode = 0;
	      event->IoControlCode = 0;
	      event->DumpData[0] = 0;
	      IoWriteErrorLogEntry(event);
	    }

      } else if ((pci.VendorID=0x8086) && (pci.DeviceID==0xb555)) {
	    xsp->dev_ops = &jse_operations;
	    printk("ise%u: Detected %s board\n",
		   xsp->id, xsp->dev_ops->full_name);

	    { IO_ERROR_LOG_PACKET*event;
	      unsigned psize = sizeof(IO_ERROR_LOG_PACKET);
	      event = IoAllocateErrorLogEntry(fdo, (UCHAR)psize);
	      event->ErrorCode = ISE_DETECTED_JSE;
	      event->UniqueErrorValue = 0;
	      event->FinalStatus = STATUS_SUCCESS;
	      event->MajorFunctionCode = 0;
	      event->IoControlCode = 0;
	      event->DumpData[0] = 0;
	      IoWriteErrorLogEntry(event);
	    }

      } else if (pci.DeviceID==0x0091) {
	      /* Windows Vista does not return the correct Vendor ID
		 for this device. Possibly, it is confused by the
		 sequence of bridges between here and the
		 device. Paradoxically, is *does* get the device id
		 right, so we can use that to detect that we have an
		 EJSE. The really spectacular paradox about all this,
		 though, is that windows DOES get the correct
		 device/vendor when it probes the device to bind the
		 driver. Since 0x0091 is the only device ID in the inf
		 file for this driver, this shortened test is
		 adequate. *Sheesh* */
	    xsp->dev_ops = &ejse_operations;
	    printk("ise%u: Detected %s board\n",
		   xsp->id, xsp->dev_ops->full_name);

	    { IO_ERROR_LOG_PACKET*event;
	      unsigned psize = sizeof(IO_ERROR_LOG_PACKET) + 2*sizeof(ULONG);
	      event = IoAllocateErrorLogEntry(fdo, (UCHAR)psize);
	      event->ErrorCode = ISE_DETECTED_EJSE;
	      event->UniqueErrorValue = 0;
	      event->FinalStatus = STATUS_SUCCESS;
	      event->MajorFunctionCode = 0;
	      event->IoControlCode = 0;
	      event->DumpDataSize = 2 * sizeof(ULONG);
	      event->DumpData[0] = pci.VendorID;
	      event->DumpData[1] = pci.DeviceID;
	      IoWriteErrorLogEntry(event);
	    }

      } else {
	    IO_ERROR_LOG_PACKET*event;
	    unsigned psize = sizeof(IO_ERROR_LOG_PACKET) + 3*sizeof(ULONG);
	    event = IoAllocateErrorLogEntry(fdo, (UCHAR)psize);
	    event->ErrorCode = ISE_NO_DEVICE;
	    event->UniqueErrorValue = (pci.DeviceID << 16) + pci.VendorID;
	    event->FinalStatus = STATUS_SUCCESS;
	    event->MajorFunctionCode = 0;
	    event->IoControlCode = 0;
	    event->DumpDataSize = 3 * sizeof(ULONG);
	    event->DumpData[0] = pci.VendorID;
	    event->DumpData[1] = pci.DeviceID;
	    event->DumpData[2] = 0x55555555;
	    IoWriteErrorLogEntry(event);

	    printk("ise%u: Unknown device type?!\n", xsp->id);
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

	/* Scan the translated resources, looking for the bits that I
	   need to manipulate the board. */

      for (rdx = 0 ;  rdx < res->Count ;  rdx += 1) {
	    unsigned idx;
	    unsigned need_bar = 0;
	    CM_FULL_RESOURCE_DESCRIPTOR*frd = res->List+rdx;


	    for (idx = 0 ;  idx < frd->PartialResourceList.Count ;  idx += 1) {
		  CM_PARTIAL_RESOURCE_DESCRIPTOR*prd;
		  prd = frd->PartialResourceList.PartialDescriptors + idx;
		  switch (prd->Type) {

		      case CmResourceTypePort:
			printk("ise%u: IO port.\n", xsp->id);
			break;

		      case CmResourceTypeMemory:
			printk("ise%u: memory at %x:%x (%u bytes)\n",
			       xsp->id, prd->u.Memory.Start.HighPart,
			       prd->u.Memory.Start.LowPart,
			       prd->u.Memory.Length);
			xsp->bar0_size = prd->u.Memory.Length;
			xsp->bar0 = MmMapIoSpace(prd->u.Memory.Start,
						 xsp->bar0_size,
						 MmNonCached);
			break;

		      case CmResourceTypeInterrupt:
			xsp->irql = (KIRQL)prd->u.Interrupt.Level;
			xsp->ivec = prd->u.Interrupt.Vector;
			xsp->affinity = prd->u.Interrupt.Affinity;
			printk("ise%u: Interrupt vector=%u.\n",
			       xsp->id, xsp->ivec);
			break;

		      default:
			printk("ise%u: resource[%u].Type == %u\n",
				 xsp->id, idx, prd->Type);
			break;
		  }
	    }
      }

	/* Connect the interrupt to the interrupt handler. */
      status = IoConnectInterrupt(&xsp->irq, xxisr, fdo, 0, xsp->ivec,
				  xsp->irql, xsp->irql, LevelSensitive,
				  TRUE, xsp->affinity, FALSE);

      if (!NT_SUCCESS(status)) {
	    switch (status) {
		case STATUS_INSUFFICIENT_RESOURCES:
		  printk("ise%u: IoConnectInterrupt reported "
			 "insufficient resources!\n", xsp->id);
		  break;
		default:
		  printk("ise%u: IoConnectInterrupt returned 0x%x\n",
			 xsp->id, status);
		  break;
	    }
	    xsp->irq = 0;
	    return status;
      }

      KeInitializeDpc(&xsp->root_to_board_dpc, &root_to_board_dpc, xsp);
      KeInitializeDpc(&xsp->root_timer_dpc,    &root_timer_dpc,    xsp);
      KeInitializeDpc(&xsp->pending_read_dpc,  &pending_read_dpc,  xsp);
      KeInitializeDpc(&xsp->pending_write_dpc, &pending_write_dpc, xsp);
      KeInitializeTimer(&xsp->root_timer);

      InitializeListHead(&xsp->pending_read_irps);
      InitializeListHead(&xsp->pending_write_irps);

      KeInitializeSpinLock(&xsp->pending_read_sync);
      KeInitializeSpinLock(&xsp->pending_write_sync);

      KeInitializeSpinLock(&xsp->mutex);

	/* Create a DMA_ADAPTER object for use while allocating
	   buffers. This adapter is for allocating the commin buffer
	   tables for communicating with the JSE. These buffers must
	   *not* be in 64bit space, and are not for the frame pages. */
      { DEVICE_DESCRIPTION desc;
        unsigned long nmap = 131072;
	int idx;
	KIRQL save_irql;

	RtlFillMemory(&desc, sizeof desc, 0);
	desc.Version = DEVICE_DESCRIPTION_VERSION;
        desc.Master = TRUE;
	desc.ScatterGather = TRUE;
	desc.Dma32BitAddresses = TRUE;
	desc.Dma64BitAddresses = FALSE;
	desc.InterfaceType = PCIBus;
	desc.MaximumLength = 0x7fffffffUL;

	xsp->dma = IoGetDmaAdapter(xsp->pdo, &desc, &nmap);
	if (xsp->dma == 0) {
	      IO_ERROR_LOG_PACKET*event;
	      unsigned psize = sizeof(IO_ERROR_LOG_PACKET) + 2*sizeof(ULONG);
	      event = IoAllocateErrorLogEntry(xsp->fdo, (UCHAR)psize);
	      event->ErrorCode = ISE_GET_ADAPTER_FAILED;
	      event->UniqueErrorValue = 0;
	      event->FinalStatus = STATUS_UNSUCCESSFUL;
	      event->MajorFunctionCode = 0;
	      event->IoControlCode = 0;
	      event->DumpDataSize = 2*sizeof(ULONG);
	      event->DumpData[0] = xsp->id;
	      event->DumpData[1] = 16;
	      IoWriteErrorLogEntry(event);
	      printk("ise%u: IoGetDmaAdapter failed!\n", xsp->id);
	      return STATUS_UNSUCCESSFUL;
	}

	printk("ise%u: IoGetDmaAdapter nmap=%u\n", xsp->id, nmap);

	  /* AllocateAdapterChannel requires IRQL==DISPATCH_LEVEL. We
	     know by context that we are called from a PNP dispatch
	     routine and are therefore PASSIVE_LEVEL. So RAISE the
	     irql to get the adapter channel, and lower it when done. */
	KeRaiseIrql(DISPATCH_LEVEL, &save_irql);
	xsp->dma->DmaOperations->AllocateAdapterChannel(xsp->dma,
							xsp->pdo,
							nmap,
							dma_adapter_stub,
							xsp);
	KeLowerIrql(save_irql);

	  /* Allocate DMA adapters for all the possible frames as
	     well. Logically, the ISE has a dma adapter for each frame
	     that can be mapped. */
	for (idx = 0 ;  idx < 16 ;  idx += 1) {
	      nmap = 131072;
	      RtlFillMemory(&desc, sizeof desc, 0);
	      desc.Version = DEVICE_DESCRIPTION_VERSION;
	      desc.Master = TRUE;
	      desc.ScatterGather = TRUE;
	      desc.Dma32BitAddresses = TRUE;
	      desc.Dma64BitAddresses = TRUE;
	      desc.InterfaceType = PCIBus;
	      desc.MaximumLength = 0x7fffffffUL;

	      xsp->frame_dma[idx] = IoGetDmaAdapter(xsp->pdo, &desc, &nmap);
	      if (xsp->frame_dma[idx] == 0) {
		    IO_ERROR_LOG_PACKET*event;
		    unsigned psize = sizeof(IO_ERROR_LOG_PACKET) + 2*sizeof(ULONG);
		    event = IoAllocateErrorLogEntry(xsp->fdo, (UCHAR)psize);
		    event->ErrorCode = ISE_GET_ADAPTER_FAILED;
		    event->UniqueErrorValue = 0;
		    event->FinalStatus = STATUS_UNSUCCESSFUL;
		    event->MajorFunctionCode = 0;
		    event->IoControlCode = 0;
		    event->DumpDataSize = 2*sizeof(ULONG);
		    event->DumpData[0] = xsp->id;
		    event->DumpData[1] = idx;
		    IoWriteErrorLogEntry(event);
		    printk("ise%u: IoGetDmaAdapter [%d] failed!\n",
			   xsp->id, idx);
		    return STATUS_UNSUCCESSFUL;
	      }

	      KeRaiseIrql(DISPATCH_LEVEL, &save_irql);
	      xsp->dma->DmaOperations->AllocateAdapterChannel(xsp->frame_dma[idx],
							      xsp->pdo,
							      nmap,
							      dma_adapter_stub,
							      xsp);
	      KeLowerIrql(save_irql);

		/* Clear other frame related members. */
	      xsp->frame_tab[idx] = 0;
	      xsp->frame_mdl_irp[idx] = 0;
	      xsp->frame_wait_irp[idx] = 0;
	}

	xsp->frame_done_flags = 0;
	xsp->frame_cleanup_mask = 0;
	xsp->frame_cleanup_work = 0;
      }
#if 0
	/* Create and initialize an initial root table for the
	   device. Do not send it to the board, though, because the
	   board only needs to wake up when there is at least one
	   channel open to it. */
      xsp->root = (struct root_table*)
	    xsp->dma->DmaOperations->AllocateCommonBuffer(xsp->dma,
					   sizeof(struct root_table),
					   &xsp->rootl, FALSE);
      if (xsp->root == 0) {
	    IO_ERROR_LOG_PACKET*event;
	    unsigned psize = sizeof(IO_ERROR_LOG_PACKET) + 2*sizeof(ULONG);
	    event = IoAllocateErrorLogEntry(xsp->fdo, (UCHAR)psize);
	    event->ErrorCode = ISE_GET_ROOT_TABLE_FAILED;
	    event->UniqueErrorValue = 0;
	    event->FinalStatus = STATUS_INSUFFICIENT_RESOURCES;
	    event->MajorFunctionCode = 0;
	    event->IoControlCode = 0;
	    event->DumpDataSize = 2*sizeof(ULONG);
	    event->DumpData[0] = xsp->id;
	    event->DumpData[1] = sizeof(struct root_table);
	    IoWriteErrorLogEntry(event);
	    printk("ise%u: Unable to allocate root table\n", xsp->id);
	    return STATUS_INSUFFICIENT_RESOURCES;
      }

      RtlZeroMemory(xsp->root, sizeof(*xsp->root));
      xsp->root->magic = ROOT_TABLE_MAGIC;
      xsp->root->self = xsp->rootl.LowPart;
#else
      xsp->root = 0;
#endif
      { int idx;
	for (idx = 0 ; idx < ROOT_STANDBY_TABLE_MAX ; idx += 1)
	      xsp->root_standby_table[idx].root = 0;
      }
      xsp->root_standby_leak = 0;

      dev_clear_hardware(xsp);


	/* All done. Return success. */
      irp->IoStatus.Status = STATUS_SUCCESS;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return STATUS_SUCCESS;
}

/*
 * This function is the opposite of the pnp_start_ise function, in
 * that it detaches the device from the physical resources that are
 * attached to the board. This may be called from STOP_DEVICE or from
 * REMOVE_DEVICE, before it calls remove_ise below. All this does is
 * stop and detach the ISE board. The fdo remain intact, to be deleted
 * later, and the IRP is not completed here.
 *
 * WDM assures us that the driver is not open anywhere, so there are
 * certainly no open channels.
 */
void pnp_stop_ise(DEVICE_OBJECT*fdo)
{
      int idx;
      KIRQL save_irql;
      struct instance_t*xsp = (struct instance_t*)fdo->DeviceExtension;

      printk("ise%u: pnp_stop_ise\n", xsp->id);

      cleanup_channel_standby_list(xsp);

	/* Release the root table. */
      if (xsp->root) {
	    xsp->dma->DmaOperations->FreeCommonBuffer(xsp->dma,
						      sizeof(*xsp->root),
						      xsp->rootl,
						      xsp->root,
						      FALSE);
	    xsp->root = 0;
      }

      for (idx = 0 ; idx < ROOT_STANDBY_TABLE_MAX ; idx += 1) {
	    struct root_table*root = xsp->root_standby_table[idx].root;
	    PHYSICAL_ADDRESS rootl = xsp->root_standby_table[idx].rootl;
	    if (root != 0) {
		  xsp->dma->DmaOperations->FreeCommonBuffer(xsp->dma,
					  sizeof(*root), rootl, root, FALSE);
	    }
      }

	/* Make sure the hardware will not interrupt the host. Only do
	   this if we actually cot device registers. */
      if (xsp->bar0_size)
	    dev_init_hardware(xsp);

      KeRaiseIrql(DISPATCH_LEVEL, &save_irql);

	/* Free the adapters for the frames. */
      for (idx = 0 ;  idx < 16 ;  idx += 1) {
	    if (xsp->frame_dma[idx] == 0)
		  continue;

	    xsp->frame_dma[idx]->DmaOperations->FreeAdapterChannel(xsp->frame_dma[idx]);
	    xsp->frame_dma[idx] = 0;
      }

	/* Release the DmaOperations object. */
      if (xsp->dma) {
	    xsp->dma->DmaOperations->FreeAdapterChannel(xsp->dma);
	    xsp->dma->DmaOperations->PutDmaAdapter(xsp->dma);
	    xsp->dma = 0;
      }

      KeLowerIrql(save_irql);

	/* Disconnect the interrupt */
      if (xsp->irq != 0) {
	    IoDisconnectInterrupt(xsp->irq);
	    xsp->irq = 0;
      }

	/* Unmap the memory region for the device. */
      if (xsp->bar0_size) {
	    MmUnmapIoSpace(xsp->bar0, xsp->bar0_size);
	    xsp->bar0 = 0;
	    xsp->bar0_size = 0;
      }

}

/*
 * This function creates a new device object for an ISE device that
 * AddDevice discovered. I create here the device node links that the
 * user mode uses to get a hold of me as well.
 */
DEVICE_OBJECT*create_ise(DRIVER_OBJECT*drv, unsigned dev_no)
{
      NTSTATUS status;
      wchar_t dev_buf[sizeof(devname)/sizeof(devname[0]) + 4];
      wchar_t dos_buf[sizeof(dosname)/sizeof(dosname[0]) + 4];
      wchar_t tmp_buf[8];
      UNICODE_STRING dev_str, dos_str, tmp_str;

      DEVICE_OBJECT*fdo;

      dev_str.Buffer = dev_buf;
      dev_str.MaximumLength = sizeof(dev_buf);
      dev_str.Length = 0;

      RtlAppendUnicodeToString(&dev_str, (wchar_t*)devname);

      tmp_str.Buffer = tmp_buf;
      tmp_str.MaximumLength = sizeof(tmp_buf);
      tmp_str.Length = 0;
      RtlIntegerToUnicodeString(dev_no, 10, &tmp_str);

      RtlAppendUnicodeStringToString(&dev_str, &tmp_str);

      status = IoCreateDevice(drv, sizeof(struct instance_t), &dev_str,
			      FILE_DEVICE_ISE, 0, FALSE, &fdo);

      if (!NT_SUCCESS(status))
	    return 0;

      RtlZeroMemory(fdo->DeviceExtension, sizeof(struct instance_t));

      dos_str.Buffer = dos_buf;
      dos_str.MaximumLength = sizeof(dos_buf);
      dos_str.Length = 0;

      RtlAppendUnicodeToString(&dos_str, (wchar_t*)dosname);

      tmp_str.Buffer = tmp_buf;
      tmp_str.MaximumLength = sizeof(tmp_buf);
      tmp_str.Length = 0;
      RtlIntegerToUnicodeString(dev_no, 10, &tmp_str);

      RtlAppendUnicodeStringToString(&dos_str, &tmp_str);

      IoCreateSymbolicLink(&dos_str, &dev_str);

      fdo->Flags |= DO_BUFFERED_IO;
      fdo->Flags &= ~DO_DEVICE_INITIALIZING;

      return fdo;
}

/*
 * This is roughly the opposite of the create_device function. It even
 * deletes the device, so the caller should unlink it from  device
 * stacks first. Note also that since the create_device only creates
 * the windows objects for a device, this only removes those
 * objects. The pnp_remove_ise function calls the pnp_stop_ise
 * function above to detach the hardware.
 */
void remove_ise(DEVICE_OBJECT*fdo)
{
      struct instance_t*xsp = (struct instance_t*)fdo->DeviceExtension;

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

      IoDeleteDevice(fdo);
}


/*
 * $Log$
 * Revision 1.22  2009/04/03 18:21:17  steve
 *  Implement frame64 support in Windows driver.
 *  More robust error handling around root tables.
 *  Keep a deeper root standby list to prevent leaks.
 *
 * Revision 1.21  2008/12/06 03:27:08  steve
 *  Add EJSE support.
 *
 * Revision 1.20  2005/09/12 21:52:11  steve
 *  Get IRQL for AllocateAdapterChannel right.
 *
 * Revision 1.19  2005/07/26 01:17:32  steve
 *  New method of mapping frames for version 2.5
 *
 * Revision 1.18  2005/04/30 03:00:43  steve
 *  Put timeout on root-to-board operations.
 *
 * Revision 1.17  2005/03/02 15:25:43  steve
 *  Better job of poisoning old roots, and activating new ones.
 *
 * Revision 1.16  2004/10/25 23:27:10  steve
 *  Fix close to detach when the last channel is closed.
 *
 * Revision 1.15  2004/10/25 19:04:49  steve
 *  Snapshot 20041005: Some more error logging.
 *
 * Revision 1.14  2004/07/15 04:19:26  steve
 *  Extend to support JSE boards.
 *
 * Revision 1.13  2002/06/14 16:09:29  steve
 *  spin locks around root table manipulations.
 *
 * Revision 1.12  2002/05/13 20:07:52  steve
 *  More diagnostic detail, and check registers.
 *
 * Revision 1.11  2002/04/11 00:49:30  steve
 *  Move FreeCommonBuffers to PASSIVE_MODE using standby lists.
 *
 * Revision 1.10  2002/04/10 23:20:27  steve
 *  Do not touch IRP after it is completed.
 *
 * Revision 1.9  2002/04/10 21:05:05  steve
 *  Handling failure to get bars.
 *
 * Revision 1.8  2001/10/01 22:48:20  steve
 *  Cancel timers in cancel routines.
 *
 * Revision 1.7  2001/09/28 18:09:53  steve
 *  Create a per-device mutex to manage multi-processor access
 *  to the instance object.
 *
 *  Fix some problems with timeout handling.
 *
 *  Add some diagnostic features for tracking down locking
 *  or delay problems.
 *
 * Revision 1.6  2001/09/07 03:00:32  steve
 *  The ise device is not exclusive, so far as Windows is concerned.
 *
 * Revision 1.5  2001/09/06 18:28:43  steve
 *  Read timeouts.
 *
 * Revision 1.4  2001/09/05 22:05:55  steve
 *  protect mappings from misused or forgotten unmaps.
 *
 * Revision 1.3  2001/08/14 22:25:30  steve
 *  Add SseBase device
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

