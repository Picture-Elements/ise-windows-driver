/*
 * Copyright (c) 2001 Picture Elements, Inc.
 *    Stephen Williams (steve@picturel.com)
 *
 * $Id$
 */

# include  "ise_sys.h"

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
 * Make a copy of the current root table, with the magic numbers
 * twisted so that this is a valid table. The caller can then do minor
 * modifications to this copy before installing it.
 */
struct root_table*duplicate_root(struct instance_t*xsp, PHYSICAL_ADDRESS*ptrl)
{
      KIRQL save_irql;
      struct root_table*newroot;

      KeAcquireSpinLock(&xsp->mutex, &save_irql);
      if (xsp->root_standby) {
	    newroot = xsp->root_standby;
	    *ptrl = xsp->rootl_standby;
	    xsp->root_standby = 0;
	    KeReleaseSpinLock(&xsp->mutex, save_irql);

      } else {
	    KeReleaseSpinLock(&xsp->mutex, save_irql);
	    newroot = (struct root_table*)
		  xsp->dma->DmaOperations->AllocateCommonBuffer(xsp->dma,
				     sizeof(struct root_table), ptrl, FALSE);

	    if (newroot == 0)
		  return 0;
      }

      RtlCopyMemory(newroot, xsp->root, sizeof (*newroot));
      newroot->self = ptrl->LowPart;

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
		   callback_t fun)
{
      KIRQL save_irql;
      unsigned long mask;

      KeAcquireSpinLock(&xsp->mutex, &save_irql);
      mask = dev_mask_irqs(xsp);

      if (xsp->root_callback != 0) {
	    printk("ise%u: warning: root_callback overrun.\n", xsp->id);
      }

	/* Stash the pointers, the IRP and the callback into a standby
	   area and mark the IRP pending. */
      xsp->root2 = root;
      xsp->rootl2 = rootl;
      xsp->root_irp = irp;
      xsp->root_callback = fun;
      IoMarkIrpPending(irp);
      irp->IoStatus.Status = STATUS_PENDING;


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

static void root_to_board_dpc(KDPC*dpc, void*ctx, void*arg1, void*arg2)
{
      struct instance_t*xsp = (struct instance_t*)ctx;
      IRP*irp;

      KeAcquireSpinLockAtDpcLevel(&xsp->mutex);

      irp = xsp->root_irp;

	/* Detect possible spurious DPC calls. Maybe there is no IRP
	   waiting, or maybe the ISE board is not yet finished the
	   change. */
      if (irp == 0) {
	    KeReleaseSpinLockFromDpcLevel(&xsp->mutex);
	    return;
      }

      if (dev_get_root_table_resp(xsp) != (xsp->root2? xsp->root2->self : 0)) {
	    printk("ise%u: warning: root not received yet: "
		   "resp=%x, self=%x.\n", xsp->id,
		   dev_get_root_table_resp(xsp),
		   (xsp->root2? xsp->root2->self : 0));
	    KeReleaseSpinLockFromDpcLevel(&xsp->mutex);
	    return;
      }

      xsp->root_irp = 0;

      if (xsp->root2 == 0)
	    goto complete;

	/* Poison then release the old root table, and set the
	   pointers to point to the new root table. Save the existing
	   root for possible reuse later. */
      xsp->root->magic = 0x11111111;
      if (xsp->root_standby)
	    xsp->root_standby_leak += 1;
      xsp->root_standby = xsp->root;
      xsp->rootl_standby = xsp->rootl;

      xsp->root  = xsp->root2;
      xsp->rootl = xsp->rootl2;

      xsp->root2 = 0;

 complete:
      { callback_t callback = xsp->root_callback;
        xsp->root_callback = 0;
	KeReleaseSpinLockFromDpcLevel(&xsp->mutex);
	(*callback)(xsp, irp);
      }
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
      xpd->proc = IoGetCurrentProcess();

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
        newroot->chan[0].magic = xpd->table->magic;
	newroot->chan[0].ptr   = xpd->table->self;

	rc = root_to_board(xsp, irp, newroot, newrootl, &complete_success);
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
      struct root_table*newroot = duplicate_root(xsp, &newrootl);

      if (xpd->proc != IoGetCurrentProcess()) {
	    printk("ise%u.%u: close by wrong owner?!\n",
		   xsp->id, xpd->channel);
      }

	/* This shouldn't be necessary, but is defensive. */
      KeCancelTimer(&xpd->read_timer);

	/* If this is the last channel for this process, then remove
	   any frame mappings first. */
      { unsigned count = 0, fidx;
        struct channel_t*cur;
	for (cur = xpd->next ;  cur != xpd ;  cur = cur->next) {
	      if (cur->proc == xpd->proc)
		    count += 1;
	}

	if (count == 0) for (fidx = 0 ;  fidx < 16 ;  fidx += 1) {

	      if (xsp->frame_map[fidx].base
		  && (xsp->frame_map[fidx].proc == xpd->proc)) {
		    printk("ise%u.%u: unmap frame %u on final close.\n",
			   xsp->id, xpd->channel, fidx);

		    MmUnmapLockedPages(xsp->frame_map[fidx].base,
				       xsp->frame_mdl[fidx]);

		    xsp->frame_map[fidx].proc = 0;
		    xsp->frame_map[fidx].base = 0;
	      }
	}
      }

      cleanup_channel_standby_list(xsp);

	/* Now start detaching the channel from the board. */
      newroot->chan[xpd->channel].magic = 0;
      newroot->chan[xpd->channel].ptr = 0;

      { NTSTATUS rc;
        rc = root_to_board(xsp, irp, newroot, newrootl, &dev_close_2);
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

      if (xpd->table) {
	      /* Release the table itself. Poison the memory first. */
	    KeAcquireSpinLock(&xsp->mutex, &save_irql);
	    xpd->table->self = 0;
	    xpd->table->magic = 0x11111111;
	    xpd->next = xsp->channel_standby_list;
	    xsp->channel_standby_list = xpd;
	    KeReleaseSpinLock(&xsp->mutex, save_irql);

      } else {
	      /* All done, release the channel_t object. */
	    ExFreePool(xpd);
      }

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
      struct instance_t*xsp = (struct instance_t*)fdo->DeviceExtension;

      printk("ise%u: pnp_start_ise\n", xsp->id);

      iop = IoGetCurrentIrpStackLocation(irp);
      raw = iop->Parameters.StartDevice.AllocatedResources;
      res = iop->Parameters.StartDevice.AllocatedResourcesTranslated;

      if (res == 0) {
	    printk("ise%u: no resources?!\n", xsp->id);
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
      KeInitializeDpc(&xsp->pending_read_dpc,  &pending_read_dpc,  xsp);
      KeInitializeDpc(&xsp->pending_write_dpc, &pending_write_dpc, xsp);

      InitializeListHead(&xsp->pending_read_irps);
      InitializeListHead(&xsp->pending_write_irps);

      KeInitializeSpinLock(&xsp->pending_read_sync);
      KeInitializeSpinLock(&xsp->pending_write_sync);

      KeInitializeSpinLock(&xsp->mutex);

	/* Create a DMA_ADAPTER object for use while allocating buffers. */
      { DEVICE_DESCRIPTION desc;
        unsigned long nmap = 0;

	RtlFillMemory(&desc, sizeof desc, 0);
	desc.Version = DEVICE_DESCRIPTION_VERSION;
        desc.Master = TRUE;
	desc.ScatterGather = TRUE;
	desc.Dma32BitAddresses = TRUE;
	desc.InterfaceType = PCIBus;
	desc.MaximumLength = 0x0fffffffUL;

	xsp->dma = IoGetDmaAdapter(xsp->pdo, &desc, &nmap);
	if (xsp->dma == 0) {
	      printk("ise%u: IoGetDmaAdapter failed!\n", xsp->id);
	      return STATUS_UNSUCCESSFUL;
	}
      }

	/* Clear the frame members of the instance. */
      { unsigned idx;
        for (idx = 0 ;  idx < 16 ; idx += 1) {
	      xsp->frame_tab[idx] = 0;
	      xsp->frame_mdl[idx] = 0;
	      xsp->frame_pag[idx] = 0;
	      xsp->frame_map[idx].proc = 0;
	      xsp->frame_map[idx].base = 0;
	}
      }

	/* Create and initialize an initial root table for the
	   device. Do not send it to the board, though, because the
	   board only needs to wake up when there is at least one
	   channel open to it. */
      xsp->root = (struct root_table*)
	    xsp->dma->DmaOperations->AllocateCommonBuffer(xsp->dma,
					   sizeof(struct root_table),
					   &xsp->rootl, FALSE);
      if (xsp->root == 0) {
	    printk("ise%u: Unable to allocate root tabl\n", xsp->id);
	    return STATUS_INSUFFICIENT_RESOURCES;
      }

      RtlZeroMemory(xsp->root, sizeof(*xsp->root));
      xsp->root->magic = ROOT_TABLE_MAGIC;
      xsp->root->self = xsp->rootl.LowPart;

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

      if (xsp->root_standby) {
	    xsp->dma->DmaOperations->FreeCommonBuffer(xsp->dma,
						  sizeof(*xsp->root_standby),
						  xsp->rootl_standby,
						  xsp->root_standby,
						  FALSE);
	    xsp->root_standby = 0;
      }
	/* Make sure the hardware will not interrupt the host. Only do
	   this if we actually cot device registers. */
      if (xsp->bar0_size)
	    dev_init_hardware(xsp);

	/* Release the DmaOperations object. */
      if (xsp->dma) {
	    xsp->dma->DmaOperations->PutDmaAdapter(xsp->dma);
	    xsp->dma = 0;
      }

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

