/*
 * Copyright (c) 2001 Picture Elements, Inc.
 *    Stephen Williams (steve@picturel.com)
 *
 * $Id$
 */

# include  "ise_sys.h"
# include  "ucrif.h"

/*
 * The ISEX device nodes are an alternative interface into the device
 * board that is used for control access. It is used for such tasks as
 * resetting the board or pressing diagnostic buttons. A device object
 * is created along with each real fdo.
 */

static const wchar_t devname[] = L"\\Device\\isex";
static const wchar_t dosname[] = L"\\DosDevices\\ISEX";


static NTSTATUS ucrx_restart_board(DEVICE_OBJECT*dev, IRP*irp)
{
      struct instance_t*xsp = *((struct instance_t**)dev->DeviceExtension);

	/* Cannot restart the board if there are any channels open. */
      if (xsp->channels != 0) {
	    irp->IoStatus.Status = STATUS_DEVICE_ALREADY_ATTACHED;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return irp->IoStatus.Status;
      }

      if (debug_flag&UCR_TRACE_UCRX)
	    printk("ucrx%u: restart ise%u\n", xsp->id, xsp->id);

	/* Clear the table pointers and disable interrupts. */
      dev_init_hardware(xsp);
      dev_mask_irqs(xsp);

	/* restart doorbell to the processor. Code on the processor
	   should notice this interrupt and restart itself. */
      dev_set_bells(xsp, 0x40000000);


	/* Free all the frames that this board may have had. This is
	   safe to do because there are no longer any pointers to the
	   table, and the processor has been rebooted. */
      { unsigned idx;
        for (idx = 0 ;  idx < 16 ;  idx += 1)
	      ise_free_frame(xsp, idx);
      }


	/* Make sure the table pointers are still clear, and re-enable
	   the interrupts. */
      dev_init_hardware(xsp);

      irp->IoStatus.Status = STATUS_SUCCESS;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return irp->IoStatus.Status;
}


static NTSTATUS isex_diagnose(DEVICE_OBJECT*dev, IRP*irp)
{
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

	  case 1:
	    printk("ise%u: %u root table pending\n", xsp->id,
		   xsp->root_irp? 1 : 0);

	    printk("ise%u: reads scheduled=%u, completed=%u, "
		   "cancelled=%u\n", xsp->id,
		   xsp->pending_read_count.scheduled,
		   xsp->pending_read_count.complete,
		   xsp->pending_read_count.cancelled);

	    printk("ise%u: root_table = (base=%x resp=%x), "
		   "IDR=%x, IIMR=%x, ODR=%x, OIMR=%x\n", xsp->id,
		   READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x10)),
		   READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x18)),
		   READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x20)),
		   READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x28)),
		   READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x2c)),
		   READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x34)));
		   
	    printk("ise%u: ROOT TABLE at %p(%x) "
		   "MAGIC=[%x:%x]\n", xsp->id, xsp->root,
		   xsp->root->self, xsp->root->magic,
		   xsp->root->self);

	    if (xsp->channels) {
		  struct channel_t*xpd = xsp->channels;
		  do {
			unsigned idx;
			printk("ise%u.%u: CHANNEL TABLE "
			       "MAGIC=[%x:%x]\n", xsp->id,
			       xpd->channel, xpd->table->magic,
			       xpd->table->self);

			printk("ise%u.%u: OUT "
			       "(first=%u, next=%u, off=%u) "
			       "IN (first=%u, next=%u)\n", xsp->id,
			       xpd->channel, xpd->table->first_out_idx,
			       xpd->table->next_out_idx, xpd->out_off,
			       xpd->table->first_in_idx,
			       xpd->table->next_in_idx);

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
	    break;

      }

      irp->IoStatus.Status = STATUS_SUCCESS;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return irp->IoStatus.Status;
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
	    return irp->IoStatus.Status;
      }

      
	/*  Invoke the read timeout explicitly for the selected
	    channel. The read_timeout function handles synchronization
	    with the reads and read cancels. */
      KeCancelTimer(&xpd->read_timer);
      read_timeout(0, xpd, 0, 0);


      irp->IoStatus.Status = STATUS_SUCCESS;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return irp->IoStatus.Status;
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
      return irp->IoStatus.Status;
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

	  case UCRX_SET_TRACE:
	    return isex_set_trace(dev, irp);

	  case UCRX_DIAGNOSE:
	    return isex_diagnose(dev, irp);

      }

      irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return irp->IoStatus.Status;
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

      if (status != STATUS_SUCCESS)
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
 * Revision 1.4  2001/09/06 18:28:43  steve
 *  Read timeouts.
 *
 * Revision 1.3  2001/08/14 22:25:30  steve
 *  Add SseBase device
 *
 * Revision 1.2  2001/07/30 21:32:43  steve
 *  Rearrange the status path to follow the return codes of
 *  the callbacks, and preliminary implementation of the
 *  RUN_PROGRAM ioctl.
 *
 * Revision 1.1  2001/07/26 00:31:30  steve
 *  Windows 2000 driver.
 *
 */

