/*
 * Copyright (c) 2001 Picture Elements, Inc.
 *    Stephen Williams (steve@picturel.com)
 *
 * $Id$
 */

# include  "ise_sys.h"


static NTSTATUS read_complete(DEVICE_OBJECT*dev, IRP*req, void*ctx);
static NTSTATUS write_complete(DEVICE_OBJECT*dev, IRP*req, void*ctx);

/*
 * The ISE board bootprom uses the BIST bit to tell the host that the
 * application program is loaded and ready to go. If the BIST bit is
 * set, then start the BIST to cause the application to go.
 */
NTSTATUS isex_run_program(DEVICE_OBJECT*dev, IRP*irp)
{
      IRP*req;
      IO_STACK_LOCATION*req_stp;

      struct instance_t*xsp = *((struct instance_t**)dev->DeviceExtension);
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);

	/* Cannot start a program if there are any channels open. */
      if (xsp->channels != 0) {
	    irp->IoStatus.Status = STATUS_DEVICE_ALREADY_ATTACHED;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return irp->IoStatus.Status;
      }

      if (debug_flag&UCR_TRACE_UCRX)
	    printk("isex%u: run ise%u\n", xsp->id, xsp->id);

      IoMarkIrpPending(irp);

	/* Stash the instance pointer in the IRP for future use. */
      stp->Parameters.DeviceIoControl.Type3InputBuffer = xsp;

	/* Allocate an IRP to read the BIST register for the device
	   configuration space. Set the completion function and return
	   STATUS_PENDING. */
      req = IoAllocateIrp(xsp->next_dev->StackSize, FALSE);
      req_stp = IoGetCurrentIrpStackLocation(req);

      req_stp->MajorFunction = IRP_MJ_PNP;
      req_stp->MinorFunction = IRP_MN_READ_CONFIG;
      req_stp->Parameters.ReadWriteConfig.Buffer = xsp->config_buf;
      req_stp->Parameters.ReadWriteConfig.Offset = 0x0c;
      req_stp->Parameters.ReadWriteConfig.Length = 4;
      req->IoStatus.Status = STATUS_NOT_SUPPORTED;

      IoSetCompletionRoutine(req, read_complete, irp, TRUE, TRUE, TRUE);
      IoCallDriver(xsp->next_dev, req);

      return STATUS_PENDING;
}

/*
 * This function is called in response to a completed read from the
 * PCI device configuration space. Check that the BIST bit is on, then
 * start a write to start the BIST. This tells the ISE board to start
 * running the loaded program.
 */
static NTSTATUS read_complete(DEVICE_OBJECT*dev, IRP*req, void*ctx)
{
      IO_STACK_LOCATION*req_stp;
      IRP*irp = (IRP*)ctx;
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct instance_t*xsp = (struct instance_t*)
	    stp->Parameters.DeviceIoControl.Type3InputBuffer;

      IoFreeIrp(req);

      if (debug_flag&UCR_TRACE_UCRX)
	    printk("isex%u: read config bits: %x:%x:%x:%x\n", xsp->id,
		   xsp->config_buf[0], xsp->config_buf[1],
		   xsp->config_buf[2], xsp->config_buf[3]);

      if (! (xsp->config_buf[3] & 0x80)) {
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);


	    return STATUS_MORE_PROCESSING_REQUIRED;
      }

      xsp->config_buf[3] |= 0x40;

      req = IoAllocateIrp(xsp->next_dev->StackSize, FALSE);

      req_stp = IoGetCurrentIrpStackLocation(req);
      req_stp->MajorFunction = IRP_MJ_PNP;
      req_stp->MinorFunction = IRP_MN_WRITE_CONFIG;
      req_stp->Parameters.ReadWriteConfig.Buffer = xsp->config_buf;
      req_stp->Parameters.ReadWriteConfig.Offset = 0x0c;
      req_stp->Parameters.ReadWriteConfig.Length = 4;
      req->IoStatus.Status = STATUS_NOT_SUPPORTED;

      IoSetCompletionRoutine(req, write_complete, irp, TRUE, TRUE, TRUE);
      IoCallDriver(xsp->next_dev, req);

      return STATUS_MORE_PROCESSING_REQUIRED;
}

/*
 * This is the response that the configuration write has
 * completed. Complete the ioctl IRP.
 */
static NTSTATUS write_complete(DEVICE_OBJECT*dev, IRP*req, void*ctx)
{
      IRP*irp = (IRP*)ctx;
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct instance_t*xsp = (struct instance_t*)
	    stp->Parameters.DeviceIoControl.Type3InputBuffer;


      irp->IoStatus.Status = STATUS_SUCCESS;
      irp->IoStatus.Information = 0;
      IoCompleteRequest(irp, IO_NO_INCREMENT);

      IoFreeIrp(req);

      return STATUS_MORE_PROCESSING_REQUIRED;
}

/*
 * $Log$
 * Revision 1.1  2001/07/30 21:32:43  steve
 *  Rearrange the status path to follow the return codes of
 *  the callbacks, and preliminary implementation of the
 *  RUN_PROGRAM ioctl.
 *
 */

