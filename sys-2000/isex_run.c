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
 * The ISE board bootprom uses a status register bit to tell me that
 * the device is ready to run the loaded program.
 */
NTSTATUS isex_run_program(DEVICE_OBJECT*dev, IRP*irp)
{
      IRP*req;
      IO_STACK_LOCATION*req_stp;
      __u32 state;

      struct instance_t*xsp = *((struct instance_t**)dev->DeviceExtension);
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);

	/* Cannot start a program if there are any channels open. */
      if (xsp->channels != 0) {
	    irp->IoStatus.Status = STATUS_DEVICE_ALREADY_ATTACHED;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_DEVICE_ALREADY_ATTACHED;
      }

      state = dev_get_status_resp(xsp);

      switch (state & 3) {

	  case 0:
	  case 2:
	    printk("isex%u: bootprom not active (or obsolete)\n", xsp->id);
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;

	  case 1:
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;

	  default:
	    break;
      }

	/* At this point we know that the status bits are 3:

	   bit0==1 is the bootprom active bit, and
	   bit1==1 is the program ready bit.

	   Respond by setting the run program bit, and the board
	   should run the program. */
      dev_set_status_value(xsp, 2);
      dev_set_bells(xsp, STATUS_BELLMASK);

      if (debug_flag&UCR_TRACE_UCRX)
	    printk("isex%u: run ise%u\n", xsp->id, xsp->id);

      irp->IoStatus.Status = STATUS_SUCCESS;
      irp->IoStatus.Information = 0;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return STATUS_SUCCESS;
}

/*
 * $Log$
 * Revision 1.3  2002/04/10 23:20:27  steve
 *  Do not touch IRP after it is completed.
 *
 * Revision 1.2  2001/08/03 17:39:41  steve
 *  Use status method to run programs.
 *
 * Revision 1.1  2001/07/30 21:32:43  steve
 *  Rearrange the status path to follow the return codes of
 *  the callbacks, and preliminary implementation of the
 *  RUN_PROGRAM ioctl.
 *
 */

