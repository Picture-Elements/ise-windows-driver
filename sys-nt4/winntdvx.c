/*
 * Copyright (c) 1997 Picture Elements, Inc.
 *    Stephen Williams (steve@picturel.com)
 *
 *    This source code is free software; you can redistribute it
 *    and/or modify it in source code form under the terms of the GNU
 *    General Public License as published by the Free Software
 *    Foundation; either version 2 of the License, or (at your option)
 *    any later version. In order to redistribute the software in
 *    binary form, you will need a Picture Elements Binary Software
 *    License.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
 *  ---
 *    You should also have recieved a copy of the Picture Elements
 *    Binary Software License offer along with the source. This offer
 *    allows you to obtain the right to redistribute the software in
 *    binary (compiled) form. If you have not received it, contact
 *    Picture Elements, Inc., 777 Panoramic Way, Berkeley, CA 94704.
 */
#ifndef WINNT
#ident "$Id$"
#endif

#include  "ucrif.h"
#include  "os.h"
#include  "t-config.h"
#include  "ucrpriv.h"

/*
 * The devX device is a control device that allows such operations as
 * board reset. Each channel device has a matching devX device. This
 * device does not physically exist, it is a pseudo-device associated
 * with the seperate real device driver.
 */


void devx_diag1(struct Instance*xsp)
{
      printk(DEVICE_NAME "%u: NT ISE.SYS STATE...\n", xsp->number);
      printk(DEVICE_NAME "%u: READ COUNTS: total=%u, pending=%u\n",
	     xsp->number, xsp->cnt.read, xsp->cnt.read_pend);
      printk(DEVICE_NAME "%u: WRITE COUNTS: total=%u, pending=%u\n",
	     xsp->number, xsp->cnt.write, xsp->cnt.write_pend);
      printk(DEVICE_NAME "%u: SLEEP COUNTS: total=%u, pending=%u\n",
	     xsp->number, xsp->cnt.sleep, xsp->cnt.sleep_pend);

      if (!IsListEmpty(&xsp->pending_read_irps)) {
	    printk(DEVICE_NAME "%u: PENDING READ IRPs LISTED\n", xsp->number);
      }

      if (!IsListEmpty(&xsp->pending_write_irps)) {
	    printk(DEVICE_NAME "%u: PENDING WRITE IRPs LISTED\n", xsp->number);
      }

      if (!IsListEmpty(&xsp->pending_ioctl_irps)) {
	    printk(DEVICE_NAME "%u: PENDING IOCTL IRPs LISTED\n", xsp->number);
      }

      if (xsp->root_sync) {
	    printk(DEVICE_NAME "%u: Threads blocked on root_sync\n",
		   xsp->number);
      }

      if (xsp->dispatch_sync) {
	    printk(DEVICE_NAME "%u: Threads blocked on dispatch_sync\n",
		   xsp->number);
      }

      if (xsp->channels) {
	    struct ChannelData*xpd = xsp->channels;
	    printk(DEVICE_NAME "%u: Per Channel information...\n",
		   xsp->number);
	    do {
		  printk(DEVICE_NAME "%u.%u: CHANNEL READ COUNTS: "
			 "total=%u, pending=%u\n",
			 xsp->number, xpd->channel, xpd->cnt.read,
			 xpd->cnt.read_pend);

		  printk(DEVICE_NAME "%u.%u: CHANNEL WRITE COUNTS: "
			 "total=%u, pending=%u\n",
			 xsp->number, xpd->channel, xpd->cnt.write,
			 xpd->cnt.write_pend);


		  xpd = xpd->next;
	    } while (xsp->channels != xpd);
      }
}

static void do_ucrx_timeout(DEVICE_OBJECT*dev, IRP*irp)
{
      struct Instance*xsp = *((struct Instance**)dev->DeviceExtension);
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);

      struct ucrx_timeout_s*ts = (struct ucrx_timeout_s*)
	    irp->AssociatedIrp.SystemBuffer;

      struct ChannelData*xpd = channel_by_id(xsp, ts->id);
      if (xpd == 0) {
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    return;
      }

      if (ts->read_timeout >= 0) {
	    xpd->read_timeout = ts->read_timeout;

      } else switch (ts->read_timeout) {

	  case UCRX_TIMEOUT_OFF:
	    xpd->read_timeout = UCRX_TIMEOUT_OFF;
	    break;

	  case UCRX_TIMEOUT_FORCE:
	    xpd->read_timeout_flag = 1;
	    if (xpd->read_timing) {
		   KeCancelTimer(&xpd->read_timer.timer);
		   xpd->read_timing = 0;
	    }
	    KeInsertQueueDpc(&xsp->pending_io_dpc, 0, 0);
	    break;

	  default:
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    return;
      }

      irp->IoStatus.Status = STATUS_SUCCESS;
      irp->IoStatus.Information = 0;
}

/*
 * This passes the IOCTL to common code.
 */
static void do_standard_ioctl(DEVICE_OBJECT*dev, IRP*irp)
{
      long rc;
      unsigned long cmd, arg;
      struct Instance*xsp = *((struct Instance**)dev->DeviceExtension);
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);

      cmd = stp->Parameters.DeviceIoControl.IoControlCode;

      if (stp->Parameters.DeviceIoControl.InputBufferLength==sizeof arg)
	    arg = *(unsigned long*)irp->AssociatedIrp.SystemBuffer;
      else
	    arg = 0;

      rc = ucrx_ioctl(xsp, cmd, arg);
      if (rc == -ENOTTY)
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
      else
	    irp->IoStatus.Status = STATUS_SUCCESS;


      if (stp->Parameters.DeviceIoControl.OutputBufferLength == sizeof rc) {
	    *(long*)irp->AssociatedIrp.SystemBuffer = rc;
	    irp->IoStatus.Information = sizeof rc;
      } else {
	    irp->IoStatus.Information = 0;
      }

}

static NTSTATUS do_restart_board(DEVICE_OBJECT*dev, IRP*irp)
{
      struct Instance*xsp = *((struct Instance**)dev->DeviceExtension);

	/* Cannot restart the board if there are any channels open. */
      if (xsp->channels != 0) {
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    return irp->IoStatus.Status;
      }

      if (debug_flag&UCR_TRACE_UCRX)
	    printk("ucrx%u: restart ise%u\n", xsp->number, xsp->number);

	/* restart doorbell to the processor. Code on the processor
	   should notice this interrupt and restart itself. */
      dev_set_bells(xsp->dev, 0x40000000);

	/* Clear outbound interrupts and the table pointer. (The ISE
	   board will loop waiting for the table pointer to clear. If
	   the table pointer already is cleared, then it will continue
	   with its reset.) */
      dev_init_hardware(xsp->dev);

	/* Free all the frames that this board may have had. This is
	   safe to do because there are no longer any pointers to the
	   table, and the processor has been rebooted. */
      { unsigned idx;
        for (idx = 0 ;  idx < 16 ;  idx += 1)
	      ucr_free_frame(xsp, idx);
      }


      irp->IoStatus.Status = STATUS_SUCCESS;
      return irp->IoStatus.Status;
}

NTSTATUS devx_ioctl(DEVICE_OBJECT*dev, IRP*irp)
{
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      unsigned long cmd = stp->Parameters.DeviceIoControl.IoControlCode;

      switch (cmd) {
	  case UCRX_TIMEOUT:
	    do_ucrx_timeout(dev, irp);
	    break;

	  case UCRX_RESTART_BOARD:
	    do_restart_board(dev, irp);
	    break;

	  default:
	    do_standard_ioctl(dev, irp);
	    break;
      }

      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return irp->IoStatus.Status;
}


#define dev_name_str L"\\Device\\isex"
#define dev_link_str L"\\DosDevices\\isex"

/*
 * Unloading this device is relatively easy because there is nothing
 * that physically exists.
 */
void unload_devx(DEVICE_OBJECT*dev)
{
      struct Instance*xsp = *((struct Instance**)dev->DeviceExtension);
      UNICODE_STRING dev_link;
      wchar_t dev_link_buf[sizeof(dev_link_str) + 10];
      UNICODE_STRING number;
      wchar_t number_buf[10];

	/* Remove the symbolic link in \DosDevices */
      dev_link.Buffer = dev_link_buf;
      dev_link.MaximumLength = sizeof dev_link_buf;
      dev_link.Length = 0;
      RtlZeroMemory(dev_link_buf, sizeof dev_link_buf);
      RtlAppendUnicodeToString(&dev_link, dev_link_str);

      number.Buffer = number_buf;
      number.MaximumLength = sizeof(number_buf);
      number.Length = 0;
      RtlZeroMemory(number_buf, sizeof number_buf);
      RtlIntegerToUnicodeString(xsp->number, 10, &number);

      RtlAppendUnicodeStringToString(&dev_link, &number);
      IoDeleteSymbolicLink(&dev_link);
}


NTSTATUS create_devx(DRIVER_OBJECT*drv, struct Instance*xsp)
{
      NTSTATUS status;
      DEVICE_OBJECT*dev;
      UNICODE_STRING dev_name, dev_link;
      wchar_t dev_name_buf[sizeof(dev_name_str) + 10];
      wchar_t dev_link_buf[sizeof(dev_link_str) + 10];
      UNICODE_STRING number;
      wchar_t number_buf[10];

	/* Make the kernel device name (\Device\ise0) from the prefix
	   and the current instance number. Lots of UNICODE kruft. */
      dev_name.Buffer = dev_name_buf;
      dev_name.MaximumLength = sizeof(dev_name_buf);
      dev_name.Length = 0;
      RtlZeroMemory(dev_name_buf, sizeof dev_name_buf);
      RtlAppendUnicodeToString(&dev_name, dev_name_str);

      number.Buffer = number_buf;
      number.MaximumLength = sizeof(number_buf);
      number.Length = 0;
      RtlZeroMemory(number_buf, sizeof number_buf);
      RtlIntegerToUnicodeString(xsp->number, 10, &number);

      RtlAppendUnicodeStringToString(&dev_name, &number);


	/* Make the kernel device object and initialize the instance
	   structure with the basics. */
      status = IoCreateDevice(drv, sizeof(struct Instance*), &dev_name,
			      FILE_DEVICE_UCRX, 0, FALSE, &dev);
      if (! NT_SUCCESS(status)) return status;

      dev->Flags |= DO_DIRECT_IO;

      *((struct Instance**)dev->DeviceExtension) = xsp;

	/* Make the device accessible to WIN32 by creating a link in
	   \DosDevices to this driver. */
      dev_link.Buffer = dev_link_buf;
      dev_link.MaximumLength = sizeof dev_link_buf;
      dev_link.Length = 0;
      RtlZeroMemory(dev_link_buf, sizeof dev_link_buf);
      RtlAppendUnicodeToString(&dev_link, dev_link_str);

      number.Buffer = number_buf;
      number.MaximumLength = sizeof(number_buf);
      number.Length = 0;
      RtlZeroMemory(number_buf, sizeof number_buf);
      RtlIntegerToUnicodeString(xsp->number, 10, &number);

      RtlAppendUnicodeStringToString(&dev_link, &number);
      IoCreateSymbolicLink(&dev_link, &dev_name);


      return STATUS_SUCCESS;
}

/*
 * $Log$
 * Revision 1.3  2001/07/12 22:49:42  steve
 *  Add support for UCRX_RESTART_BOARD, and
 *  support read timeouts.
 *
 * Revision 1.2  2001/07/12 20:31:05  steve
 *  Support UCRX_TIMEOUT_FORCE
 *
 * Revision 1.1  2001/03/05 20:11:40  steve
 *  Add NT4 driver to ISE source tree.
 *
 * Revision 1.4  1999/08/18 00:06:00  steve
 *  Fix SMP synchronization problem.
 *
 * Revision 1.3  1999/07/15 02:11:12  steve
 *  Count pending IRPs.
 *
 * Revision 1.2  1998/09/29 00:39:36  steve
 *  Add os specific state dump.
 *
 * Revision 1.1  1998/05/28 22:53:02  steve
 *  NT port.
 *
 * Revision 1.4  1998/04/18 02:43:55  steve
 *  Fix trouble removing device lines at unload.
 *  Add support for read cancellation under NT.
 *  isecons device (nt only) now throws away old
 *  data instead of truncating new.
 *
 * Revision 1.3  1998/04/01 01:27:58  steve
 *  Change device names to be consistent
 *
 * Revision 1.2  1997/12/06 05:15:51  steve
 *  Add the control device to the NT port.
 *
 * Revision 1.1  1997/12/02 02:29:04  steve
 *  Add support for ISE board reset.
 *
 */

