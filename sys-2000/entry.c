/*
 * Copyright (c) 2001 Picture Elements, Inc.
 *    Stephen Williams (steve@picturel.com)
 *
 * $Id$
 */

# include  "ise_sys.h"

/*
 * This file contains the common entry point code that is more or less
 * independent of the device or device node. These support the initial
 * interractions with Windows 2000.
 */

/*
 * As ise boards are disabled and re-enabled, this mask remembers
 * which id numbers are used and which are free. When a board is
 * enabled, it is attached to the lowest free number.
 */
static unsigned long ise_existence_mask = 0;

static NTSTATUS xxcreate(DEVICE_OBJECT*fdo, IRP*irp)
{
      switch (fdo->DeviceType) {

	  case FILE_DEVICE_ISE:
	    return dev_create(fdo, irp);

	  case FILE_DEVICE_UCRX:
	    irp->IoStatus.Status = STATUS_SUCCESS;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_SUCCESS;

	  case FILE_DEVICE_ISE_CONS:
	    DbgPrint("isecons: open\n");
	    irp->IoStatus.Status = STATUS_SUCCESS;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_SUCCESS;

	  default:
	    return STATUS_UNSUCCESSFUL;
      }
}

static NTSTATUS xxclose(DEVICE_OBJECT*fdo, IRP*irp)
{
      switch (fdo->DeviceType) {

	  case FILE_DEVICE_ISE:
	    return dev_close(fdo, irp);

	  case FILE_DEVICE_UCRX:
	    irp->IoStatus.Status = STATUS_SUCCESS;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_SUCCESS;

	  case FILE_DEVICE_ISE_CONS:
	    DbgPrint("isecons: close\n");
	    irp->IoStatus.Status = STATUS_SUCCESS;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_SUCCESS;

	  default:
	    return STATUS_UNSUCCESSFUL;
      }
}

static NTSTATUS xxread(DEVICE_OBJECT*fdo, IRP*irp)
{
      switch (fdo->DeviceType) {

	  case FILE_DEVICE_ISE:
	    return dev_read(fdo, irp);

	  case FILE_DEVICE_ISE_CONS:
	    return cons_read(fdo, irp);

	  default:
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }
}

static NTSTATUS xxwrite(DEVICE_OBJECT*fdo, IRP*irp)
{
      switch (fdo->DeviceType) {

	  case FILE_DEVICE_ISE:
	    return dev_write(fdo, irp);

	  default:
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }
}

static NTSTATUS xxioctl(DEVICE_OBJECT*dev, IRP*irp)
{
      switch (dev->DeviceType) {

	  case FILE_DEVICE_ISE:
	    return dev_ioctl(dev, irp);

	  case FILE_DEVICE_UCRX:
	    return isex_ioctl(dev, irp);

	  case FILE_DEVICE_ISE_CONS:
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;

	  default:
	    return STATUS_UNSUCCESSFUL;
      }
}


static NTSTATUS pnp_start_device(DEVICE_OBJECT*fdo, IRP*irp);
static NTSTATUS pnp_stop_device(DEVICE_OBJECT*fdo, IRP*irp);
static NTSTATUS pnp_remove_device(DEVICE_OBJECT*fdo, IRP*irp);

/*
 * The xxPnP function handles all the various PnP functions that WDM
 * throws at us. This includes starting and stopping the device, and
 * unloading (detaching the driver from) the device. Initialling
 * attaching the device is handled by AddDevice so that it can create
 * an fdo that can receive a PnP IRP in the first place.
 */
static NTSTATUS xxPnP(DEVICE_OBJECT*fdo, IRP*irp)
{
      IO_STACK_LOCATION*iop = IoGetCurrentIrpStackLocation(irp);
      struct instance_t*xsp = (struct instance_t*)fdo->DeviceExtension;


      switch (iop->MinorFunction) {

	  case IRP_MN_START_DEVICE:
	    return pnp_start_device(fdo, irp);

	  case IRP_MN_QUERY_STOP_DEVICE:
	    printk("ise%u: QUERY_STOP_DEVICE called\n", xsp->id);
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;

	  case IRP_MN_STOP_DEVICE:
	    return pnp_stop_device(fdo, irp);

	  case IRP_MN_QUERY_REMOVE_DEVICE:
	      /* We can do this, simply by passing the request down. */
	    printk("ise%u: QUERY_REMOVE_DEVICE called\n", xsp->id);
	    irp->IoStatus.Status = STATUS_SUCCESS;
	    IoSkipCurrentIrpStackLocation(irp);
	    return IoCallDriver(xsp->next_dev, irp);

	  case IRP_MN_REMOVE_DEVICE:
	    return pnp_remove_device(fdo, irp);

	  default:
	    printk("ise%u: xxPnP[%u] called\n",
		     xsp->id, iop->MinorFunction);
	    IoSkipCurrentIrpStackLocation(irp);
	    return IoCallDriver(xsp->next_dev, irp);
      }

      return STATUS_SUCCESS;
}


static NTSTATUS wakeup_completer(DEVICE_OBJECT*fdo, IRP*irp, KEVENT*ev)
{
      KeSetEvent(ev, 0, FALSE);
      return STATUS_MORE_PROCESSING_REQUIRED;
}

/*
 * This is the opposite of the pnp_start_device, and does things in
 * the reverse order. That is, it cleans up the hardware first, then
 * passes the IRP down to the next driver for completion.
 *
 * This function is only used to temporarily stop the device, in other
 * words to withdraw its resource allocation. Disabling the device
 * with the device manager will cause a PNP_REMOVE_DEVICE instead. The
 * device object, therefore, must *not* be deleted. Only detach
 * resources, in expectation of a new START_DEVICE.
 *
 * NOTE: The ISE driver does not support this, so the
 * PNP_QUERY_STOP_REQUEST returning false should prevent it ever
 * happening.
 */
static NTSTATUS pnp_stop_device(DEVICE_OBJECT*fdo, IRP*irp)
{
      struct instance_t*xsp = (struct instance_t*)fdo->DeviceExtension;

      pnp_stop_ise(fdo);

      irp->IoStatus.Status = STATUS_SUCCESS;
      IoSkipCurrentIrpStackLocation(irp);

      return IoCallDriver(xsp->next_dev, irp);
}

/*
 * After the device driver is added by AddDevice, the PNP manager goes
 * off and does some stuff, then passes the PNP_START_DEVICE IRP to
 * give me the resources of the device. This is where I turn on the
 * device and enable interaction with it.
 *
 * Note that I can't do this right away, I must first pass down the
 * IRP to lower levels of the device stack. This gives HAL a chance to
 * map and claim resources, and do other fixup. I only enable the
 * board when the IRP is on the way back.
 */
static NTSTATUS pnp_start_device(DEVICE_OBJECT*fdo, IRP*irp)
{
      struct instance_t*xsp = (struct instance_t*)fdo->DeviceExtension;
      NTSTATUS status;
      KEVENT event;

	/* First, pass the IRP down to the next device object for
	   initial processing. This may cause the thread to be
	   suspended. So be it, we will wait. */
      KeInitializeEvent(&event, NotificationEvent, FALSE);
      IoCopyCurrentIrpStackLocationToNext(irp);
      IoSetCompletionRoutine(irp, wakeup_completer, &event, TRUE, TRUE, TRUE);
      status = IoCallDriver(xsp->next_dev, irp);
      if (status == STATUS_PENDING) {
	    KeWaitForSingleObject(&event, Executive, KernelMode, FALSE, 0);
	    status = irp->IoStatus.Status;
      }

	/* If there was an error in lower drivers, then skip my device
	   (log a convenient message) and pass the code up. */
      if (!NT_SUCCESS(status)) {
	    printk("ise%u: START_DEVICE error from lower device: 0x%x\n",
		   xsp->id, status);
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return status;
      }

	/* Now do the device specific handling of the IRP. */
      return pnp_start_ise(fdo, irp);
}

/*
 * This is called by the PnP manager to detach a device from the
 * device driver. This can be invoked at run time by using the device
 * manager to disable the device. Some PnP IRPs are created to check
 * that it is a valid thing to do, then this function is called to
 * actually do the remove. This function is also used when it is time
 * to unload the driver, possibly for system shutdown.
 *
 * Once this function returns, the fdo is no longer referenced. This
 * function *removes* the fdo and the fdx of the isex device.
 */
static NTSTATUS pnp_remove_device(DEVICE_OBJECT*fdo, IRP*irp)
{
      DEVICE_OBJECT*next_dev;
      struct instance_t*xsp = (struct instance_t*)fdo->DeviceExtension;
      unsigned dev_no = xsp->id;

      printk("ise%u: REMOVE_DEVICE called\n", xsp->id);

	/* First make sure the device is stopped. */
      pnp_stop_ise(fdo);

	/* Delete the isex device. */
      remove_isex(xsp->fdx);

	/* Detach the device from the stack and delete it. */
      next_dev = xsp->next_dev;
      if (next_dev) IoDetachDevice(next_dev);

	/* Remove the device object and associated handles. */
      remove_ise(fdo);

      ise_existence_mask &= ~ (1 << dev_no);

      irp->IoStatus.Status = STATUS_SUCCESS;
      IoSkipCurrentIrpStackLocation(irp);
      return IoCallDriver(next_dev, irp);
}

/*
 * This function is called when a device is attached to the driver by
 * the PnP engine. Normally this happens during the boot process, but
 * can also happen if the device is Enabled via the device manager.
 */
static NTSTATUS AddDevice(DRIVER_OBJECT*drv, DEVICE_OBJECT*pdo)
{
      unsigned dev_no;

      NTSTATUS status;
      DEVICE_OBJECT*fdo;
      struct instance_t*xsp;

	/* Choose the dev_no as the first free number in the mask. */
      dev_no = 0;
      while (ise_existence_mask & (1 << dev_no))
	    dev_no += 1;

      ise_existence_mask |= (1 << dev_no);

      printk("ise%u: AddDevice\n", dev_no);

	/* Create the device object for the ISE board. */
      fdo = create_ise(drv, dev_no);

      xsp = (struct instance_t*)fdo->DeviceExtension;
      xsp->pdo = pdo;
      xsp->next_dev = IoAttachDeviceToDeviceStack(fdo, pdo);
      xsp->id = dev_no;

	/* Create the isex device object. */
      xsp->fdx = create_isex(drv, xsp);

      return STATUS_SUCCESS;
}

/*
 * No power management supported.
 */
static NTSTATUS xxPower(DEVICE_OBJECT*fdo, IRP*irp)
{
      struct instance_t*xsp = (struct instance_t*)fdo->DeviceExtension;

      PoStartNextPowerIrp(irp);
      IoSkipCurrentIrpStackLocation(irp);
      return PoCallDriver(xsp->next_dev, irp);
}

/*
 * Not a WMI data provider.
 */
static NTSTATUS xxSystemControl(DEVICE_OBJECT*fdo, IRP*irp)
{
      struct instance_t*xsp = (struct instance_t*)fdo->DeviceExtension;
      IoSkipCurrentIrpStackLocation(irp);
      return IoCallDriver(xsp->next_dev, irp);
}

static void unload(DRIVER_OBJECT*drv)
{
      DbgPrint("ise: unloading driver\n");
      unload_console(drv);
}

NTSTATUS DriverEntry(DRIVER_OBJECT*drv, UNICODE_STRING*path)
{
      drv->MajorFunction[IRP_MJ_CREATE] = xxcreate;
      drv->MajorFunction[IRP_MJ_CLOSE]  = xxclose;
      drv->MajorFunction[IRP_MJ_READ]   = xxread;
      drv->MajorFunction[IRP_MJ_WRITE]  = xxwrite;
      drv->MajorFunction[IRP_MJ_DEVICE_CONTROL] = xxioctl;
      drv->MajorFunction[IRP_MJ_PNP]    = xxPnP;
      drv->MajorFunction[IRP_MJ_POWER]  = xxPower;
      drv->MajorFunction[IRP_MJ_SYSTEM_CONTROL] = xxSystemControl;
      drv->DriverUnload = unload;
      drv->DriverExtension->AddDevice = AddDevice;


	/* Create the one and only console device for this driver. */
      create_console(drv);
      printk("ise: Enter ISE.sys\n");

      return STATUS_SUCCESS;
}


/*
 * $Log$
 * Revision 1.3  2002/04/10 23:20:27  steve
 *  Do not touch IRP after it is completed.
 *
 * Revision 1.2  2002/04/10 21:05:30  steve
 *  Add stup WMI and Power functions
 *
 * Revision 1.1  2001/07/26 00:31:30  steve
 *  Windows 2000 driver.
 *
 */

