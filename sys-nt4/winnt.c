/*
 * Copyright (c) 1997-1998 Picture Elements, Inc.
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
#if !defined(WINNT)
#ident "$Id$"
#endif

#include  "os.h"
#include  "t-config.h"
#include  "ucrpriv.h"

/*
 * This file contains some of the initial overhead code of the Windows
 * NT device driver. All the DRIVER functions are here. The device
 * functions live elsewhere.
 */

extern void* alloc_memory(unsigned long size)
{
      PHYSICAL_ADDRESS limit;
      limit.LowPart = 0xffffffff;
      limit.HighPart = 0;
      return MmAllocateContiguousMemory(size, limit);
}

static void irp_complete_ok(IRP*irp)
{
      irp->IoStatus.Status = STATUS_SUCCESS;
      irp->IoStatus.Information = 0;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
}

static NTSTATUS xxcreate(DEVICE_OBJECT*dev, IRP*irp)
{
      switch (dev->DeviceType) {

	  case FILE_DEVICE_ISE:
	    return dev_create(dev, irp);

	  case FILE_DEVICE_UCRX:
	    irp_complete_ok(irp);
	    return STATUS_SUCCESS;

	  case FILE_DEVICE_ISE_CONS:
	    irp_complete_ok(irp);
	    return STATUS_SUCCESS;

	  default:
	    return STATUS_UNSUCCESSFUL;
      }
}

static NTSTATUS xxclose(DEVICE_OBJECT*dev, IRP*irp)
{
      switch (dev->DeviceType) {

	  case FILE_DEVICE_ISE:
	    return dev_close(dev, irp);

	  case FILE_DEVICE_UCRX:
	    irp_complete_ok(irp);
	    return STATUS_SUCCESS;

	  case FILE_DEVICE_ISE_CONS:
	    irp_complete_ok(irp);
	    return STATUS_SUCCESS;

	  default:
	    return STATUS_UNSUCCESSFUL;
      }
}

static NTSTATUS xxread(DEVICE_OBJECT*dev, IRP*irp)
{
      switch (dev->DeviceType) {

	  case FILE_DEVICE_ISE:
	    return dev_read(dev, irp, FALSE);

	  case FILE_DEVICE_ISE_CONS:
	    return cons_read(dev, irp);

	  default:
	    return STATUS_UNSUCCESSFUL;
      }
}

static NTSTATUS xxwrite(DEVICE_OBJECT*dev, IRP*irp)
{
      switch (dev->DeviceType) {

	  case FILE_DEVICE_ISE:
	    return dev_write(dev, irp, FALSE);

	  default:
	    return STATUS_UNSUCCESSFUL;
      }
}

static NTSTATUS xxioctl(DEVICE_OBJECT*dev, IRP*irp)
{
      switch (dev->DeviceType) {

	  case FILE_DEVICE_ISE:
	    return dev_ioctl(dev, irp, FALSE);

	  case FILE_DEVICE_UCRX:
	    return devx_ioctl(dev, irp);

	  case FILE_DEVICE_ISE_CONS:
	    irp_complete_ok(irp);
	    return STATUS_UNSUCCESSFUL;

	  default:
	    return STATUS_UNSUCCESSFUL;
      }
}

static void get_debug_flag_from_registry(UNICODE_STRING*reg_path)
{
      static const unsigned long debug_default = 0;
      RTL_QUERY_REGISTRY_TABLE table[2];
      unsigned long size = reg_path->Length+sizeof(wchar_t);
      wchar_t*str = ExAllocatePool(PagedPool, size);

      RtlCopyMemory(str, reg_path->Buffer, reg_path->Length);
      str[size/sizeof(wchar_t) - 1] = 0;

      RtlZeroMemory(table, sizeof table);
      table[0].Name  = L"DebugFlag";
      table[0].Flags = RTL_QUERY_REGISTRY_DIRECT;
      table[0].EntryContext = &debug_flag;
      table[0].DefaultType  = REG_DWORD;
      table[0].DefaultData  = (void*)&debug_default;
      table[0].DefaultLength= sizeof debug_default;

      RtlQueryRegistryValues(RTL_REGISTRY_ABSOLUTE, str, table, 0, 0);

      ExFreePool(str);
}

static unsigned long get_frame_pool_from_registry(UNICODE_STRING*reg_path)
{
      unsigned long poolsize;
      static const unsigned long poolsize_default = 0;
      RTL_QUERY_REGISTRY_TABLE table[2];
      unsigned long size = reg_path->Length+sizeof(wchar_t);
      wchar_t*str = ExAllocatePool(PagedPool, size);

      RtlCopyMemory(str, reg_path->Buffer, reg_path->Length);
      str[size/sizeof(wchar_t) - 1] = 0;

      RtlZeroMemory(table, sizeof table);
      table[0].Name  = L"FramePool";
      table[0].Flags = RTL_QUERY_REGISTRY_DIRECT;
      table[0].EntryContext = &poolsize;
      table[0].DefaultType  = REG_DWORD;
      table[0].DefaultData  = (void*)&poolsize_default;
      table[0].DefaultLength= sizeof poolsize_default;

      RtlQueryRegistryValues(RTL_REGISTRY_ABSOLUTE, str, table, 0, 0);

      ExFreePool(str);

	  return poolsize;
}

static void driver_unload(DRIVER_OBJECT*drv)
{
      DEVICE_OBJECT*dev = drv->DeviceObject;
      while (dev) {
	    struct Instance*xsp;
	    DEVICE_OBJECT*next = dev->NextDevice;

	    switch (dev->DeviceType) {

		case FILE_DEVICE_ISE:
		  unload_device(drv, dev);
		  break;

		case FILE_DEVICE_UCRX:
		  unload_devx(dev);
		  break;

		case FILE_DEVICE_ISE_CONS:
		  unload_console(dev);
		  break;

		default:
		  break;
	    }

	    IoDeleteDevice(dev);
	    dev = next;
      }

      ucr_deinit_nt_frame_pool();
}

/*
 * The NT kernel calls me here, when I am loaded. Installed, that
 * is. I set up the driver object to dispatch the entry points
 * correctly and locate any devices that match my numbers.
 */

NTSTATUS DriverEntry(DRIVER_OBJECT*drv, UNICODE_STRING*reg_path)
{
      unsigned bus_no;
      unsigned dev_num = 0;
      unsigned long frame_pool;
      NTSTATUS status;

      drv->DeviceObject = 0;
      drv->DriverUnload = driver_unload;
      drv->MajorFunction[IRP_MJ_CREATE] = xxcreate;
      drv->MajorFunction[IRP_MJ_CLOSE]  = xxclose;
      drv->MajorFunction[IRP_MJ_READ]   = xxread;
      drv->MajorFunction[IRP_MJ_WRITE]  = xxwrite;
      drv->MajorFunction[IRP_MJ_DEVICE_CONTROL] = xxioctl;


      get_debug_flag_from_registry(reg_path);
      frame_pool = get_frame_pool_from_registry(reg_path);

      status = create_console(drv);

      if (debug_flag != 0) printk("ISE: debug_flag==%u\n", debug_flag);
	  printk("frame pool size == %u\n", frame_pool);

	/* This loop scans the PCI bus looking for instances of the
	   ISE board. Call create_device for each board I find. */

      for (bus_no = 0 ;  bus_no < 256 ;  bus_no += 1) {

	    PCI_SLOT_NUMBER slot;
	    unsigned dev, fn;

	    slot.u.AsULONG = 0;

	    for (dev = 0 ; dev < 32 ;  dev += 1) {

		  for (fn = 0 ;  fn < 8 ;  fn += 1) {
			PCI_COMMON_CONFIG config;
			unsigned long rc;

			slot.u.bits.DeviceNumber = dev;
			slot.u.bits.FunctionNumber = fn;

			rc = HalGetBusData(PCIConfiguration, bus_no,
					   slot.u.AsULONG, &config, 4);

			switch (rc) {
			    case 0: /* bus number invalid */
			      fn = 8;
			      dev = 32;
			      bus_no = 256;
			      break;
			    case 2: /* bus OK, but no device */
			      fn = 8;
			      break;

			    default: if ((config.VendorID == VENDOR_ID) &&
					 (config.DeviceID == DEVICE_ID)) {

				  status = create_device(drv, bus_no,
							 slot.u.AsULONG,
							 dev_num);
				  if (status == STATUS_SUCCESS)
					dev_num += 1;
			    }
			}
		  }
	    }
      }

      if (dev_num == 0) {
	    return STATUS_NO_SUCH_DEVICE;
      }

      if (! ucr_init_nt_frame_pool(frame_pool)) {
	    driver_unload(drv);
	    return STATUS_NO_MEMORY;
      }

      return STATUS_SUCCESS;
}

/*
 * $Log$
 * Revision 1.1  2001/03/05 20:11:40  steve
 *  Add NT4 driver to ISE source tree.
 *
 * Revision 1.4  1998/09/23 23:12:57  steve
 *  Better detect diagnose broken interrupts.
 *
 * Revision 1.3  1998/05/30 18:59:43  steve
 *  Allocate contiguous memory.
 *
 * Revision 1.2  1998/05/29 18:11:22  steve
 *  Proper cancel/abort behavior for NT.
 *
 * Revision 1.1  1998/05/28 22:53:02  steve
 *  NT port.
 *
 */

