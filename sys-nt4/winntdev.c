/*
 * Copyright (c) 1997-1999 Picture Elements, Inc.
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

/*
 * This file contains the interface functions for the ISEx
 * devices. The read/write/ioctl functions for actually accessing the
 * channels is implemented in this file.
 */
#include  "ucrif.h"
#include  "os.h"
#include  "t-config.h"
#include  "ucrpriv.h"

#define get_channel(irp) ((struct ChannelData*)(IoGetCurrentIrpStackLocation(irp)->FileObject->FsContext))


/*
 * This is the cancel function that handles enqueued read IRPs. All I
 * have to do here is remove from the pending_read_irps list and
 * cancel the IRP.
 */
static void dev_cancel_irp(DEVICE_OBJECT*dev, IRP*irp)
{
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct Instance*xsp = (struct Instance*)dev->DeviceExtension;
      struct ChannelData*xpd;

      if (debug_flag & UCR_TRACE_CHAN) {
	    struct ChannelData*xpd = get_channel(irp);
	    printk(DEVICE_NAME "%u.%u (d): Cancel irp.\n",
		   xsp->number, xpd->channel);
      }

      RemoveEntryList(&irp->Tail.Overlay.ListEntry);

      xpd = get_channel(irp);
      switch (stp->MajorFunction) {
	  case IRP_MJ_WRITE:
	    xsp->cnt.write_pend -= 1;
	    xpd->cnt.write_pend -= 1;
	    break;
	  case IRP_MJ_READ:
	    xsp->cnt.read_pend -= 1;
	    xpd->cnt.read_pend -= 1;
	    break;
	  default:
	    break;
      }

      irp->IoStatus.Status = STATUS_CANCELLED;
      irp->IoStatus.Information = 0;

      IoReleaseCancelSpinLock(irp->CancelIrql);
      IoCompleteRequest(irp, IO_NO_INCREMENT);
}

void add_to_pending(struct ccp_t*ccp)
{
      KIRQL save_irql;

      IoAcquireCancelSpinLock(&save_irql);
      InsertTailList(ccp->pend_list, &ccp->irp->Tail.Overlay.ListEntry);
      IoSetCancelRoutine(ccp->irp, dev_cancel_irp);
      if (! ccp->pend_flag) {
	    IoMarkIrpPending(ccp->irp);
	    ccp->pend_flag = TRUE;
      }
      IoReleaseCancelSpinLock(save_irql);
      ccp->pend_list = 0;
}


NTSTATUS dev_create(DEVICE_OBJECT*dev, IRP*irp)
{
      struct Instance*xsp = (struct Instance*)dev->DeviceExtension;
      struct ChannelData*xpd;
      int rc;

      xpd = ExAllocatePool(NonPagedPool, sizeof(struct ChannelData));
      if (xpd == 0) {
	    irp->IoStatus.Status = STATUS_NO_MEMORY;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_NO_MEMORY;
      }

      get_channel(irp) = xpd;

      xpd->block_flag = 0;
      xpd->ccp_read.pend_list = 0;
      xpd->cnt.read = 0;
      xpd->cnt.read_pend = 0;
      xpd->cnt.write = 0;
      xpd->cnt.write_pend = 0;
      xpd->ccp_write.pend_list = 0;
      xpd->ccp_ioctl.pend_list = 0;

      rc = ucr_open(xsp, xpd);

      if (rc == 0) {
	    irp->IoStatus.Status = STATUS_SUCCESS;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_SUCCESS;
      }

      ExFreePool(xpd);
      get_channel(irp) = 0;

      switch (rc) {
	  default:
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
      }

      return STATUS_UNSUCCESSFUL;
}

NTSTATUS dev_close(DEVICE_OBJECT*dev, IRP*irp)
{
      struct Instance*xsp = (struct Instance*)dev->DeviceExtension;
      struct ChannelData*xpd = get_channel(irp);

	/* Doesn't make a whole heck of a lot of sense to support
	   cancel of close, so invoke the blocking behavior. */
	//xpd->irp = irp;
	//xpd->pend_list = 0;
      ucr_release(xsp, xpd);

      ExFreePool(xpd);

      irp->IoStatus.Status = STATUS_SUCCESS;
      irp->IoStatus.Information = 0;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return STATUS_SUCCESS;
}

NTSTATUS dev_read(DEVICE_OBJECT*dev, IRP*irp, BOOLEAN retry_flag)
{
      char*base;
      unsigned long cnt;
      KIRQL save_irql;
      int rc, flag;
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct Instance*xsp = (struct Instance*)dev->DeviceExtension;
      struct ChannelData*xpd = get_channel(irp);
      struct ccp_t*ccp = &xpd->ccp_read;

	/* Use the low bit of the block_flag flag to turn off blocking
	   for at least one byte. This works by making ucr_open return
	   0 instead of -EINTR if there is no data. */
      flag = (xpd->block_flag & 1) ? 0 : 1;

	/* If this is an actual call from outside the kernel, then
	   reset the state of the operation, mark counters, and get
	   ready for action. */
      if (! retry_flag) {
	    xsp->cnt.read += 1;
	    xsp->cnt.read_pend += 1;
	    xpd->cnt.read += 1;
	    xpd->cnt.read_pend += 1;
	    xpd->read_timeout_flag = 0;
	    stp->Parameters.Read.ByteOffset.LowPart = 0;
	    ccp->pend_flag = FALSE;
      }


      base = irp->AssociatedIrp.SystemBuffer;
      base += stp->Parameters.Read.ByteOffset.LowPart;
      cnt = stp->Parameters.Read.Length;
      cnt -= stp->Parameters.Read.ByteOffset.LowPart;

	/* Call ucr_read to get things started. If it does something,
	   then it returns >0. If it can't read anything, and this
	   call must block, then it return -EINTR and it is up to me
	   to mark it pending and arrange for a restart. */
      ccp->irp = irp;
      ccp->pend_list = &xsp->pending_read_irps;
      rc = ucr_read(xsp, xpd, ccp, base, cnt, flag);

      if (rc == -EINTR)
	    return STATUS_PENDING;


      if (rc >= 0) {
	    irp->IoStatus.Status = STATUS_SUCCESS;
	    irp->IoStatus.Information
		  = rc + stp->Parameters.Read.ByteOffset.LowPart;

      } else {
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;

      }

      xsp->cnt.read_pend -= 1;
      xpd->cnt.read_pend -= 1;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return irp->IoStatus.Status;
}

NTSTATUS dev_write(DEVICE_OBJECT*dev, IRP*irp, BOOLEAN retry_flag)
{
      const char*base;
      unsigned cnt;
      int rc;
      struct Instance*xsp = (struct Instance*)dev->DeviceExtension;
      struct ChannelData*xpd = get_channel(irp);
      struct ccp_t*ccp = &xpd->ccp_write;
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);

      if (! retry_flag) {
	    xsp->cnt.write += 1;
	    xsp->cnt.write_pend += 1;
	    xpd->cnt.write += 1;
	    xpd->cnt.write_pend += 1;
	    stp->Parameters.Write.ByteOffset.LowPart = 0;
	    ccp->pend_flag = FALSE;
      }

      base = irp->AssociatedIrp.SystemBuffer;
      base += stp->Parameters.Write.ByteOffset.LowPart;

	/* continue writing so long as there are bytes yet to be
	   delivered. This may take a few iterations as we cross
	   boundaries. If we get to a point where we would block, then
	   the ucr_write will return -EINTR. In that case, mark the
	   IRP pending and go away. */
      ccp->irp = irp;
      ccp->pend_list = &xsp->pending_write_irps;
      while (stp->Parameters.Write.ByteOffset.LowPart
	     < stp->Parameters.Write.Length) {

	    cnt = stp->Parameters.Write.Length;
	    cnt -= stp->Parameters.Write.ByteOffset.LowPart;
	    rc = ucr_write(xsp, xpd, ccp, base, cnt);

	    if (rc == -EINTR)
		  return STATUS_PENDING;

	    if (rc > 0) {
		  stp->Parameters.Write.ByteOffset.LowPart += rc;
		  base += rc;
	    }
      }


      if (rc >= 0) {
	    irp->IoStatus.Status = STATUS_SUCCESS;
	    irp->IoStatus.Information
		  = stp->Parameters.Write.ByteOffset.LowPart;

      } else {
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;

      }

      xsp->cnt.write_pend -= 1;
      xpd->cnt.write_pend -= 1;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return irp->IoStatus.Status;
}

static NTSTATUS dev_mmap(struct Instance*xsp, IRP*irp)
{
      UNICODE_STRING phys_name;
      HANDLE phys_handle;
      OBJECT_ATTRIBUTES phys_attr;
      LARGE_INTEGER frame_base;
      NTSTATUS rc;

      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct UcrMmapInfo*mapinfo;

      mapinfo = (struct UcrMmapInfo*)irp->AssociatedIrp.SystemBuffer;
      if (mapinfo->frame_id >= 16) {
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_UNSUCCESSFUL;
      }

      if (xsp->frame[mapinfo->frame_id] == 0) {
	    irp->IoStatus.Status = STATUS_NO_MEMORY;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_NO_MEMORY;
      }

      mapinfo->off  = 0;
      mapinfo->size = xsp->frame[mapinfo->frame_id]->page_count *
	    xsp->frame[mapinfo->frame_id]->page_size;

      RtlInitUnicodeString(&phys_name, L"\\Device\\PhysicalMemory");

      InitializeObjectAttributes(&phys_attr, &phys_name,
				 OBJ_CASE_INSENSITIVE, 0, 0);

      rc = ZwOpenSection(&phys_handle, SECTION_ALL_ACCESS, &phys_attr);
      if (rc != STATUS_SUCCESS) {
	    printk(DEVICE_NAME "%u: error opening physical memory.\n",
		   xsp->number);
	    irp->IoStatus.Status = rc;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return rc;
      }

      mapinfo->base = 0;
      frame_base.HighPart = 0;
      frame_base.LowPart = xsp->frame[mapinfo->frame_id]->page[0];

      rc = ZwMapViewOfSection(phys_handle, (HANDLE)-1, &mapinfo->base, 0L,
			      mapinfo->size, &frame_base, &mapinfo->size,
			      ViewShare, 0, PAGE_READWRITE|PAGE_NOCACHE);
      if (rc != STATUS_SUCCESS) {
	    printk(DEVICE_NAME "%u: unable to map section. rc=%x, mapinfo->size=%x\n",
		    xsp->number, rc, mapinfo->size);
	    irp->IoStatus.Status = rc;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return rc;
      }

      irp->IoStatus.Status = STATUS_SUCCESS;
      irp->IoStatus.Information = sizeof *mapinfo;
      IoCompleteRequest(irp, IO_NO_INCREMENT);

      return irp->IoStatus.Status;
}

static NTSTATUS dev_munmap(struct Instance*xsp, IRP*irp)
{
      struct UcrMmapInfo *mapinfo;

      mapinfo = (struct UcrMmapInfo*)irp->AssociatedIrp.SystemBuffer;
      ZwUnmapViewOfSection((HANDLE)-1, mapinfo->base);

      irp->IoStatus.Status = STATUS_SUCCESS;
      irp->IoStatus.Information = 0;
      IoCompleteRequest(irp, IO_NO_INCREMENT);

      return irp->IoStatus.Status;
}

static NTSTATUS dev_test(struct Instance*xsp, struct ChannelData*xpd, IRP*irp)
{
      unsigned idx;
      printk("XXXX First word of %u pages of frame 0...\n",
	     xsp->frame[0]->page_count);

      irp->IoStatus.Status = STATUS_SUCCESS;
      irp->IoStatus.Information = 0;
      IoCompleteRequest(irp, IO_NO_INCREMENT);

      return irp->IoStatus.Status;
}

/*
 * Process a DeviceIoControl call on the device. Get the input
 * argument and the command code, format them so that the generic code
 * can cope and let it fly.
 *
 * There are a few codes that are NT specific, so process them
 * directly.
 */
NTSTATUS dev_ioctl(DEVICE_OBJECT*dev, IRP*irp, BOOLEAN retry_flag)
{
      int rc;
      unsigned long cmd, arg;
      struct Instance*xsp = (struct Instance*)dev->DeviceExtension;
      struct ChannelData*xpd = get_channel(irp);
      struct ccp_t*ccp = &xpd->ccp_ioctl;
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);

      cmd = stp->Parameters.DeviceIoControl.IoControlCode;

      switch (cmd) {

	  case UCR_MMAP_FRAME:
	    return dev_mmap(xsp, irp);
	  case UCR_MUNMAP_FRAME:
	    return dev_munmap(xsp, irp);

	  case UCR_NONBLOCK:
	    if ( (stp->Parameters.DeviceIoControl.InputBufferLength ==
		  sizeof(unsigned long))
		 && (*(unsigned long*)irp->AssociatedIrp.SystemBuffer) )
		  xpd->block_flag |= 1;
	    else
		  xpd->block_flag &= ~1;

	    rc = 0;
	    break;

	  case UCR_TEST:
	    return dev_test(xsp, xpd, irp);

	  case UCR_FLUSH:
	  case UCR_SYNC:
	    ccp->irp = irp;
	    ccp->pend_list = &xsp->pending_ioctl_irps;
	    rc = ucr_ioctl(xsp, xpd, ccp, cmd, 0);
	    if (rc == -EINTR)
		  return STATUS_PENDING;

	    break;

	  default:
	      /* This is a standard UCR IOCTL, use the generic code to
		 process it. */

	    if (stp->Parameters.DeviceIoControl.InputBufferLength
		== sizeof arg)
		  arg = *(unsigned long*)irp->AssociatedIrp.SystemBuffer;
	    else
		  arg = 0;

	    ccp->pend_list = 0;
	    rc = ucr_ioctl(xsp, xpd, ccp, cmd, arg);
	    break;
      }

      if (rc < 0) {
	    irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	    irp->IoStatus.Information = 0;

      } else {
	    if (stp->Parameters.DeviceIoControl.OutputBufferLength >=
		sizeof(unsigned long)) {
		  *(unsigned long*)irp->AssociatedIrp.SystemBuffer = rc;
		  irp->IoStatus.Information = sizeof (unsigned long);
	    }
	    irp->IoStatus.Status = STATUS_SUCCESS;
      }

      IoCompleteRequest(irp, IO_NO_INCREMENT);
      return irp->IoStatus.Status;
}

/*
 * This is a DPC function scheduled by the interrupt handler. It
 * checks to see if there are any read IRPs stuck in the read queue
 * that can now go. It checks this by re-running them through the
 * dev_read function.
 */
static void recheck_pending_irps(KDPC*dpc, void*con, void*arg1, void*arg2)
{
      DEVICE_OBJECT*dev = (DEVICE_OBJECT*)con;
      struct Instance*xsp = (struct Instance*)dev->DeviceExtension;
      KIRQL save_irql;

      KeAcquireSpinLockAtDpcLevel(&xsp->pending_lock);
      InitializeListHead(&xsp->tmp_queue);

      IoAcquireCancelSpinLock(&save_irql);
      while (! IsListEmpty(&xsp->pending_read_irps)) {
	    IRP*irp;
	    LIST_ENTRY*qe = RemoveHeadList(&xsp->pending_read_irps);

	    if (debug_flag & UCR_TRACE_CHAN) {
		  struct ChannelData*xpd = get_channel(irp);
		  printk("recheck read IRP\n");
	    }

	    irp = CONTAINING_RECORD(qe, IRP, Tail.Overlay.ListEntry);
	    IoSetCancelRoutine(irp, 0);

	    if (irp->Cancel)
		  printk("%s:%u: Impossible Cancel on read IRP\n",
			 __FILE__, __LINE__);

	    InsertTailList(&xsp->tmp_queue, qe);
      }
      IoReleaseCancelSpinLock(save_irql);

      while (! IsListEmpty(&xsp->tmp_queue)) {
	    IRP*irp;
	    LIST_ENTRY*qe = RemoveHeadList(&xsp->tmp_queue);
	    irp = CONTAINING_RECORD(qe, IRP, Tail.Overlay.ListEntry);
	    dev_read(dev, irp, TRUE);
      }

      IoAcquireCancelSpinLock(&save_irql);
      while (! IsListEmpty(&xsp->pending_write_irps)) {
	    IRP*irp;
	    LIST_ENTRY*qe = RemoveHeadList(&xsp->pending_write_irps);

	    if (debug_flag & UCR_TRACE_CHAN) {
		  struct ChannelData*xpd = get_channel(irp);
		  printk("recheck write IRP\n");
	    }

	    irp = CONTAINING_RECORD(qe, IRP, Tail.Overlay.ListEntry);
	    IoSetCancelRoutine(irp, 0);

	    if (irp->Cancel)
		  printk("%s:%u: Impossible Cancel on write IRP\n",
			 __FILE__, __LINE__);

	    InsertTailList(&xsp->tmp_queue, qe);
      }
      IoReleaseCancelSpinLock(save_irql);

      while (! IsListEmpty(&xsp->tmp_queue)) {
	    IRP*irp;
	    LIST_ENTRY*qe = RemoveHeadList(&xsp->tmp_queue);
	    irp = CONTAINING_RECORD(qe, IRP, Tail.Overlay.ListEntry);
	    dev_write(dev, irp, TRUE);
      }

      IoAcquireCancelSpinLock(&save_irql);
      while (! IsListEmpty(&xsp->pending_ioctl_irps)) {
	    IRP*irp;
	    LIST_ENTRY*qe = RemoveHeadList(&xsp->pending_ioctl_irps);

	    if (debug_flag & UCR_TRACE_CHAN) {
		  struct ChannelData*xpd = get_channel(irp);
		  printk("recheck ioctl IRP\n");
	    }

	    irp = CONTAINING_RECORD(qe, IRP, Tail.Overlay.ListEntry);
	    IoSetCancelRoutine(irp, 0);

	    if (irp->Cancel)
		  printk("%s:%u: Impossible Cancel on ioctl IRP\n",
			 __FILE__, __LINE__);

	    InsertTailList(&xsp->tmp_queue, qe);
      }
      IoReleaseCancelSpinLock(save_irql);

      while (! IsListEmpty(&xsp->tmp_queue)) {
	    IRP*irp;
	    LIST_ENTRY*qe = RemoveHeadList(&xsp->tmp_queue);
	    irp = CONTAINING_RECORD(qe, IRP, Tail.Overlay.ListEntry);
	    dev_ioctl(dev, irp, TRUE);
      }

      KeReleaseSpinLockFromDpcLevel(&xsp->pending_lock);
}

static BOOLEAN xxirq(PKINTERRUPT irq, void*dev_id)
{
      DEVICE_OBJECT*dev = (DEVICE_OBJECT*)dev_id;
      struct Instance*xsp = (struct Instance*)dev->DeviceExtension;
      int flag = ucr_irq(xsp);
      if (flag) {
	    KeInsertQueueDpc(&xsp->pending_io_dpc, 0, 0);
	    return TRUE;
      } else {
	    return FALSE;
      }
}

#define dev_name_str L"\\Device\\ise"
#define dev_link_str L"\\DosDevices\\ISE"

void unload_device(DRIVER_OBJECT*drv, DEVICE_OBJECT*dev)
{
      struct Instance*xsp = (struct Instance*)dev->DeviceExtension;

      UNICODE_STRING dev_link;
      wchar_t dev_link_buf[sizeof(dev_link_str) + 10];
      UNICODE_STRING number;
      wchar_t number_buf[10];
      PCI_COMMON_CONFIG config;

	/* Turn off bus mastering for the device. */
      HalGetBusData(PCIConfiguration, xsp->bus, xsp->dfn, &config,
		    sizeof config);
      config.Command &= ~(PCI_ENABLE_MEMORY_SPACE|PCI_ENABLE_BUS_MASTER);
      HalSetBusData(PCIConfiguration, xsp->bus, xsp->dfn, &config, 8);

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

      IoDisconnectInterrupt(xsp->irq);
      if (xsp->dev_is_mmapped) MmUnmapIoSpace((void*)xsp->dev, DEVICE_SIZE);

      {
	    CM_RESOURCE_LIST rlist;
	    BOOLEAN over;
	    rlist.Count = 0;
	    IoReportResourceUsage(0, drv, 0, 0, dev, &rlist,
				  sizeof(rlist), TRUE, &over);
      }

      ucr_clear_instance(xsp);
}

/*
 * This method, given the driver object and an instance number,
 * creates a device object.
 */
NTSTATUS create_device(DRIVER_OBJECT*drv, unsigned bus_no,
		       unsigned slot, unsigned num)
{
      unsigned long rc;
      PCI_COMMON_CONFIG config;
      NTSTATUS status;
      struct Instance*xsp;

      unsigned long ivec;
      unsigned char irq;
      KAFFINITY affinity;

      DEVICE_OBJECT*dev;

      UNICODE_STRING dev_name, dev_link;
      wchar_t dev_name_buf[sizeof(dev_name_str) + 10];
      wchar_t dev_link_buf[sizeof(dev_link_str) + 10];
      UNICODE_STRING number;
      wchar_t number_buf[10];

	/* Now get ALL the PCI configuration space information for the
	   device. Use this to figure out stuff about the device that
	   is needed for the later I/O operations. */
      rc = HalGetBusData(PCIConfiguration, bus_no, slot, &config,
			 sizeof config);
      if (rc != sizeof config) return STATUS_UNSUCCESSFUL;


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
      RtlIntegerToUnicodeString(num, 10, &number);

      RtlAppendUnicodeStringToString(&dev_name, &number);


	/* Make the kernel device object and initialize the instance
	   structure with the basics. */
      status = IoCreateDevice(drv, sizeof(struct Instance), &dev_name,
			      FILE_DEVICE_ISE, 0, FALSE, &dev);
      if (! NT_SUCCESS(status)) return status;

      dev->Flags |= DO_BUFFERED_IO;

      xsp = (struct Instance*)dev->DeviceExtension;
      ucr_init_instance(xsp);
      xsp->number = num;
      xsp->bus = bus_no;
      xsp->dfn = slot;

      InitializeListHead(&xsp->pending_read_irps);
      InitializeListHead(&xsp->pending_write_irps);
      InitializeListHead(&xsp->pending_ioctl_irps);
      KeInitializeDpc(&xsp->pending_io_dpc, recheck_pending_irps, dev);
      KeInitializeSpinLock(&xsp->pending_lock);

      for (rc = 0 ;  rc < 16 ;  rc += 1)
	    xsp->fb[rc] = 0;

	/* Make the device accessible to WIN32 by creating a link in
	   \DosDevices to this driver. */
      dev_link.Buffer = dev_link_buf;
      dev_link.MaximumLength = sizeof dev_link_buf;
      dev_link.Length = 0;
      RtlZeroMemory(dev_link_buf, sizeof dev_link_buf);
      RtlAppendUnicodeToString(&dev_link, dev_link_str);

      RtlAppendUnicodeStringToString(&dev_link, &number);
      status = IoCreateSymbolicLink(&dev_link, &dev_name);
      if (! NT_SUCCESS(status)) {
	    printk("Failed to create device symbolic link.\n");
      }

	/* Get information about the interrupt vector. */
      irq = config.u.type0.InterruptLine;
      ivec = HalGetInterruptVector(PCIBus, bus_no, irq, irq,
				   &xsp->irql, &affinity);

	/* Report device resources */
      {
	    BOOLEAN over;
	    CM_RESOURCE_LIST*rlist;
	    CM_PARTIAL_RESOURCE_DESCRIPTOR*dp;
	    unsigned rsize = sizeof(CM_RESOURCE_LIST) +
		  sizeof(CM_PARTIAL_RESOURCE_DESCRIPTOR);
	    rlist = ExAllocatePool(PagedPool, rsize);
	    rlist->Count = 1;

	    rlist->List[0].InterfaceType = PCIBus;
	    rlist->List[0].BusNumber = bus_no;
	    rlist->List[0].PartialResourceList.Count = 2;
	    rlist->List[0].PartialResourceList.Version = 1;
	    rlist->List[0].PartialResourceList.Revision = 1;
	    dp = rlist->List[0].PartialResourceList.PartialDescriptors;
	    dp->Type = CmResourceTypeInterrupt;
	    dp->ShareDisposition = CmResourceShareShared;
	    dp->Flags = CM_RESOURCE_INTERRUPT_LEVEL_SENSITIVE;
	    dp->u.Interrupt.Level = irq;
	    dp->u.Interrupt.Vector = xsp->irql;
	    dp->u.Interrupt.Affinity = affinity;
	    dp += 1;
	    dp->Type = CmResourceTypeMemory;
	    dp->Flags = CM_RESOURCE_MEMORY_READ_WRITE;
	    dp->u.Memory.Start.LowPart = config.u.type0.BaseAddresses[0] & ~0xf;
	    dp->u.Memory.Start.HighPart = 0;
	    dp->u.Memory.Length = DEVICE_SIZE;
		  
	    IoReportResourceUsage(0, drv, 0, 0, dev, rlist, rsize,
				  TRUE, &over);
	    ExFreePool(rlist);
      }

	/* Get the address of the I/O registers for the device. The
	   target specific code uses this values to access its
	   registers with the readl and writel macros. */
      {
	    BOOLEAN rc;
	    unsigned tmp = 0;
	    PHYSICAL_ADDRESS addr, scr;
	    addr.LowPart = config.u.type0.BaseAddresses[0] & ~0xf;
	    addr.HighPart = 0;

	      /* Ask NT for a bus to logical translation. NOTE that
		 NT/ix86 seems to sometimes get totally messed up and
		 will fail to translate an address, even though it is
		 a 1-to-1 map. BUG! Cope by guessing. */
	    rc = HalTranslateBusAddress(PCIBus, bus_no, addr, &tmp, &scr);
	    if (rc == FALSE) {
		  IO_ERROR_LOG_PACKET*el =
			IoAllocateErrorLogEntry(dev,
						sizeof(IO_ERROR_LOG_PACKET));
		  el->MajorFunctionCode = 0;
		  el->RetryCount = 0;
		  el->ErrorCode = UCR_HTBA_BUG;
		  el->UniqueErrorValue = 0;
		  el->FinalStatus = 0;
		  el->SequenceNumber = 0;
		  el->IoControlCode = 0;
		  el->DumpDataSize = sizeof(unsigned long);
		  el->DumpData[0] = addr.LowPart;
		  el->NumberOfStrings = 0;
		  IoWriteErrorLogEntry(el);
		  tmp = 0;
		  scr = addr;
	    } else if (debug_flag & (UCR_TRACE_CHAN|UCR_TRACE_PROTO)) {
		  printk("Translated %d/%p to logical %p:%p\n",
			 bus_no, addr.LowPart, scr.HighPart, scr.LowPart);
	    }
	    if (tmp == 0) {
		  xsp->dev = (__u32)MmMapIoSpace(scr, DEVICE_SIZE, FALSE);
		  xsp->dev_is_mmapped = 1;
	    } else {
		  xsp->dev = scr.LowPart;
		  xsp->dev_is_mmapped = 0;
	    }
      }

	/* Connect the interrupt to the interrupt handler. */
      status = IoConnectInterrupt(&xsp->irq, xxirq, dev, 0, ivec,
				  xsp->irql, xsp->irql, LevelSensitive,
				  TRUE, affinity, FALSE);
      printk("ISE%u IRQ=%u, ivec=%u, irql=%u, affinity=0x%x\n",
	     num, irq, ivec, xsp->irql, affinity);

      if (status != STATUS_SUCCESS) switch (rc) {
	  case STATUS_INSUFFICIENT_RESOURCES:
	    printk("IoConnectInterrupt reported insufficient resources!\n");
	    break;
	  default:
	    printk("IoConnectInterrupt returned 0x%x\n", status);
	    break;
      }

	/* Turn on bus mastering for the device. */
      config.Command |= PCI_ENABLE_MEMORY_SPACE|PCI_ENABLE_BUS_MASTER;
      rc = HalSetBusData(PCIConfiguration, bus_no, slot, &config, 8);

	/* Finally, create a matchine control device. */
      create_devx(drv, xsp);

      return STATUS_SUCCESS;
}


/*
 * $Log$
 * Revision 1.3  2001/07/12 20:31:05  steve
 *  Support UCRX_TIMEOUT_FORCE
 *
 * Revision 1.2  2001/04/03 01:56:05  steve
 *  Simplify the code path for pending operations, and
 *  use buffered I/O instead of direct.
 *
 * Revision 1.1  2001/03/05 20:11:40  steve
 *  Add NT4 driver to ISE source tree.
 */

