/*
 * Copyright (c) 2001 Picture Elements, Inc.
 *    Stephen Williams (steve@picturel.com)
 *
 * $Id$
 */

# include  "ise_sys.h"
# include  <stdarg.h>

/*
 * The ISECONS device is a pseudo device that is a hook into the
 * driver proper for debug use. There is only one ise console, no
 * matter how many ISE boards are installed.
 */


# define LOG_BUF_SIZE 8192

/* The ptr is the location of the next byte to be read out, and fill
   is the number of bytes that are available for reading. */
struct ConsoleExt {
      char*log_buf;
      unsigned ptr;
      unsigned fill;
};

static struct ConsoleExt*cons = 0;
static DEVICE_OBJECT*devp = 0;

unsigned long debug_flag = 0x0000;

/*
 * The ISECONS virtual device provides access to the debug log space
 * for the driver. Parts of the driver write into it with the printk
 * function, and the application reads out of it by reading from the
 * ISECONS device.
 *
 * There are two external interfaces into the local data structures:
 * the printk function and the read dispatch function. The printk
 * function writes data into the log buffer, and the read pulls data
 * out.
 */

static void send_data_to_irp(struct ConsoleExt*xcp, IRP*irp)
{
	/* If there are bytes in the log buffer, read them out
	   and complete the READ immediately, even if this is a
	   partial read. */
      char*usr;
      IO_STACK_LOCATION*sp = IoGetCurrentIrpStackLocation(irp);
      unsigned tcount = sp->Parameters.Read.Length;
      if (tcount > xcp->fill) tcount = xcp->fill;

      irp->IoStatus.Status = STATUS_SUCCESS;
      irp->IoStatus.Information = tcount;

      usr = (char*) irp->AssociatedIrp.SystemBuffer;

      while (tcount > 0) {
	    unsigned cnt = tcount;
	    if ((xcp->ptr + cnt) > LOG_BUF_SIZE)
		  cnt = LOG_BUF_SIZE - xcp->ptr;

	    RtlCopyMemory(usr, xcp->log_buf+xcp->ptr, cnt);
	    xcp->ptr = (xcp->ptr + cnt) % LOG_BUF_SIZE;
	    xcp->fill -= cnt;
	    usr += cnt;
	    tcount -= cnt;
      }

      IoCompleteRequest(irp, IO_NO_INCREMENT);
}

/*
 * The user-mode program, the console reader program, interfaces with
 * the driver at this point. I read bytes out of the buffer if there
 * are any, returning partial results. If there are no bytes at all,
 * then it is OK to block the read.
 */
NTSTATUS cons_read(DEVICE_OBJECT*dev, IRP*irp)
{
      KIRQL flags;


      if (cons->fill > 0) {
	    DbgPrint("isecons: Read from buffer of %u bytes\n", cons->fill);
	    KeRaiseIrql(DISPATCH_LEVEL, &flags);
	    send_data_to_irp(cons, irp);
	    KeLowerIrql(flags);
	    return STATUS_SUCCESS;

      } else {
	    DbgPrint("isecons: Read EOF from console\n", cons->fill);
	    irp->IoStatus.Status = STATUS_END_OF_FILE;
	    irp->IoStatus.Information = 0;
	    IoCompleteRequest(irp, IO_NO_INCREMENT);
	    return STATUS_END_OF_FILE;
      }
}


static void write_to_buffer(const char*data, unsigned ndata)
{
      unsigned cur;

      if (cons == 0) return;
      if (ndata == 0) return;

	/* If this message alone will overflow the buffer, take only
	   the tail part that will fit. */

      if (ndata > LOG_BUF_SIZE) {
	    data += (ndata - LOG_BUF_SIZE);
	    ndata = LOG_BUF_SIZE;
      }

	/* Remove enough from the front of the buffer that this
	   message will fit. */

      if ((cons->fill+ndata) > LOG_BUF_SIZE) {
	    unsigned remove = (cons->fill+ndata) - LOG_BUF_SIZE;
	    cons->ptr = (cons->ptr + remove) % LOG_BUF_SIZE;
	    cons->fill -= remove;
      }

      while (ndata > 0) {
	    unsigned cur = (cons->ptr + cons->fill) % LOG_BUF_SIZE;
	    unsigned tcount = ndata;

	      /* Watch for wrapping... */
	    if ((cur + tcount) > LOG_BUF_SIZE)
		  tcount = LOG_BUF_SIZE - cur;

	      /* Do the somewhat constrained write into the buffer. */
	    RtlCopyMemory(cons->log_buf+cur, data, tcount);
	    cons->fill += tcount;
	    data += tcount;
	    ndata -= tcount;
      }
}

/*
 * This is the function that performs the actual formatting and
 * generates bytes that go into the buffer. It is also the entry into
 * the console log from the other parts of the driver.
 */

static const char hexchar[] = "0123456789ABCDEF";

void printk(const char*fmt, ...)
{
      char fmtbuf[128];
      char*fmtout = fmtbuf;
      const char*cp = fmt;

      va_list args;

	/* Sorry, no printing in interrupt level. */
	//if (KeGetCurrentIrql() > DISPATCH_LEVEL) return;

      va_start(args, fmt);

      while (*cp) {
	    char c = *cp++;
	    if (c == '%') {
		  c = *cp++;
		  switch (c) {
		      case '%':
			*fmtout++ = '%';
			break;

		      case 'd': {
			    long val = va_arg(args, long);
			    char st[16];
			    unsigned stp = 0;
			    int sign;
			    if (val >= 0) {
				  sign = 1;
			    } else {
				  sign = -1;
				  val = -val;
			    }
			    while (val) {
				  st[stp++] = hexchar[val%10];
				  val /= 10;
			    }
			    if (stp == 0) st[stp++] = '0';
			    if (sign < 0) st[stp++] = '-';
			    while (stp > 0) *fmtout++ = st[--stp];
			    break;
		      }

			  /* Dump pointers in 8 HEX characters */
		      case 'p': {
			    unsigned long val = va_arg(args, unsigned long);
			    char st[2*sizeof val];
			    unsigned stp = 0;
			    int idx;
			    for (idx = 0 ;  idx < sizeof st ;  idx += 1) {
				  st[stp++] = hexchar[val%16];
				  val /= 16;
			    }
			    while (stp > 0) *fmtout++ = st[--stp];
			    break;
		      }

		      case 'u': {
			    unsigned long val = va_arg(args, unsigned long);
			    char st[3*sizeof val];
			    unsigned stp = 0;
			    while (val) {
				  st[stp++] = hexchar[val%10];
				  val /= 10;
			    }
			    if (stp == 0) st[stp++] = '0';
			    while (stp > 0) *fmtout++ = st[--stp];
			    break;
		      }

		      case 's': {
			    const char*val = va_arg(args, const char*);
			    while (*val)
				  *fmtout++ = *val++;
			    break;
		      }

		      case 'x': {
			    unsigned long val = va_arg(args, unsigned long);
			    char st[2*sizeof val];
			    unsigned stp = 0;
			    while (val) {
				  st[stp++] = hexchar[val%16];
				  val /= 16;
			    }
			    if (stp == 0) st[stp++] = '0';
			    while (stp > 0) *fmtout++ = st[--stp];
			    break;
		      }

		      default:
			*fmtout++ = '%';
			*fmtout++ = c;
		  }
	    } else {
		  *fmtout++ = c;
	    }
      }

      va_end(args);

      write_to_buffer(fmtbuf, fmtout-fmtbuf);
}

void unload_console(DRIVER_OBJECT*drv)
{
      UNICODE_STRING link_name;
      struct ConsoleExt*xcp = (struct ConsoleExt*)devp->DeviceExtension;

      DbgPrint("ise: unload_console called\n");

      RtlInitUnicodeString(&link_name, L"\\DosDevices\\ISECONS");
      IoDeleteSymbolicLink(&link_name);

      cons = 0;
      ExFreePool(xcp->log_buf);
      xcp->log_buf = 0;
}

NTSTATUS create_console(DRIVER_OBJECT*drv)
{
      NTSTATUS status;
      DEVICE_OBJECT*dev;
      struct ConsoleExt*xcp;

      UNICODE_STRING dev_name, link_name;

      cons = 0;

      RtlInitUnicodeString(&dev_name, L"\\Device\\isecons");
      status = IoCreateDevice(drv, sizeof(struct ConsoleExt),
			      &dev_name, FILE_DEVICE_ISE_CONS, 0,
			      TRUE, &dev);
      if (! NT_SUCCESS(status)) return status;

      dev->Flags |= DO_BUFFERED_IO;

      xcp = (struct ConsoleExt*)dev->DeviceExtension;

      xcp->log_buf = ExAllocatePool(NonPagedPool, LOG_BUF_SIZE);
      if (xcp->log_buf == 0) {
	    return STATUS_NO_MEMORY;
      }

      xcp->ptr = 0;
      xcp->fill = 0;

      cons = xcp;
      devp = dev;

      RtlInitUnicodeString(&link_name, L"\\DosDevices\\ISECONS");
      IoCreateSymbolicLink(&link_name, &dev_name);

      DbgPrint("ise: ISE console created.\n");
      printk("isecons: Console created.\n");

      return STATUS_SUCCESS;
}

/*
 * $Log$
 * Revision 1.3  2001/09/06 01:25:36  steve
 *  Default debug flags off.
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

