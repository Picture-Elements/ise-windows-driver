/*
 * Copyright (c) 2001 Picture Elements, Inc.
 *    Stephen Williams (steve@picturel.com)
 *
 * $Id$
 */

# include  "ise_sys.h"


static void dev_write_2(struct instance_t*xsp, IRP*irp);

NTSTATUS dev_write(DEVICE_OBJECT*dev, IRP*irp)
{
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct instance_t*xsp = (struct instance_t*)dev->DeviceExtension;
      struct channel_t*xpd = get_channel(irp);

	/* Steal the ByteOffset member as a progress pointer for the
	   actual write operation. This allows me to span function
	   calls using the IRP for context. */
      stp->Parameters.Write.ByteOffset.LowPart = 0;

      if (debug_flag & UCR_TRACE_CHAN)
	    printk("ise%u.%u: write %u bytes\n",
		   xsp->id, xpd->channel, stp->Parameters.Write.Length);


      dev_write_2(xsp, irp);
      return irp->IoStatus.Status;
}

static void dev_write_2(struct instance_t*xsp, IRP*irp)
{
      IO_STACK_LOCATION*stp = IoGetCurrentIrpStackLocation(irp);
      struct channel_t*xpd = get_channel(irp);
      unsigned char*bytes;
      unsigned long count, tcount;

      bytes  = irp->AssociatedIrp.SystemBuffer;
      count  = stp->Parameters.Write.Length;
      tcount = count;

      bytes  += stp->Parameters.Write.ByteOffset.LowPart;
      tcount -= stp->Parameters.Write.ByteOffset.LowPart;

      while (tcount > 0) {
	    unsigned trans = tcount;
	    unsigned idx;
	    unsigned char*buf;

	    idx = xpd->table->next_out_idx;
	    buf = xpd->out[idx].ptr;

	    if ((xpd->out_off + trans) > xpd->table->out[idx].count)
		  trans = xpd->table->out[idx].count - xpd->out_off;

	    if (debug_flag & UCR_TRACE_CHAN)
		  printk("ise%u.%u (d): write %u bytes to out_ptr=%u"
			 "(buf=0x%x) offset=%u\n", xsp->id, xpd->channel,
			 trans, idx, buf, xpd->out_off);

	    RtlCopyMemory(buf + xpd->out_off, bytes, trans);

	    xpd->out_off += trans;
	    tcount -= trans;
	    bytes  += trans;
	    stp->Parameters.Write.ByteOffset.LowPart += trans;

	      /* If the current buffer is full, then send it to the
		 ISE board for reading. */
	    if (xpd->out_off == xpd->table->out[idx].count) {

		  flush_channel(xsp, irp, dev_write_2);
		  return;
	    }
      }

      if (debug_flag & UCR_TRACE_CHAN)
	    printk("ise%u.%u (d): write completed %d bytes\n",
		   xsp->id, xpd->channel, count-tcount);

      irp->IoStatus.Status = STATUS_SUCCESS;
      irp->IoStatus.Information = stp->Parameters.Write.ByteOffset.LowPart;
      IoCompleteRequest(irp, IO_NO_INCREMENT);
}


/*
 * $Log$
 * Revision 1.1  2001/07/26 00:31:30  steve
 *  Windows 2000 driver.
 *
 */

