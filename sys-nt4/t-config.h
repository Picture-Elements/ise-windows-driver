#ifndef __t_ise_H
#define __t_ise_H
/*
 * Copyright (c) 1998 Picture Elements, Inc.
 *    Stephen Williams <steve@picturel.com>
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
 *
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
 * The ISE board has on it a PCI-PCI bridge (function unit 0) and an
 * Address Translation Unit (function unit 1). It is the latter that
 * we are interested in, as it has the registers for communicating
 * with the i960 on the board. We ignore the bridge, as the BIOS takes
 * care of it for the most part.
 */

/*
 * These are the registers that I use from the ISE board. The register
 * names are relative the board, so INBELLS are bells into the board,
 * etc.
 */
enum ATUOffsets {
      INBOUND0 = 0x10,
      INBOUND1 = 0x14,
      OUTBOUND0 = 0x18,
      OUTBOUND1 = 0x1c,

      INBELLS = 0x20,
      OUTBELLS = 0x2c,
      OISR = 0x30, /* Outbound Interrupt Status Register */
      OIMR = 0x34, /* Outbound Interrupt Mask Register */

      DEVICE_SIZE = 0x1000
};



/*
 * This method is called to initialize the hardware in preparation for
 * all the other calls. This should clear any interrupts, then enable
 * only the OMR0/1 interrupts.
 */

static __inline__ void dev_init_hardware(unsigned long dev)
{
      writel(0x0fffffff, dev+OUTBELLS);
      writel(0, dev+INBOUND0);
      writel(0, dev+INBOUND1);
      writel(0, dev+OUTBOUND0);
      writel(0xfb, dev+OIMR);
}

/*
 * This turns off interrupts, so the board can no longer abuse the
 * host. It need not totally reset the board.
 */
static __inline__ void dev_clear_hardware(unsigned long dev)
{
      writel(0xff, dev+OIMR);
      writel(0, dev+INBOUND0);
      writel(0, dev+OUTBOUND0);
}


/*
 * The driver uses i960Rx message registers to send values. Send the
 * root table to the ISE board by writing to the INBOUND0 register the
 * value. The board will respond by copying the value to the OUTBOUND0
 * registers.
 */

static __inline__ void dev_set_root_table_base(unsigned long dev, __u32 value)
{     writel(value, dev+INBOUND0);
}

static __inline__ void dev_set_root_table_resp(unsigned long dev, __u32 value)
{     writel(0, dev+OUTBOUND0);
}

static __inline__ void dev_set_status_value(unsigned long dev, __u32 value)
{
      writel(value, dev+INBOUND1);
}

static __inline__ __u32 dev_get_root_table_ack(unsigned long dev)
{
      return readl(dev+OUTBOUND0);
}

static __inline__ __u32 dev_get_status_value(unsigned long dev)
{
      return readl(dev+OUTBOUND1);
}

/*
 * This method sends the bell interrupts that are set in the
 * mask. This is done by writing into the out bells register, which is
 * different from the in bells. Note that on the i960RP, there are only
 * 31 doorbells, the highest bit is a NMI.
 */
static __inline__ void dev_set_bells(unsigned long dev,
				     unsigned long mask)
{
      writel(mask & 0x7fffffff, dev+INBELLS);
}


/*
 * This method gets the bits of the currently set input bells, and
 * leaves those bits cleared in the bell register. That way, repeated
 * calls to dev_get_bells() will get a 1 only for bells that have been
 * set since the last call. The i960RP can only send 28 bells.
 *
 * In a shared interrupt environment, it is posible for a sibling
 * interrupt to cause the ISE interrupt handler to be called. If I see
 * that the doorbell interrupts are masked, then pretend that no
 * doorbells happen. The real interrupt will occur when the mask is
 * turned off.
 */
static __inline__ unsigned long dev_get_bells(unsigned long dev)
{
      if (readl(dev+OIMR) & 4) return 0;
      else {
	    unsigned long mask = readl(dev+OUTBELLS);
	    writel(mask, dev+OUTBELLS);
	    return mask & 0x0fffffff;
      }
}


/*
 * the dev_mask_irqs function masks the doorbell interrupts and
 * returns the current (before the mask) flag bits. The caller is
 * expected to pass that result back into the dev_unmask_irqs function
 * to restore interrupts.
 */
static __inline__ unsigned long dev_mask_irqs(unsigned long dev)
{
      unsigned long result = readl(dev+OIMR);
      writel(result | 0x0f, dev+OIMR);
      return result;
}

static __inline__ void dev_unmask_irqs(unsigned long dev, unsigned long mask)
{
      writel(mask, dev+OIMR);
}


#if 1
# define VENDOR_ID 0x12c5
# define DEVICE_ID 0x007f
#else
# define VENDOR_ID 0x8086
# define DEVICE_ID 0x1960
#endif
# define DEVICE_NAME "ise"
# define DEVICE_LONG_NAME "Picture Elements -- ISE"

#endif
/*
 * $Log$
 * Revision 1.3  2002/07/02 21:55:04  steve
 *  Detect and use status based SSE startup.
 *
 * Revision 1.2  2002/06/28 23:13:32  steve
 *  Slightly more thorough init.
 *
 * Revision 1.1  2001/03/05 20:11:40  steve
 *  Add NT4 driver to ISE source tree.
 *
 * Revision 1.3  1999/06/14 20:28:48  steve
 *  Fix improper interrupts in shared irq environment.
 *
 * Revision 1.2  1998/05/30 01:49:52  steve
 *  Handle startup after reset.
 *
 * Revision 1.1  1998/05/26 16:16:34  steve
 *  New channel protocol
 *
 */
