/*
 * Copyright (c) 2001 Picture Elements, Inc.
 *    Stephen Williams (steve@picturel.com)
 *
 * $Id$
 */

# include  "ise_sys.h"

/*
 * This source file includes the convenience functions that diddle
 * bits on the hardware. There should be no WRITE_ or READ_REGISTER
 * calls anywhere else. These functions define the hardware access.
 */


void dev_init_hardware(struct instance_t*xsp)
{
      __u32 tmp;

	/* OUTBELLS = 0x0fffffff */
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x2c), 0x0fffffff);
	/* INBOUND0 = 0 */
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x10), 0);
	/* INBOUND1 = 0 */
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x14), 0);
	/* OUTBOUNT0 = 0 */
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x18), 0);

	/* OIMR = 0xfb (Enable only doorbells) */
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x34), 0xfb);
      tmp = READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x34));
      if ((tmp&0x7f) != 0x7b) {
	    printk("ise%u: error: OIMR wont stick. Wrote 0xfb, OIMR=%x\n",
		   xsp->id, tmp);
	    WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x34), 0xfb);
	    WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x34), 0xfb);
	    tmp = READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x34));
	    printk("ise%u:      : Tried harder. Wrote 0xfb, OIMR=%x\n",
		   xsp->id, tmp);
      }
}

void dev_clear_hardware(struct instance_t*xsp)
{
      __u32 tmp;

	/* OIMR = 0xfb (Enable only doorbells) */
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x34), 0xfb);
      tmp = READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x34));
      if ((tmp&0x7f) != 0x7b) {
	    printk("ise%u: error: OIMR wont stick. Wrote 0xfb, got %x\n",
		   xsp->id, tmp);
      }

	/* INBOUND0 = 0 */
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x10), 0);
	/* INBOUND1 = 0 */
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x14), 0);
	/* OUTBOUNT0 = 0 */
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x18), 0);
}

void dev_set_bells(struct instance_t*xsp, unsigned long mask)
{
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x20),
			   mask & 0x7fffffffUL);
}

/*
 * Write the pointer into the root table base word. Read it back again
 * to make sure the write gets through the PCI bus and into the
 * device. While we're at it, compare that readout value to triple-
 * check that all went properly.
 */
void dev_set_root_table_base(struct instance_t*xsp, __u32 value)
{
      __u32 tmp;
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x10), value);

      tmp = READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x10));
      if (tmp != value) {
	    printk("ise%u: error: root pointer wont stick."
		   " Wrote %x, got %x.\n", xsp->id, value, tmp);
      }
}

void dev_set_root_table_resp(struct instance_t*xsp, __u32 value)
{
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x18), value);
}

unsigned long dev_get_root_table_resp(struct instance_t*xsp)
{
      return READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x18));
}

/*
 * The status resp and value registers are the OMR1 and IMR1 regisers.
 */
__u32 dev_get_status_resp(struct instance_t*xsp)
{
	/* return OMR1 */
      return READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x1c));
}

void dev_set_status_value(struct instance_t*xsp, __u32 value)
{
	/* IMR1 = value; */
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x14), value);
}

/*
 * Return the mask of bells that are currently set, and clear them in
 * the hardware as I do it. Note that if the interrupt is blocked,
 * then pretend no bells are set. This allows the IRQ to use the mask
 * as a synchronization trick, even when the hardware IRQ is shared.
 */
unsigned long dev_get_bells(struct instance_t*xsp)
{
      unsigned long mask;

      if (4 & READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x34)))
	    return 0;

      mask = READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x2c));
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x2c), mask);

      return mask & 0x0fffffff;
}

unsigned long dev_mask_irqs(struct instance_t*xsp)
{
      unsigned long mask =
	    READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x34));

      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x34), mask|0x0f);

      return mask;
}

void dev_unmask_irqs(struct instance_t*xsp, unsigned long mask)
{
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x34), mask);
}


/*
 * $Log$
 * Revision 1.4  2002/05/13 20:07:52  steve
 *  More diagnostic detail, and check registers.
 *
 * Revision 1.3  2001/10/25 23:46:41  steve
 *  Mask all but doorbell interrupts.
 *
 * Revision 1.2  2001/08/03 17:39:41  steve
 *  Use status method to run programs.
 *
 * Revision 1.1  2001/07/26 00:31:30  steve
 *  Windows 2000 driver.
 *
 */

