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
	/* OUTBELLS = 0x0fffffff */
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x2c), 0x0fffffff);
	/* INBOUND0 = 0 */
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x10), 0);
	/* INBOUND1 = 0 */
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x14), 0);
	/* OUTBOUNT0 = 0 */
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x18), 0);
	/* OIMR = 00xfb */
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x34), 0xfb);
}

void dev_clear_hardware(struct instance_t*xsp)
{
	/* OIMR = 0x00 */
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x34), 0x00);
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

void dev_set_root_table_base(struct instance_t*xsp, __u32 value)
{
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x10), value);
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
 * Revision 1.1  2001/07/26 00:31:30  steve
 *  Windows 2000 driver.
 *
 */

