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


static void ise_init_hardware(struct instance_t*xsp)
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

static void ise_clear_hardware(struct instance_t*xsp)
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

static void ise_set_bells(struct instance_t*xsp, unsigned long mask)
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
static void ise_set_root_table_base(struct instance_t*xsp, __u32 value)
{
      __u32 tmp;
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x10), value);

      tmp = READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x10));
      if (tmp != value) {
	    printk("ise%u: error: root pointer wont stick."
		   " Wrote %x, got %x.\n", xsp->id, value, tmp);
      }
}

static void ise_set_root_table_resp(struct instance_t*xsp, __u32 value)
{
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x18), value);
}

static unsigned long ise_get_root_table_resp(struct instance_t*xsp)
{
      return READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x18));
}

/*
 * The status resp and value registers are the OMR1 and IMR1 regisers.
 */
static __u32 ise_get_status_resp(struct instance_t*xsp)
{
	/* return OMR1 */
      return READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x1c));
}

static void ise_set_status_value(struct instance_t*xsp, __u32 value)
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
static unsigned long ise_get_bells(struct instance_t*xsp)
{
      unsigned long mask;

      if (4 & READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x34)))
	    return 0;

      mask = READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x2c));
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x2c), mask);

      return mask & 0x0fffffff;
}

static unsigned long ise_mask_irqs(struct instance_t*xsp)
{
      unsigned long mask =
	    READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x34));

      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x34), mask|0x0f);

      return mask;
}

static void ise_unmask_irqs(struct instance_t*xsp, unsigned long mask)
{
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x34), mask);
}

static void ise_diagnose0_dump(struct instance_t*xsp)
{
      unsigned long magic;
      unsigned lineno, cnt, idx;
      char msg[128];

      magic = READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x50));
      if (magic != 0xab0440ba) {
	    printk("isex%u: No abort magic number.\n", xsp->id);
	    return;
      }

      lineno = READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x54));
      cnt = READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x58));
      if (cnt >= sizeof msg) {
	    printk("isex%u: Invalid count: %u\n", xsp->id, cnt);
	    return;
      }

      for (idx = 0 ;  idx < cnt ;  idx += 1) {
	    msg[idx] =
		 READ_REGISTER_UCHAR((unsigned char*)xsp->bar0 + 0x5c + idx);
      }

      msg[cnt] = 0;

      printk("isex%u: target panic msg: %s\n", xsp->id, msg);
      printk("isex%u: target panic code: %u (0x%x)\n", xsp->id,
	     lineno, lineno);
}

static void ise_diagnose1_dump(struct instance_t*xsp)
{
      if (xsp->bar0_size > 0) {
	    printk("ise%u: root_table = (base=%x resp=%x)\n", xsp->id,
		   READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x10)),
		   READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x18)));

	    printk("ise%u: OIMR=%x, OISR=%x, ODR=%x\n", xsp->id,
		   READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x34)),
		   READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x30)),
		   READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x2c)));

	    printk("ise%u: IIMR=%x, IISR=%x, IDR=%x\n", xsp->id,
		   READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x28)),
		   READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x24)),
		   READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x20)));

	    printk("ise%u: OMR0=%x, OMR1=%x\n", xsp->id,
		   READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x18)),
		   READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x1c)));

	    printk("ise%u: IMR0=%x, IMR1=%x\n", xsp->id,
		   READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x10)),
		   READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x14)));

      } else {
	    printk("ise%u: <** Device Not Mapped **>\n", xsp->id);
      }
}

const struct ise_ops_tab ise_operations = {
      "ISE/SSE",
      0, /* No flags */
      ise_init_hardware,
      ise_clear_hardware,
      ise_mask_irqs,
      ise_unmask_irqs,
      ise_set_root_table_base,
      ise_set_root_table_resp,
      ise_get_root_table_resp,
      ise_get_status_resp,
      ise_set_status_value,

      ise_set_bells,
      ise_get_bells,

      ise_diagnose0_dump,
      ise_diagnose1_dump
};


/*
 * $Log$
 * Revision 1.2  2009/04/03 18:21:17  steve
 *  Implement frame64 support in Windows driver.
 *  More robust error handling around root tables.
 *  Keep a deeper root standby list to prevent leaks.
 *
 * Revision 1.1  2004/07/15 04:19:26  steve
 *  Extend to support JSE boards.
 *
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

