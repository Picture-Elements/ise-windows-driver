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
/*
 * The JSE uses an Intel 21555 bridge to provide the interface. In
 * particular, the scratchpad and doorbell registers are used to
 * communicate with the processor on the other side.
 *
 *    ScratchPad0 (0xa8)  - root table base
 *    ScratchBad1 (0xac)  - root table response
 *    ScratchPad2 (0xb0)  - status value
 *    ScratchBad3 (0xb4)  - Status register response
 *
 * The bridge has only 16 bits of doorbells for each direction. They
 * are mapped from the ucr bellmasks like so:
 *
 *    ROOT_TABLE  0x00000001    --> 0x0001
 *    STATUS      0x00000002    --> 0x0002
 *    CHANGE      0x00000004    --> 0x0004
 *    RESTART     0x40000000    --> 0x4000
 *
 */


static void jse_init_hardware(struct instance_t*xsp)
{
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xa8), 0x0000);
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xac), 0x0000);
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xb0), 0x0000);
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xa0), 0xffff);
}

static void jse_clear_hardware(struct instance_t*xsp)
{
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xa8), 0x0000);
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xac), 0x0000);
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xb0), 0x0000);
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xa4), 0xffff);
}

static void jse_set_bells(struct instance_t*xsp, unsigned long mask)
{
      unsigned short use_mask = (unsigned short) mask & 0x000000ff;
      if (mask & 0x40000000) use_mask |= 0x4000;

      WRITE_REGISTER_USHORT((USHORT*)((char*)xsp->bar0 + 0x9e), use_mask);
}

/*
 * Write the pointer into the root table base word. Read it back again
 * to make sure the write gets through the PCI bus and into the
 * device. While we're at it, compare that readout value to triple-
 * check that all went properly.
 */
static void jse_set_root_table_base(struct instance_t*xsp, __u32 value)
{
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xa8), value);
}

static void jse_set_root_table_resp(struct instance_t*xsp, __u32 value)
{
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xac), value);
}

static unsigned long jse_get_root_table_resp(struct instance_t*xsp)
{
      return READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xac));
}

/*
 * The status resp and value registers are the OMR1 and IMR1 regisers.
 */
static __u32 jse_get_status_resp(struct instance_t*xsp)
{
      return READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xb4));
}

static void jse_set_status_value(struct instance_t*xsp, __u32 value)
{
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xb0), value);
}

/*
 * Return the mask of bells that are currently set, and clear them in
 * the hardware as I do it. Note that if the interrupt is blocked,
 * then pretend no bells are set. This allows the IRQ to use the mask
 * as a synchronization trick, even when the hardware IRQ is shared.
 */
static unsigned long jse_get_bells(struct instance_t*xsp)
{
      unsigned long result;
	/* mask = Primary IRQ mask */
      USHORT mask = READ_REGISTER_USHORT((USHORT*)((char*)xsp->bar0+0xa0));
	/* bell = Primary IRQ */
      USHORT bell = READ_REGISTER_USHORT((USHORT*)((char*)xsp->bar0+0x98));

	/* Ignore masked bits. */
      bell = bell & ~mask;

	/* Primary Clear IRQ = interrupting bells */
      WRITE_REGISTER_USHORT((USHORT*)((char*)xsp->bar0 + 0x98), bell);

      result = bell & 0x00ff;
      if (bell & 0x4000) result |= 0x40000000;

      return result;
}

static unsigned long jse_mask_irqs(struct instance_t*xsp)
{
	/* Get the current mask status from the Primary IRQ mask */
      unsigned long result
	    = READ_REGISTER_USHORT((USHORT*)((char*)xsp->bar0 + 0xa4));

      WRITE_REGISTER_USHORT((USHORT*)((char*)xsp->bar0 + 0xa4), 0xffff);

      return result;
}

static void jse_unmask_irqs(struct instance_t*xsp, unsigned long mask)
{
	/* Set bits that need to be set */
      WRITE_REGISTER_USHORT((USHORT*)((char*)xsp->bar0 + 0xa4),
			    (USHORT) mask&0xffff);
	/* Clear bits that need to be cleared */
      WRITE_REGISTER_USHORT((USHORT*)((char*)xsp->bar0 + 0xa0),
			    (USHORT) (~mask)&0xffff);
}

static void jse_diagnose0_dump(struct instance_t*xsp)
{
}

static void jse_diagnose1_dump(struct instance_t*xsp)
{
      if (xsp->bar0_size > 0) {
	    unsigned long scratch0, scratch1, scratch2, scratch3;
	    scratch0 =  READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xa8));
	    scratch1 =  READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xac));
	    scratch2 =  READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xb0));
	    scratch3 =  READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xb4));

	    printk("ise%u: SCRATCH0=0x%x SCRATCH1=0x%x\n",
		   xsp->id, scratch0, scratch1);
	    printk("ise%u: SCRATCH2=0x%x SCRATCH3=0x%x\n",
		   xsp->id, scratch2, scratch3);

	    printk("ise%u: IRQ=0x%x, MASK=0x%x\n", xsp->id,
		   READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0+0x98)),
		   READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0+0xa0)));

      } else {
	    printk("ise%u: <** Device Not Mapped **>\n", xsp->id);
      }
}

const struct ise_ops_tab jse_operations = {
      "JSE",
      jse_init_hardware,
      jse_clear_hardware,
      jse_mask_irqs,
      jse_unmask_irqs,
      jse_set_root_table_base,
      jse_set_root_table_resp,
      jse_get_root_table_resp,
      jse_get_status_resp,
      jse_set_status_value,

      jse_set_bells,
      jse_get_bells,

      jse_diagnose0_dump,
      jse_diagnose1_dump
};


/*
 * $Log$
 * Revision 1.1  2004/07/15 04:19:26  steve
 *  Extend to support JSE boards.
 *
 */

