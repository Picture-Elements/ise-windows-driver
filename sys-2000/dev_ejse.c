/*
 * Copyright (c) 2008 Picture Elements, Inc.
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
 *
 *    Mailbox0 (0xf0000)  - root table base
 *    Mailbox1 (0xf0004)  - root table response
 *    Mailbox2 (0xf0008)  - status value
 *    Mailbox3 (0xf000c)  - Status register response
 *
 * The bridge has only 16 bits of doorbells for each direction. They
 * are mapped from the ucr bellmasks like so:
 *
 *    ROOT_TABLE  0x00000001    --> 0x01
 *    STATUS      0x00000002    --> 0x02
 *    CHANGE      0x00000004    --> 0x04
 *    RESTART     0x40000000    --> 0x80
 *
 */


static void ejse_init_hardware(struct instance_t*xsp)
{
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xf0000), 0x0000);
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xf0004), 0x0000);
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xf0008), 0x0000);
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x1000c), 0xff00);
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x0000c), 0x0001);
}

static void ejse_clear_hardware(struct instance_t*xsp)
{
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xf0000), 0x0000);
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xf0004), 0x0000);
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xf0008), 0x0000);
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x00008), 0x0001);
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x10008), 0xff00);
}

static void ejse_set_bells(struct instance_t*xsp, unsigned long mask)
{
      unsigned short use_mask = (unsigned short) mask & 0x0000007f;
      if (mask & 0x40000000) use_mask |= 0x0080;

      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x1000c), use_mask<<16);
}

/*
 * Write the pointer into the root table base word. Read it back again
 * to make sure the write gets through the PCI bus and into the
 * device. While we're at it, compare that readout value to triple-
 * check that all went properly.
 */
static void ejse_set_root_table_base(struct instance_t*xsp, __u32 value)
{
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xf0000), value);
}

static void ejse_set_root_table_resp(struct instance_t*xsp, __u32 value)
{
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xf0004), value);
}

static unsigned long ejse_get_root_table_resp(struct instance_t*xsp)
{
      return READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xf0004));
}

/*
 * The status resp and value registers are the OMR1 and IMR1 regisers.
 */
static __u32 ejse_get_status_resp(struct instance_t*xsp)
{
      return READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xf000c));
}

static void ejse_set_status_value(struct instance_t*xsp, __u32 value)
{
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0xf0008), value);
}

/*
 * Return the mask of bells that are currently set, and clear them in
 * the hardware as I do it. Note that if the interrupt is blocked,
 * then pretend no bells are set. This allows the IRQ to use the mask
 * as a synchronization trick, even when the hardware IRQ is shared.
 */
static unsigned long ejse_get_bells(struct instance_t*xsp)
{
      unsigned long result;
      unsigned long tmp = READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0+0x10000));
	/* mask == EBellIEn */
      unsigned mask = (tmp >> 8) & 0xff;
	/* bell = EBell */
      unsigned bell = (tmp >> 0) & 0xff;

	/* Ignore masked bits. They are not really interrupting. Note
	   that the "mask" from the hardware is actually enables. */
      unsigned masked_bell = bell & mask;

	/* Primary Clear IRQ == interrupting bells */
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0+0x10008), masked_bell);

      result = masked_bell & 0x007f;
      if (masked_bell & 0x80) result |= 0x40000000;

      return result;
}

static unsigned long ejse_mask_irqs(struct instance_t*xsp)
{
	/* Get the current mask status from the Primary IRQ mask */
      unsigned long result
	    = READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x10000));

      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x10008), 0xff00);

      return 0x0000ff00 & result;
}

static void ejse_unmask_irqs(struct instance_t*xsp, unsigned long mask)
{
	/* Set bits that need to be set */
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x1000c),
			   (ULONG) mask&0xff00);
	/* Clear bits that need to be cleared */
      WRITE_REGISTER_ULONG((ULONG*)((char*)xsp->bar0 + 0x10008),
			   (ULONG) (~mask)&0xff00);
}

static void ejse_diagnose0_dump(struct instance_t*xsp)
{
}

static void ejse_diagnose1_dump(struct instance_t*xsp)
{
      if (xsp->bar0_size > 0) {
	    unsigned long EGlbIntp, EGlbWr;
	    unsigned long scratch0, scratch1, scratch2, scratch3;
	    unsigned long scratch4, scratch5, scratch6, scratch7;

	    EGlbIntp = READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0+0x00000));
	    EGlbWr   = READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0+0x00004));

	    scratch0 =  READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0+0xf0000));
	    scratch1 =  READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0+0xf0004));
	    scratch2 =  READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0+0xf0008));
	    scratch3 =  READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0+0xf000c));
	    scratch4 =  READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0+0xf0010));
	    scratch5 =  READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0+0xf0014));
	    scratch6 =  READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0+0xf0018));
	    scratch7 =  READ_REGISTER_ULONG((ULONG*)((char*)xsp->bar0+0xf001c));

	    printk("ise%u: EGlbIntp=0x%x EGlbWr=0x%x\n",
		   xsp->id, EGlbIntp, EGlbWr);
	    printk("ise%u: SharedM0=0x%x SharedM1=0x%x\n",
		   xsp->id, scratch0, scratch1);
	    printk("ise%u: SharedM2=0x%x SharedM3=0x%x\n",
		   xsp->id, scratch2, scratch3);
	    printk("ise%u: SharedM4=0x%x SharedM5=0x%x\n",
		   xsp->id, scratch4, scratch5);
	    printk("ise%u: SharedM6=0x%x SharedM7=0x%x\n",
		   xsp->id, scratch6, scratch7);


      } else {
	    printk("ise%u: <** Device Not Mapped **>\n", xsp->id);
      }
}

const struct ise_ops_tab ejse_operations = {
      "EJSE",
      ejse_init_hardware,
      ejse_clear_hardware,
      ejse_mask_irqs,
      ejse_unmask_irqs,
      ejse_set_root_table_base,
      ejse_set_root_table_resp,
      ejse_get_root_table_resp,
      ejse_get_status_resp,
      ejse_set_status_value,

      ejse_set_bells,
      ejse_get_bells,

      ejse_diagnose0_dump,
      ejse_diagnose1_dump
};


/*
 * $Log$
 * Revision 1.1  2008/12/06 03:27:08  steve
 *  Add EJSE support.
 *
 * Revision 1.1  2004/07/15 04:19:26  steve
 *  Extend to support JSE boards.
 *
 */

