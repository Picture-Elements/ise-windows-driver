/*
 * Copyright (c) 1997 Picture Elements, Inc.
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
#ifndef WINNT
#ident "$Id$"
#endif

/*
 * The ucrx device is a control access into the uCR target
 * board. Various board specific controls exist here. Each board has
 * an Instance, an ordinary device driver (which holds the channels)
 * and the control device, which is here.
 */


# include  "ucrif.h"

# include  "os.h"
# include  "t-config.h"
# include  "ucrpriv.h"

static int ucrx_reset_board(struct Instance*xsp)
{
      int rc;
      unsigned char state;
      unsigned tmp[6];

	/* On the ISE board, the I960Rx ALWAYS places the bridge at
	   the function unit one below the CPU. This gives me a nifty
	   way to calculate exactly the matching bridge unit */
      unsigned char bdfn = i960rp_to_bridge(xsp->dfn);

	/* Cannot reset the board if there are any open channels. */
      if (xsp->channels != 0)
	    return -EBUSY;

      if (debug_flag&UCR_TRACE_UCRX)
	    printk("ucrx: reset ISE at bus=%u, dfn=%u, bridge dfn=%u\n",
		   xsp->bus, xsp->dfn, bdfn);

	/* Check to see if the board is already being held reset. If
	   it is, skip the reset step. Otherwise, save the volatile
	   configuration registers and set the processor to reset
	   state. */
      rc = pcibios_read_config_byte(xsp->bus, bdfn, 0x40, &state);

      if (debug_flag&UCR_TRACE_UCRX)
	    printk("ucrx: read_config_byte returns rc=%d\n", rc);

      if ((state & 0x02) == 0) {
	      /* Save the volatile configuration registers. */
	    pcibios_read_config_dword(xsp->bus, xsp->dfn, 0x04, &tmp[0]);
	    pcibios_read_config_dword(xsp->bus, xsp->dfn, 0x0c, &tmp[1]);
	    pcibios_read_config_dword(xsp->bus, xsp->dfn, 0x10, &tmp[2]);
	    pcibios_read_config_dword(xsp->bus, xsp->dfn, 0x30, &tmp[3]);
	    pcibios_read_config_dword(xsp->bus, xsp->dfn, 0x3c, &tmp[4]);
	    pcibios_read_config_dword(xsp->bus, xsp->dfn, 0x40, &tmp[5]);

	      /* Send the reset command to the board. This goes to the
		 BRIDGE part of the device, which can control the i960
		 processor. */
	    pcibios_write_config_byte(xsp->bus, bdfn, 0x40, 0x20);

	      /* Restore the saved configuration registers. */
	    pcibios_write_config_dword(xsp->bus, xsp->dfn, 0x40, tmp[5]);
	    pcibios_write_config_dword(xsp->bus, xsp->dfn, 0x04, tmp[0]);
	    pcibios_write_config_dword(xsp->bus, xsp->dfn, 0x0c, tmp[1]);
	    pcibios_write_config_dword(xsp->bus, xsp->dfn, 0x10, tmp[2]);
	    pcibios_write_config_dword(xsp->bus, xsp->dfn, 0x30, tmp[3]);
	    pcibios_write_config_dword(xsp->bus, xsp->dfn, 0x3c, tmp[4]);
      }

	/* Turn OFF any mention of any root table. This is normally
	   the case anyhow, but at powerup things may be a little
	   weird. Reinit the MU because the reset probably cleared
	   things. */
      dev_init_hardware(xsp->dev);
      dev_set_root_table_base(xsp->dev, 0);
      dev_set_root_table_resp(xsp->dev, 0);

	/* Free all the frames that this board may have had. This can
	   be done directly because we know that the ISE board is held
	   reset and there is no need to synchronize tables. */
      { unsigned idx;
        for (idx = 0 ;  idx < 16 ;  idx += 1)
	      ucr_free_frame(xsp, idx);
      }

	/* Release the processor so it is free to execute the
	   bootprom. This works my removing the reset state bit. */
      rc = pcibios_write_config_byte(xsp->bus, bdfn, 0x40, 0x00);

      if (debug_flag&UCR_TRACE_UCRX)
	    printk("ucrx: reset done--write_config_byte returns rc=%d\n", rc);

      return 0;
}

/*
 * The ISE board bootprom uses the BIST bit to tell the host that the
 * application program is loaded and ready to go. If the BIST bit is
 * set, then start the BIST to cause the application to go.
 */
static int ucrx_run_program(struct Instance*xsp)
{
      unsigned state = 0;
      int rc;

	/* Cannot reset the board if there are any open channels. */
      if (xsp->channels != 0)
	    return -EBUSY;

      pcibios_read_config_dword(xsp->bus, xsp->dfn, 0x0c, &state);

      if (debug_flag&UCR_TRACE_UCRX)
	    printk("ucrx: Run ISE at bus=%u, dfn=%u state=%x\n",
		   xsp->bus, xsp->dfn, state);

      if (! (state & 0x80000000))
	    return -EAGAIN;

      rc = pcibios_write_config_dword(xsp->bus, xsp->dfn, 0x0c,
				      state|0x40000000);

      if (debug_flag&UCR_TRACE_UCRX)
	    printk("ucrx: Run done, rc=%d\n", rc);

      return 0;
}

static int ucrx_diagnose(struct Instance*xsp, unsigned long arg)
{
      unsigned magic, lineno, idx;
      int cnt;
      char msg[128];

      printk("ucrx: diagnose, arg=%u\n", arg);

      switch (arg) {

	  case 0:
	    magic = readl(xsp->dev+0x50);
	    if (magic != 0xab0440ba) {
		  printk("ucrx: No abort magic number.\n");
		  return 0;
	    }

	    lineno = readl(xsp->dev+0x54);

	    cnt = readl(xsp->dev+0x58);
	    if (cnt > (DEVICE_SIZE-0x50)) {
		  printk("ucrx: Invalid count: %u\n", cnt);
		  return 0;
	    }
	    if (cnt >= sizeof msg)
		  cnt = sizeof msg - 1;

	    memcpy_fromio(msg, xsp->dev+0x5c, cnt);
	    msg[cnt] = 0;

	    printk("ucrx: target panic msg: %s\n", msg);
	    printk("ucrx: target panic code: %u (0x%x)\n", lineno, lineno);
	    break;

	  case 1:
	    printk(DEVICE_NAME "%u: ROOT TABLE at %p(%x) "
		   "MAGIC=[%x:%x]\n", xsp->number, xsp->root,
		   xsp->root->self, xsp->root->magic,
		   xsp->root->self);

	    for (idx = 0 ;  idx < 16 ;  idx += 1) {
		  if (xsp->frame[idx] == 0)
			continue;

		  printk(DEVICE_NAME "%u: FRAME %u magic=[%x:%x] "
			 "page_size=%uK page_count=%u\n", xsp->number,
			 idx, xsp->frame[idx]->magic,
			 xsp->frame[idx]->self,
			 xsp->frame[idx]->page_size/1024,
			 xsp->frame[idx]->page_count);
	    }

	    for (idx = 0 ;  idx < ROOT_TABLE_CHANNELS ;  idx += 1) {
		  if (xsp->root->chan[idx].magic == 0)
			continue;

		  printk(DEVICE_NAME "%u: CHANNEL %u TABLE AT (%x) "
			 "MAGIC=%x\n", xsp->number, idx,
			 xsp->root->chan[idx].ptr,
			 xsp->root->chan[idx].magic);
	    }

	    if (xsp->channels) {
		  struct ChannelData*xpd = xsp->channels;
		  do {
			printk(DEVICE_NAME "%u.%u: CHANNEL TABLE "
			       "MAGIC=[%x:%x]\n", xsp->number,
			       xpd->channel, xpd->table->magic,
			       xpd->table->self);

			printk(DEVICE_NAME "%u.%u: OUT "
			       "(first=%u, next=%u, off=%u) "
			       "IN (first=%u, next=%u)\n", xsp->number,
			       xpd->channel, xpd->table->first_out_idx,
			       xpd->table->next_out_idx, xpd->out_off,
			       xpd->table->first_in_idx,
			       xpd->table->next_in_idx);

			for (idx = 0 ;  idx < CHANNEL_OBUFS ;  idx += 1)
			      printk(DEVICE_NAME "%u.%u: obuf %u: "
				     "ptr=%x, count=%u\n",
				     xsp->number, xpd->channel, idx,
				     xpd->table->out[idx].ptr,
				     xpd->table->out[idx].count);

			for (idx = 0 ;  idx < CHANNEL_IBUFS ;  idx += 1)
			      printk(DEVICE_NAME "%u.%u: ibuf %u: "
				     "ptr=%x, count=%u\n",
				     xsp->number, xpd->channel, idx,
				     xpd->table->in[idx].ptr,
				     xpd->table->in[idx].count);


			xpd = xpd->next;
		  } while (xsp->channels != xpd);
	    }

	    devx_diag1(xsp);
	    break;
      }

      return 0;
}

static int ucrx_set_debug_flag(unsigned flag)
{
      extern unsigned debug_flag;
      if (flag != debug_flag) {
	    debug_flag = flag;
	    printk("ucrx: set debug_flag to 0x%x\n", debug_flag);
      }
      return 0;
}

static int ucrx_get_debug_flag()
{
      extern unsigned debug_flag;
      return (int)debug_flag;
}

int ucrx_open(struct Instance*xsp)
{
      return 0;
}

void ucrx_release(struct Instance*xsp)
{
}

int ucrx_ioctl(struct Instance*xsp, unsigned int cmd, unsigned long arg)
{
      switch (cmd) {

	  case UCRX_RESET_BOARD:
	    return ucrx_reset_board(xsp);

	  case UCRX_RUN_PROGRAM:
	    return ucrx_run_program(xsp);

	  case UCRX_DIAGNOSE:
	    return ucrx_diagnose(xsp, arg);

	  case UCRX_SET_TRACE:
	    return ucrx_set_debug_flag(arg);

	  case UCRX_GET_TRACE:
	    return ucrx_get_debug_flag();
      }

      return -ENOTTY;
}

/*
 * $Log$
 * Revision 1.2  2002/06/28 22:13:23  steve
 *  Reset that preserves bar0 region size.
 *
 * Revision 1.1  2001/03/05 20:11:40  steve
 *  Add NT4 driver to ISE source tree.
 *
 * Revision 1.7  2000/02/18 19:40:55  steve
 *  More channel diagnostics.
 *
 * Revision 1.6  2000/02/05 17:32:28  steve
 *  Free frames when resetting board.
 *
 * Revision 1.5  1998/09/29 00:39:36  steve
 *  Add os specific state dump.
 *
 * Revision 1.4  1998/07/16 22:50:05  steve
 *  Detailed frame test prints.
 *
 * Revision 1.3  1998/06/06 06:07:40  steve
 *  Better error checking.
 *
 * Revision 1.2  1998/05/30 01:49:52  steve
 *  Handle startup after reset.
 *
 * Revision 1.1  1998/05/26 16:16:35  steve
 *  New channel protocol
 *
 * Revision 1.7  1998/04/18 02:43:55  steve
 *  Fix trouble removing device lines at unload.
 *  Add support for read cancellation under NT.
 *  isecons device (nt only) now throws away old
 *  data instead of truncating new.
 *
 * Revision 1.6  1998/04/13 20:48:48  steve
 *  Get debug flags by ioctl.
 *
 * Revision 1.5  1998/04/07 22:06:12  steve
 *  More tracing support.
 *
 * Revision 1.4  1998/04/03 01:35:13  steve
 *  Add assertion support.
 *
 * Revision 1.3  1997/12/07 02:16:43  steve
 *  Fix NT blocking problems, redo the console to
 *  not block and keep a limited buffer, trace the
 *  ucrx operations, and ucrx operations work on NT.
 *
 * Revision 1.2  1997/12/06 05:15:51  steve
 *  Add the control device to the NT port.
 *
 * Revision 1.1  1997/12/02 02:29:04  steve
 *  Add support for ISE board reset.
 *
 */

