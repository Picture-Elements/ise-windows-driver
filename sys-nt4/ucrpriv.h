#ifndef __ucrpriv_H
#define __ucrpriv_H
/*
 * Copyright (c) 1998 Picture Elements, Inc.
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
 * These are definitions that are likely needed throughout the driver
 * source for the ucr target. The os.h and target.h header files must
 * be included before this.
 */

/*
 * This is the structure of the root table. The xsp pointer has a
 * pointer to this in the kernel address space, as well as the bus
 * address for telling the board where it is. This table in turn
 * contains bus addresses for all other tables. The structure is 4K bytes
 * by definition.
 */
# define ROOT_TABLE_MAGIC 0x1ead1eaf
# define ROOT_TABLE_CHANNELS 495
struct root_table {
      __u32 magic;
      __u32 self;

      struct {
	    __u32 ptr;
	    __u32 magic;
      } frame_table[16];

      struct {
	    __u32 ptr;
	    __u32 magic;
      } chan[ROOT_TABLE_CHANNELS];
};

/*
 * This is the structure that describes a frame for the board. The
 * frame is made up of some number of pages (its size is always a
 * multiple of the page size) that are addresses by the contents of
 * the page array.
 *
 * The size of this structure is defined by the page_count field. The
 * size of the frame is defined as page_size * page_count.
 */
struct frame_table {
      __u32 magic;
      __u32 self;
      __u32 page_size;
      __u32 page_count;
      __u32 page[0];
};

/*
 * The channel table is known by the ISE board to have this
 * structure. See the README.txt file for a description of these
 * fields.
 *
 * The first_out_idx selects the current output buffer in the
 * channel. The ISE board reads that value to notice where in the out
 * table the current buffer is. The ISE board also reads the
 * next_out_idx buffer to know how many buffers there are. The ISE
 * board writes to the first_out_idx field to consume a buffer.
 *
 * The first_in_idx selects the current input buffer that the driver
 * can read to get data. The driver also lools at the next_in_idx
 * field to know how much data there is to read. The ISE board
 * modifies the next_in_idx to make new buffers available.
 */
# define CHANNEL_IBUFS 4
# define CHANNEL_OBUFS 4
# define CHANNEL_TABLE_MAGIC 0x5eefeadf
struct channel_table {
      __u32 magic;
      __u32 self;
      __u32 frame;

      __u32 reserved;

      __u32 first_out_idx;
      __u32 next_out_idx;

      __u32 first_in_idx;
      __u32 next_in_idx;

      struct {
	    __u32 ptr;
	    __u32 count;
      } out[CHANNEL_OBUFS], in[CHANNEL_IBUFS];
};

# define CHANNEL_IN_EMPTY(x) ((x)->table->first_in_idx == (x)->table->next_in_idx)
# define INCR_IN_IDX(x)  ((x) = ((x) + 1) % CHANNEL_IBUFS)

# define NEXT_IN_IDX(x) (((x) + 1) % CHANNEL_IBUFS)
# define NEXT_OUT_IDX(x) (((x) + 1) % CHANNEL_OBUFS)


/*
 * There are a few parameters that are local to an open file
 * descriptor, such as the current channel. They do not affect the
 * device at all, just the process's view of it. Instances of this are
 * created in the xxopen and released in the xxrelease.
 */
struct ChannelData {
#ifdef OS_CHAN_MEMBERS
      OS_CHAN_MEMBERS
#endif
      unsigned short channel;

	/* This is the address of the table for the channel, along
	   with its physical address. This structure is used to
	   communicate with the channel on the ISE board. */
      volatile struct channel_table*table;

	/* The buffers referenced by the table are addressed (in
	   kernel address space) by these pointers. */
      void* in[CHANNEL_IBUFS];
      void* out[CHANNEL_OBUFS];

      unsigned in_off;
      unsigned out_off;
      struct timer_list out_timer;

      struct ChannelData *next, *prev;
};

/*
 * These are the parameters common to all the open files on the
 * target. This reflects the state of the target as a whole.
 */
struct Instance {
#if defined(OS_INST_MEMBERS)
      OS_INST_MEMBERS
#endif

      unsigned char number;

	/* These are the PCI bus address of the configuration
	   registers. There may be occasion to access configuration
	   space for the target device. */
      unsigned long bus, dfn;

	/* This is the value passed to writeX by the dev_x
	   functions. The init gets whatever value is needed for write
	   to work (normally the value right out of the BAR
	   register). */
      unsigned long dev;

	/* kernel and bus addresses of the root table. */
      struct root_table* root;

      struct wait_queue*root_sync;
      struct timer_list root_timer;
      int root_timeout_flag;

	/* Kernel and bus addresses of the frame tables. There can be
	   up to 16 tables. frame_ref stores the number of mappings to
	   the frame, and prevents the frame being removed if it is
	   mapped. */
      struct frame_table*frame[16];
      unsigned frame_ref[16];

      struct wait_queue*dispatch_sync;

	/* Channels are a bit more complicated, and have a driver
	   structure of their own. */
      struct ChannelData *channels;
};

/*
 * This function atomically releases the interrupts (with the mask
 * supplied) and does a sleep on the sync item. This reduces the
 * blocking of global interrupts to the small period during this
 * function, instead of blocking interrupts for long periods of time.
 */
extern int atomic_sleep_on(struct Instance*xsp,
			   unsigned long mask,
			   struct wait_queue**sync);

extern void set_time_delay(struct Instance*xsp,
			   struct timer_list*tim,
			   void (*fun)(unsigned long));
extern void cancel_time_delay(struct timer_list*tim);

extern unsigned debug_flag;

/*
 * When the driver discovers its time to mark the IRP pending, this
 * function does it SMP safely. It should be done when interrupts are
 * off as well
 */
extern void add_to_pending(struct ccp_t*ccp);


/*
 * These are the ucr functions for performing the various generic
 * operations of the uCR protocol.
 */
extern int  ucr_open(struct Instance*xsp, struct ChannelData*xpd);
extern void ucr_release(struct Instance*xsp, struct ChannelData*xpd);
extern long ucr_read(struct Instance*xsp, struct ChannelData*xpd,
		     struct ccp_t*ccp,
		     char *bytes, unsigned long count, int block_flag);
extern long ucr_write(struct Instance*xsp, struct ChannelData*xpd,
		     struct ccp_t*ccp,
		      const char*bytes, const unsigned long count);
extern int  ucr_ioctl(struct Instance*xsp, struct ChannelData*xpd,
		      struct ccp_t*ccp,
		      unsigned cmd, unsigned long arg);
extern int ucr_irq(struct Instance*xsp);

extern int  ucrx_open(struct Instance*xsp);
extern void ucrx_release(struct Instance*xsp);
extern int  ucrx_ioctl(struct Instance*xsp, unsigned cmd, unsigned long arg);

/*
 * The generic code needs to make frames occasionally, but the task
 * itself is operating system specific. These methods manage the
 * workings of frames.
 */
extern int ucr_make_frame(struct Instance*xsp, unsigned id,
			  unsigned long size);
extern int ucr_free_frame(struct Instance*xsp, unsigned id);

/*
 * These are generic functions that help with the management of the
 * instance structure.
 */
extern void ucr_init_instance(struct Instance*xsp);
extern void ucr_clear_instance(struct Instance*xsp);

/*
 * $Log$
 * Revision 1.2  2001/04/03 01:56:05  steve
 *  Simplify the code path for pending operations, and
 *  use buffered I/O instead of direct.
 *
 * Revision 1.1  2001/03/05 20:11:40  steve
 *  Add NT4 driver to ISE source tree.
 *
 * Revision 1.8  2000/06/26 22:07:45  steve
 *  Close some channel races.
 *
 * Revision 1.7  1999/07/15 16:37:37  steve
 *  isolate read and write activities under NT.
 *
 * Revision 1.6  1999/05/08 02:11:17  steve
 *  Manipulate the bus master enable bit.
 *
 * Revision 1.5  1999/04/02 00:27:45  steve
 *  IRQ sharing fix for NT.
 *
 * Revision 1.4  1998/12/02 00:52:30  steve
 *  Improved frame handling and debugging.
 *
 * Revision 1.3  1998/05/29 20:57:12  steve
 *  build up frame table under NT.
 *
 * Revision 1.2  1998/05/28 22:53:02  steve
 *  NT port.
 *
 * Revision 1.1  1998/05/26 16:16:35  steve
 *  New channel protocol
 *
 */


#endif
