#ifndef __ise_tables_H
#define __ise_tables_H
/*
 * Copyright (c) 2001 Picture Elements, Inc.
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

#ifdef WINNT
/* MS products deal with processor compatibility by defining them to
   all be 32bit ix86. Thus, specific size types are kinda obvious. */
typedef unsigned long  __u32;
typedef unsigned short __u16;
#endif

/*
 * This is the structure of the root table. The instance utruct has a
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



/*
 * $Log$
 * Revision 1.1  2001/07/11 23:47:38  steve
 *  Add ucrx_timeout device controls.
 *
 */


#endif
