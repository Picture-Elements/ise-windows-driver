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

#include  "ucrif.h"
#include  "os.h"
#include  "t-config.h"
#include  "ucrpriv.h"

/*
 * These functions support the allocation of frame space under NT.
 *
 * Each frame is allocated as contiguous memory. The limitation here
 * is not the target, as it can scatter/gather, but the ability of NT
 * to map memory to the process. The ZwMapViewOfSection takes a memory
 * handle, and I can only seem to get that handle for physical
 * (and therefore contiguous) memory.
 */

/*
 * The ALLOCATE_UNIT is the size of a chunk of frame memory. Frames are
 * allocated in multiples of this unit, and the page size passed to the
 * ISE board is this value. Since under NT we always allocate contiguous
 * frames, units larger then a page are useful.
 */
#define ALLOCATE_UNIT (0x10000)

/*
 * The frame space is kept as a sorted list of free cells. Each cell
 * is the beginning of a free region and points to the beginning of
 * the next. The root is a cell with no size that points to the first
 * cell.
 */
struct FrameCell {
      unsigned long size;
      struct FrameCell*next;
};

static struct FramePool {
      void*base;
      unsigned long size;
      struct FrameCell list;
} frame_pool;

# define FRAME_TABLE_SIZE 8192
# define FRAME_TABLE_COUNT 6

static void*table_mem = 0;
static struct frame_table*table_list = 0;

int ucr_init_nt_frame_pool(unsigned long size)
{
        unsigned idx;
	PHYSICAL_ADDRESS limit;

	size = (size + ALLOCATE_UNIT - 1) & ~(ALLOCATE_UNIT-1);
	frame_pool.size = size;
	limit.LowPart = 0xffffffff;
	limit.HighPart = 0;
	frame_pool.base = MmAllocateContiguousMemory(size, limit);
	if (frame_pool.base == 0)
	      return 0;

	frame_pool.list.size = 0;
	frame_pool.list.next = (struct FrameCell*)frame_pool.base;
	frame_pool.list.next->next = 0;
	frame_pool.list.next->size = frame_pool.size;

	table_mem = MmAllocateContiguousMemory(
			      FRAME_TABLE_SIZE*FRAME_TABLE_COUNT,
			      limit);
	if (table_mem == 0) {
	      MmFreeContiguousMemory(frame_pool.base);
	      return 0;
	}

	table_list = 0;
	for (idx = 0 ;  idx < FRAME_TABLE_COUNT ;  idx += 1) {
	      struct frame_table*cur = (struct frame_table*)
		    ((char*)table_mem + FRAME_TABLE_SIZE*idx);
	      
	      (struct frame_table*)(cur->magic) = table_list;
	      table_list = cur;
	}

	return 1;
}

void ucr_deinit_nt_frame_pool()
{
      if (frame_pool.base)
	    MmFreeContiguousMemory(frame_pool.base);
      if (table_mem)
	    MmFreeContiguousMemory(table_mem);

      frame_pool.base = 0;
      frame_pool.size = 0;
      frame_pool.list.next = 0;
}

static struct frame_table*get_frame_table()
{
      struct frame_table*cur = table_list;
      if (cur) table_list = (struct frame_table*)(cur->magic);
      return cur;
}

static void put_frame_table(struct frame_table*cur)
{
      (struct frame_table*)(cur->magic) = table_list;
      table_list = cur;
}

# define FRAME_TABLE_MAGIC 0x2eaffaaf

int ucr_make_frame(struct Instance*xsp, unsigned id, unsigned long xsize)
{
      void*mem;
      unsigned idx;
      unsigned long fsize, npages;
      struct FrameCell*cell, *prev;
      PHYSICAL_ADDRESS limit;

      limit.LowPart = 0xffffffff;
      limit.HighPart = 0;

	/* Round up to multiple of page size. */
      xsize = (xsize + (ALLOCATE_UNIT-1)) & ~(ALLOCATE_UNIT-1);

	/* Look in the pool for a cell that is big enough to hold this
	   frame. */
      prev = &frame_pool.list;
      cell = prev->next;
      for (;;) {
	    if (cell == 0)
		  goto no_mem;
	    if (cell->size >= xsize)
		  break;

	    prev = cell;
	    cell = cell->next;
      }

	/* Split the cell to closely fit the requested frame. */
      if (cell->size > xsize) {
	    struct FrameCell*ncell;

	    ncell = (struct FrameCell*)((char*)cell + xsize);
	    ncell->size = cell->size - xsize;
	    ncell->next = cell->next;
	    cell->size = xsize;
	    cell->next = ncell;
      }

	/* Remove this cell from the list. */
      prev->next = cell->next;

      fsize = cell->size;
      npages = fsize / ALLOCATE_UNIT;
      xsp->frame[id] = get_frame_table();
      if (xsp->frame[id] == 0)
	    goto no_mem;

      xsp->frame[id]->magic = FRAME_TABLE_MAGIC;
      xsp->frame[id]->self  = virt_to_bus(xsp->frame[id]);
      xsp->frame[id]->page_size = ALLOCATE_UNIT;
      xsp->frame[id]->page_count = npages;
      mem = xsp->fb[id] = cell;

      if (debug_flag & UCR_TRACE_FRAME)
	    printk("ise%u (d): frame segment at %x\n", xsp->number, mem);

      for (idx = 0 ;  idx < xsp->frame[id]->page_count ;  idx += 1) {
	    PHYSICAL_ADDRESS phys;
	    char*pb = (char*)mem + xsp->frame[id]->page_size * idx;
	    phys = MmGetPhysicalAddress(pb);
	    xsp->frame[id]->page[idx] = phys.LowPart;

	    if (debug_flag & UCR_TRACE_FRAME)
		  printk("ise%u (d): Frame page host=%x phys=%x\n",
			 xsp->number, pb, phys.LowPart);
      }

      return fsize;

 no_mem:
      return -ENOMEM;
}

int ucr_free_frame(struct Instance*xsp, unsigned id)
{
      struct FrameCell*cell, *prev;

      if (xsp->frame[id] == 0) return -EIO;

	/* Reconstitute the allocation cell from the frame
	   information. */
      cell = xsp->fb[id];
      cell->size = xsp->frame[id]->page_size * xsp->frame[id]->page_count;

	/* Free the frame structure. */
      put_frame_table(xsp->frame[id]);
      xsp->frame[id] = 0;
      xsp->fb[id] = 0;

	/* Find where this cell belongs in the list, and place the
	   cell there. */
      prev = &frame_pool.list;
      for (;;) {
	    if (prev->next == 0)
		  break;
	    if (prev->next > cell)
		  break;

	    prev = prev->next;
      }

      cell->next = prev->next;
      prev->next = cell;

	/* Join with the following cell, if possible... */
      if (cell->next == (struct FrameCell*)(((char*)cell + cell->size))) {
	    cell->size += cell->next->size;
	    cell->next = cell->next->next;
      }

	/* Merge with the previous cell, if possible. */
      if (cell == (struct FrameCell*)(((char*)prev + prev->size))) {
	    prev->size += cell->size;
	    prev->next = cell->next;
      }

      return 0;
}


/*
 * $Log$
 * Revision 1.1  2001/03/05 20:11:40  steve
 *  Add NT4 driver to ISE source tree.
 *
 * Revision 1.7  2000/02/06 04:27:08  steve
 *  Assure the frame structure is contiguous.
 *
 * Revision 1.6  1999/04/07 01:48:28  steve
 *  More frame tracing.
 *
 * Revision 1.5  1998/07/16 22:50:05  steve
 *  Detailed frame test prints.
 *
 * Revision 1.4  1998/07/11 18:56:51  steve
 *  Frames for multiple devices.
 *
 * Revision 1.3  1998/06/18 20:32:49  steve
 *  Support for 16 frames.
 *
 * Revision 1.2  1998/05/29 20:57:12  steve
 *  build up frame table under NT.
 *
 * Revision 1.1  1998/05/28 22:53:03  steve
 *  NT port.
 */

