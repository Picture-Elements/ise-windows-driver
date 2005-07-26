/*
 * Copyright (c) 2001-2005 Picture Elements, Inc.
 *    Stephen Williams (steve@picturel.com)
 *
 * $Id$
 */

/*
 * This source file holds the implementations of the various frame
 * management functions. Frames can be created or released, but not
 * resized. The frame is made of a collection of fixed size pages
 * references in the frame table.
 */
# include  "ise_sys.h"

/*
 * Take in an MDL and create a frame table for all the pages. This
 * function assumes that the MDL is already pinned into memory, and
 * the fidx indexes a free frame_tab slot. This function does not pass
 * the table to the SSE/JSE board, that is the job of the caller which
 * needs a state machine.
 */
int ise_map_frame(struct instance_t*xsp, unsigned fidx, MDL*mdl)
{
      DMA_ADAPTER*frame_dma = xsp->frame_dma[fidx];
      unsigned long page_count;
      PHYSICAL_ADDRESS frame_bus;
      unsigned long frame_size;
      unsigned char*frame_va;

      unsigned long table_size;
      int page_idx;

	/* Calculate information about the frame. */
      page_count = (MmGetMdlByteCount(mdl) + PAGE_SIZE - 1) / PAGE_SIZE;
      frame_size = page_count * PAGE_SIZE;
      frame_va   = (unsigned char*)MmGetMdlVirtualAddress(mdl);

	/* Allocate a table big enough to hold all the
	   pointers. Allocate this as a COMMON BUFFER through the core
	   dma adapter, and *not* through the frame dma adapter. */
      table_size = sizeof(struct frame_table) + page_count * sizeof(__u32);

      xsp->frame_tab[fidx] = xsp->dma->DmaOperations->AllocateCommonBuffer(
				       xsp->dma, table_size, &frame_bus, TRUE);
      if (xsp->frame_tab[fidx] == 0) {
	    printk("ise%u: Unable to allocate frame_tab[%u] (%u bytes)\n",
		   xsp->id, fidx, table_size);
	    return -1;
      }

      RtlFillMemory(xsp->frame_tab[fidx], table_size, 0);
      xsp->frame_tab[fidx]->magic = FRAME_TABLE_MAGIC;
      xsp->frame_tab[fidx]->self  = frame_bus.LowPart;
      xsp->frame_tab[fidx]->page_size = PAGE_SIZE;
      xsp->frame_tab[fidx]->page_count = page_count;

	/* Get mappings of all the pages in the MDL. Here we use the
	   frame dma adapter. */
      page_idx = 0;
      while (frame_size > 0) {
	    PHYSICAL_ADDRESS page;
	    ULONG trans = frame_size;

	    page = frame_dma->DmaOperations->MapTransfer(xsp->dma,
							mdl, 0,
							frame_va,
							&trans,
							FALSE);
	    while (trans >= PAGE_SIZE) {
		  xsp->frame_tab[fidx]->page[page_idx] = page.LowPart;
		  page.LowPart += PAGE_SIZE;
		  frame_va     += PAGE_SIZE;
		  frame_size   -= PAGE_SIZE;
		  trans        -= PAGE_SIZE;
		  page_idx     += 1;
	    }
      }

	/* At this point, page_idx should == page_count. */

	/* All done, return the page count in case someone cares. */
      return page_count;
}

/*
 * Unmap the pages of the indexed frame. The frame must already have
 * been detached from the root table.
 */
void ise_unmap_frame(struct instance_t*xsp, unsigned fidx)
{
      PHYSICAL_ADDRESS frame_bus;
      unsigned long page_count;
      unsigned long table_size;
      MDL*mdl;

      frame_bus.LowPart = xsp->frame_tab[fidx]->self;
      frame_bus.HighPart = 0;
      page_count = xsp->frame_tab[fidx]->page_count;

      table_size = sizeof(struct frame_table) + page_count * sizeof(__u32);

      mdl = xsp->frame_mdl_irp[fidx]->MdlAddress;

	/* Unmap the frame MDL all in one chunk. */
      xsp->frame_dma[fidx]->DmaOperations->FlushAdapterBuffers(
					      xsp->frame_dma[fidx],
					      mdl, 0,
					      MmGetMdlVirtualAddress(mdl),
					      MmGetMdlByteCount(mdl),
					      FALSE);

	/* Release the frame table itself. */
      xsp->dma->DmaOperations->FreeCommonBuffer(xsp->dma,
						table_size,
						frame_bus,
						xsp->frame_tab[fidx],
						TRUE);

      xsp->frame_tab[fidx] = 0;
}


/*
 * $Log$
 * Revision 1.7  2005/07/26 01:17:32  steve
 *  New method of mapping frames for version 2.5
 *
 * Revision 1.6  2002/06/21 00:51:33  steve
 *  Only allocate cached buffers to share with ISE.
 *
 * Revision 1.5  2001/10/08 23:25:31  steve
 *  Made frame_tab too big. Get the size right.
 *
 * Revision 1.4  2001/10/04 20:51:44  steve
 *  Handle low memory when allocating frames.
 *
 * Revision 1.3  2001/09/05 22:05:55  steve
 *  protect mappings from misused or forgotten unmaps.
 *
 * Revision 1.2  2001/09/05 01:17:01  steve
 *  do not release frames that do not exist.
 *
 * Revision 1.1  2001/09/04 02:47:09  steve
 *  Add frame allocate/free/map/unmap controls.
 *
 */

