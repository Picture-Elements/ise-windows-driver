/*
 * Copyright (c) 2001 Picture Elements, Inc.
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
 * Allocate a frame with the requested size, if a frame does not yet
 * exist. Return the allocated size. If the frame already exists, then
 * return the size of the existing frame.
 */
unsigned long ise_make_frame(struct instance_t*xsp, unsigned fidx,
			     unsigned long frame_size)
{
      unsigned idx;
      unsigned long page_count;
      unsigned long table_size;
      PHYSICAL_ADDRESS frame_bus;

      if (xsp->frame_tab[fidx] != 0) {
	    return xsp->frame_tab[fidx]->page_size
		  * xsp->frame_tab[fidx]->page_count;
      }

      page_count = (frame_size + PAGE_SIZE - 1) / PAGE_SIZE;
      frame_size = page_count * PAGE_SIZE;

	/* Allocate and initialize the frame table. The buffer
	   pointers will be filled in as the pages are allocated. */
      table_size = sizeof(struct frame_table) + page_count * sizeof(__u32);
      xsp->frame_tab[fidx] = xsp->dma->DmaOperations->AllocateCommonBuffer(
				    xsp->dma, frame_size, &frame_bus, FALSE);

      xsp->frame_tab[fidx]->magic = FRAME_TABLE_MAGIC;
      xsp->frame_tab[fidx]->self  = frame_bus.LowPart;
      xsp->frame_tab[fidx]->page_size = PAGE_SIZE;
      xsp->frame_tab[fidx]->page_count = page_count;

	/* Allocate an array of void* to hold the virtual addresses of
	   the frame buffers. The physical addresses will go into the
	   frame table. */
      xsp->frame_pag[fidx] = (void**)ExAllocatePool(NonPagedPool,
						    page_count*sizeof(void*));


	/* Allocate an MDL that will hold the entire frame. I will be
	   building this up later, as I allocate the pages
	   themselves. */
      xsp->frame_mdl[fidx] = IoAllocateMdl(0, frame_size, FALSE, FALSE, 0);

	/* Allocate the pages of the frame, one page at a time. Add
	   each page to the frame_tab, frame_pag and frame_mdl arrays
	   as I get them. */
      for (idx = 0 ;  idx < page_count ;  idx += 1) {
	    PHYSICAL_ADDRESS page_bus;
	    void* page_vrt;
	    MDL*mtmp;

	    page_vrt = xsp->dma->DmaOperations->AllocateCommonBuffer(xsp->dma,
				       PAGE_SIZE, &page_bus, FALSE);
	    xsp->frame_tab[fidx]->page[idx] = page_bus.LowPart;
	    xsp->frame_pag[fidx] [idx] = page_vrt;

	    if (page_vrt == 0)
		  goto no_mem;

	    RtlFillMemory(page_vrt, PAGE_SIZE, idx+1);

	      /* Make an MDL for this page, in order to make the
		 proper PFN_NUMBER for the page. Then copy that
		 PFN_NUMBER into the make mdl and release the
		 temporary mdl. */
	    mtmp = IoAllocateMdl(page_vrt, PAGE_SIZE, FALSE, FALSE, 0);
	    MmBuildMdlForNonPagedPool(mtmp);

	    MmGetMdlPfnArray(xsp->frame_mdl[fidx]) [idx] =
		  MmGetMdlPfnArray(mtmp) [0];

	    IoFreeMdl(mtmp);
      }

      xsp->frame_mdl[fidx]->MdlFlags |= MDL_PAGES_LOCKED;
      xsp->frame_mdl[fidx]->MdlFlags |= MDL_SOURCE_IS_NONPAGED_POOL;
      return xsp->frame_tab[fidx]->page_size
	    * xsp->frame_tab[fidx]->page_count;

 no_mem:

      page_count = (frame_size + PAGE_SIZE - 1) / PAGE_SIZE;
      frame_size = page_count * PAGE_SIZE;
      table_size = sizeof(struct frame_table) + page_count * sizeof(__u32);

      if (xsp->frame_mdl[fidx]) {
	    IoFreeMdl(xsp->frame_mdl[fidx]);
	    xsp->frame_mdl[fidx] = 0;
      }

      if (xsp->frame_tab[fidx]) {

	    for (idx = 0 ;  idx < page_count ;  idx += 1) {
		  PHYSICAL_ADDRESS page_bus;
		  void* page_vrt;

		  page_vrt = xsp->frame_pag[fidx] [idx];
		  if (page_vrt == 0)
			break;

		  page_bus.LowPart = xsp->frame_tab[fidx]->page[idx];
		  page_bus.HighPart = 0;
		  xsp->dma->DmaOperations->FreeCommonBuffer(xsp->dma,
			       PAGE_SIZE, page_bus, page_vrt, FALSE);
	    }

	      /* Free the frame table */
	    xsp->dma->DmaOperations->FreeCommonBuffer(xsp->dma,
			  table_size, frame_bus, xsp->frame_tab[fidx], FALSE);
	    xsp->frame_tab[fidx] = 0;
      }

      if (xsp->frame_pag[fidx]) {
	    ExFreePool(xsp->frame_pag[fidx]);
	    xsp->frame_pag[fidx] = 0;
      }

      return 0;
}

/*
 * This function frees a selected frame. It releases all the memory
 * and the mdls of the frame. It assumes that the environment has
 * assured that the memory is not mapped somewhere.
 */
void ise_free_frame(struct instance_t*xsp, unsigned fidx)
{
      unsigned long page_count, table_size;
      PHYSICAL_ADDRESS frame_bus;
      unsigned idx;
      if (xsp->frame_ref[fidx] > 0)
	    return;

      page_count = xsp->frame_tab[fidx]->page_count;
      IoFreeMdl(xsp->frame_mdl[fidx]);
      xsp->frame_mdl[fidx] = 0;

      for (idx = 0 ;  idx < page_count ;  idx += 1) {
	    PHYSICAL_ADDRESS page_bus;
	    void* page_vrt;

	    page_vrt = xsp->frame_pag[fidx] [idx];
	    if (page_vrt == 0)
		  break;

	    page_bus.LowPart = xsp->frame_tab[fidx]->page[idx];
	    page_bus.HighPart = 0;
	    xsp->dma->DmaOperations->FreeCommonBuffer(xsp->dma,
				   PAGE_SIZE, page_bus, page_vrt, FALSE);
      }

	/* Free the frame table */
      table_size = sizeof(struct frame_table) + page_count * sizeof(__u32);
      frame_bus.HighPart = 0;
      frame_bus.LowPart = xsp->frame_tab[fidx]->self;
      xsp->dma->DmaOperations->FreeCommonBuffer(xsp->dma,
		      table_size, frame_bus, xsp->frame_tab[fidx], FALSE);
      xsp->frame_tab[fidx] = 0;
}


/*
 * $Log$
 * Revision 1.1  2001/09/04 02:47:09  steve
 *  Add frame allocate/free/map/unmap controls.
 *
 */

