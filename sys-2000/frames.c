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
      unsigned long flag_tmp;
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
				    xsp->dma, table_size, &frame_bus, TRUE);
      if (xsp->frame_tab[fidx] == 0) {
	    printk("ise%u: Unable to allocate frame_tab[%u] (%u bytes)\n",
		   xsp->id, fidx, table_size);
	    return 0;
      }

      RtlFillMemory(xsp->frame_tab[fidx], table_size, 0);
      xsp->frame_tab[fidx]->magic = FRAME_TABLE_MAGIC;
      xsp->frame_tab[fidx]->self  = frame_bus.LowPart;
      xsp->frame_tab[fidx]->page_size = PAGE_SIZE;
      xsp->frame_tab[fidx]->page_count = page_count;

	/* Allocate an array of void* to hold the virtual addresses of
	   the frame buffers. The physical addresses will go into the
	   frame table. Zero fill this table so that non-allocation of
	   pages is certain to be detected. */
      xsp->frame_pag[fidx] = (void**)ExAllocatePool(NonPagedPool,
						    page_count*sizeof(void*));
      if (xsp->frame_pag[fidx] == 0) {
	    printk("ise%u: Unable to allocate frame_pag[%u]\n",
		   xsp->id, fidx);
	    goto no_mem;
      }

      RtlFillMemory(xsp->frame_pag[fidx], page_count*sizeof(void*), 0);


	/* Allocate an MDL that will hold the entire frame. I will be
	   building this up later, as I allocate the pages
	   themselves. */
      xsp->frame_mdl[fidx] = IoAllocateMdl(0, frame_size, FALSE, FALSE, 0);
      if (xsp->frame_mdl[fidx] == 0) {
	    printk("ise%u: Unable to allocate frame_mdl[%u]\n",
		   xsp->id, fidx);
	    goto no_mem;
      }


	/* Allocate the pages of the frame, one page at a time. Add
	   each page to the frame_tab, frame_pag and frame_mdl arrays
	   as I get them. Enable the cache, because on ix86 systems
	   the memory is coherent even when PCI devices are involved.
	   NOTE: XP will BSOD if it is not marked cacheable. */
      flag_tmp = 0;
      for (idx = 0 ;  idx < page_count ;  idx += 1) {
	    PHYSICAL_ADDRESS page_bus;
	    void* page_vrt;
	    MDL*mtmp;

	    page_vrt = xsp->dma->DmaOperations->AllocateCommonBuffer(xsp->dma,
				       PAGE_SIZE, &page_bus, TRUE);
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
	    if (mtmp == 0) {
		  printk("ise%u: Unable to allocate temporary mdl?\n",
			 xsp->id);
		  goto no_mem;
	    }

	    MmBuildMdlForNonPagedPool(mtmp);
	    flag_tmp |= mtmp->MdlFlags;

	    MmGetMdlPfnArray(xsp->frame_mdl[fidx]) [idx] =
		  MmGetMdlPfnArray(mtmp) [0];

	    IoFreeMdl(mtmp);
      }

      xsp->frame_mdl[fidx]->MdlFlags |= MDL_ALLOCATED_FIXED_SIZE;
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
			       PAGE_SIZE, page_bus, page_vrt, TRUE);
	    }

	      /* Free the frame table */
	    xsp->dma->DmaOperations->FreeCommonBuffer(xsp->dma,
			  table_size, frame_bus, xsp->frame_tab[fidx], TRUE);
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

	/* If the frame does not exist, then there is nothing to free. */
      if (xsp->frame_mdl[fidx] == 0)
	    return;

	/* If the frame is mapped by someone, then it can't be freed. */
      if (xsp->frame_map[fidx].base != 0)
	    return;

      page_count = xsp->frame_tab[fidx]->page_count;
      IoFreeMdl(xsp->frame_mdl[fidx]);
      xsp->frame_mdl[fidx] = 0;

	/* Free the individual pages of the frame, then free the
	   frame_pag pointer to all those pages. */
      if (xsp->frame_pag[fidx]) {
	    for (idx = 0 ;  idx < page_count ;  idx += 1) {
		  PHYSICAL_ADDRESS page_bus;
		  void* page_vrt;

		  page_vrt = xsp->frame_pag[fidx] [idx];
		  if (page_vrt == 0)
			break;

		  page_bus.LowPart = xsp->frame_tab[fidx]->page[idx];
		  page_bus.HighPart = 0;
		  xsp->dma->DmaOperations->FreeCommonBuffer(xsp->dma,
				   PAGE_SIZE, page_bus, page_vrt, TRUE);
	    }

	    ExFreePool(xsp->frame_pag[fidx]);
	    xsp->frame_pag[fidx] = 0;
      }

	/* Free the frame table */
      table_size = sizeof(struct frame_table) + page_count * sizeof(__u32);
      frame_bus.HighPart = 0;
      frame_bus.LowPart = xsp->frame_tab[fidx]->self;
      xsp->dma->DmaOperations->FreeCommonBuffer(xsp->dma,
		      table_size, frame_bus, xsp->frame_tab[fidx], TRUE);
      xsp->frame_tab[fidx] = 0;
}


/*
 * $Log$
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

