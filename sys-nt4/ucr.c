/*
 * Copyright (c) 1998 Picture Elements, Inc.
 *    Stephen Williams <steve@picturel.com>
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
 *
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
 * See README.txt for a detailed description of how the interaction
 * with the board works.
 */

# include  "ucrif.h"

/*
 * The details of wiggling the device bits lives here, in the form of
 * macros and inline functions. The os.h defines the interface to the
 * operating system, and t-config.h the interface to the target board.
 */
# include  "os.h"
# include  "t-config.h"
# include  "ucrpriv.h"

# define ROOT_TABLE_BELLMASK 0x01
# define STATUS_BELLMASK     0x02
# define CHANGE_BELLMASK     0x04


#ifndef VENDOR_ID
# error  "The target configuration must define VENDOR_ID"
#endif

#ifndef DEVICE_ID
# error  "The target configuration must define DEVICE_ID"
#endif

#ifndef DEVICE_NAME
# define DEVICE_NAME "uCR"
#endif


unsigned debug_flag = 0;

const unsigned short vendor_id = VENDOR_ID, device_id = DEVICE_ID;

/*
 * Construct a fresh instance structure. This does not depend on the
 * initial zero value that the C compiler would assign.
 */
void ucr_init_instance(struct Instance*xsp)
{
      unsigned idx;
      xsp->dev = 0;
      xsp->channels = 0;

      xsp->root = (struct root_table*)alloc_page();
      xsp->root_sync = 0;
      xsp->dispatch_sync = 0;

      xsp->root->magic = ROOT_TABLE_MAGIC;
      xsp->root->self  = virt_to_bus(xsp->root);

      for (idx = 0 ;  idx < 16 ;  idx += 1) {
	    xsp->root->frame_table[idx].ptr = 0;
	    xsp->root->frame_table[idx].magic = 0;
	    xsp->frame[idx] = 0;
	    xsp->frame_ref[idx] = 0;
      }

      for (idx = 0 ;  idx < ROOT_TABLE_CHANNELS ;  idx += 1) {
	    xsp->root->chan[idx].ptr = 0;
	    xsp->root->chan[idx].magic = 0;
      }
}

void ucr_clear_instance(struct Instance*xsp)
{
      unsigned idx;
      for (idx = 0 ;  idx < 16 ;  idx += 1)
	    if (xsp->frame[idx])
		  ucr_free_frame(xsp, idx);
}

/*
 * This function finds the ChannelData structure that has the
 * specified id. If no structures match, return 0. The result comes
 * from the instance structure, which has a table of opened channels.
 */
struct ChannelData* channel_by_id(struct Instance*xsp, unsigned short id)
{
      struct ChannelData*cp;

      if (xsp->channels == 0) return 0;
      cp = xsp->channels;

      do {
	    cp = cp->next;
	    if (cp->channel == id) return cp;
      } while (cp != xsp->channels);

      return 0;
}

/*
 * This function updates the root table by writing to the root table
 * base pointer on the ISE board. The set also clears the OMR0
 * register so that I notice when the ISE board writes my address back
 * into it.
 *
 * If the root pointer is zero(0), the operation performs a disconnect
 * instead, which involves writing a zero to the root table pointer
 * without clearing the resp pointer.
 */
static void root_timeout(unsigned long cd)
{
      struct Instance*xsp = (struct Instance*)cd;
      xsp->root_timeout_flag = 1;
      wake_up(&xsp->root_sync);
}

static int root_to_board(struct Instance*xsp, __u32 root)
{
      unsigned long mask;

      if (debug_flag & UCR_TRACE_PROTO)
	    printk(DEVICE_NAME "%u: root to target board, "
		   "root table=%p xsp->dev=%p.\n", xsp->number, root, xsp->dev);


      if (root) dev_set_root_table_resp(xsp->dev, 0);
      dev_set_root_table_base(xsp->dev, root);
      dev_set_bells(xsp->dev, ROOT_TABLE_BELLMASK);

      xsp->root_timeout_flag = 0;
      set_time_delay(xsp, &xsp->root_timer, root_timeout);

	/* Wait for the target board to respond, or timeout to
	   expire. I will know that what I desire has happened by the
	   contents of the root_table_ack register. */
      mask = dev_mask_irqs(xsp->dev);
      while (dev_get_root_table_ack(xsp->dev) != root) {
	    atomic_sleep_on(xsp, mask, &xsp->root_sync);
	    if (xsp->root_timeout_flag) {

		  if (dev_get_root_table_ack(xsp->dev) == root)
			printk(DEVICE_NAME "%u: Root table received, "
			       "but IRQ response lost.\n", xsp->number);
		  else
			printk(DEVICE_NAME "%u: timeout sending "
			       "root table.\n", xsp->number);
		  dev_unmask_irqs(xsp->dev, mask);
		  return -EIO;
	    }
      }
      dev_unmask_irqs(xsp->dev, mask);

      cancel_time_delay(&xsp->root_timer);

      return 0;
}

/*
 * These function manage the allocation and release of root tables. I
 * need to create a duplicate (with magic numbers correct) in order to
 * make changes, and I need to be able to release tables.
 */
static struct root_table* duplicate_root(struct Instance*xsp)
{
      struct root_table*rp = alloc_page();
      if (rp == 0) {
	    printk(DEVICE_NAME "%u: ERROR ALLOCATING A ROOT PAGE.\n",
		   xsp->number);
	    return 0;
      }
      memcpy(rp, xsp->root, sizeof*rp);
      rp->self = virt_to_bus(rp);
      return rp;
}

static void release_root(struct root_table*rp)
{
      rp->magic = 0x11111111;
      rp->self = 0;
      free_page((unsigned long)rp);
}

static int root_to_board_free(struct Instance*xsp, struct root_table*rp)
{
      int rc;

      rc = root_to_board(xsp, rp->self);
      release_root(xsp->root);
      xsp->root = rp;
      return rc;
}

/*
 * this function is called to cause the channel to be switched to a
 * new id. This means moving the table to a new place in the root
 * table, so the root table must be updated.
 */
static int switch_channel(struct Instance*xsp,
			  struct ChannelData*xpd,
			  unsigned newid)
{
      struct root_table*rt = duplicate_root(xsp);
      if (rt == 0)
	    return -ENOMEM;

      rt->chan[newid].ptr   = rt->chan[xpd->channel].ptr;
      rt->chan[newid].magic = rt->chan[xpd->channel].magic;

      rt->chan[xpd->channel].ptr = 0;
      rt->chan[xpd->channel].magic = 0;

      xpd->channel = newid;

      root_to_board_free(xsp, rt);

      return 0;
}

static long make_and_set_frame(struct Instance*xsp, unsigned id,
			       unsigned long size)
{
      long asize;
      struct root_table*newroot;

      if (xsp->frame[id]) {
	    asize = xsp->frame[id]->page_size * xsp->frame[id]->page_count;

	    if (debug_flag & UCR_TRACE_FRAME)
		  printk(DEVICE_NAME "%u: recycle frame %u "
			 "final size=%u bytes\n", xsp->number, id, asize);

	    return asize;
      }


      if (debug_flag & UCR_TRACE_FRAME)
	    printk(DEVICE_NAME "%u: Make frame %u size=%u bytes\n",
		   xsp->number, id, size);

      asize = ucr_make_frame(xsp, id, size);
      if (asize < 0) {
	    if (debug_flag & UCR_TRACE_FRAME)
		  printk(DEVICE_NAME "%u: Failed (%d) to make frame\n",
			 xsp->number, asize);
	    return asize;
      }

      newroot = duplicate_root(xsp);
      if (newroot == 0) {
	    if (debug_flag & UCR_TRACE_FRAME)
		  printk(DEVICE_NAME "%u: Failed to allocate duplicate root\n",
			 xsp->number);
	    ucr_free_frame(xsp, id);
	    return -ENOMEM;
      }

      newroot->frame_table[id].ptr   = xsp->frame[id]->self;
      newroot->frame_table[id].magic = xsp->frame[id]->magic;
      root_to_board_free(xsp, newroot);

      if (debug_flag & UCR_TRACE_FRAME)
	    printk(DEVICE_NAME "%u: frame %u final size=%u bytes\n",
		   xsp->number, id, asize);

      return asize;
}

static int free_and_set_frame(struct Instance*xsp, unsigned id)
{
      struct root_table*newroot;

      if (!xsp->frame[id])
	    return -EIO;

      if (debug_flag & UCR_TRACE_FRAME)
	    printk(DEVICE_NAME "%u: Free frame %u\n", xsp->number, id);

	/* First tell the ISE board that the frame is gone. */
      if (xsp->root) {
	    newroot = duplicate_root(xsp);
	    newroot->frame_table[id].ptr = 0;
	    newroot->frame_table[id].magic = 0;
	    root_to_board_free(xsp, newroot);
      }

	/* Finally, make the frame really be gone. */
      return ucr_free_frame(xsp, id);
}

static int wait_for_write_ring(struct Instance*xsp, struct ChannelData*xpd,
			       struct ccp_t*ccp)
{
      unsigned long mask;

      if (NEXT_OUT_IDX(xpd->table->next_out_idx) != xpd->table->first_out_idx)
	    return 0;

      mask = dev_mask_irqs(xsp->dev);

      while (NEXT_OUT_IDX(xpd->table->next_out_idx)
		    == xpd->table->first_out_idx) {

	    int rc;

	    if (debug_flag & UCR_TRACE_CHAN)
		  printk(DEVICE_NAME "%u.%u (d): wait for space"
			 " in write ring.\n", xsp->number,
			 xpd->channel);

	    add_to_pending(ccp);

	      /* NT by definition gets an -EINTR here. */
	    dev_unmask_irqs(xsp->dev, mask);
	    return -EINTR;
      }

      dev_unmask_irqs(xsp->dev, mask);

      return 0;
}


/*
 * When the generic part of the ucr driver finds that it must block
 * waiting for more data, it calls this function. It returns -EINTR if
 * the operation is marked pending (and the read should abort).
 */
static int wait_for_read_data(struct Instance*xsp, struct ChannelData*xpd,
			      struct ccp_t*ccp)
{
      int rc = -EINTR;

      const unsigned long mask = dev_mask_irqs(xsp->dev);

      if (CHANNEL_IN_EMPTY(xpd)) {
	    add_to_pending(ccp);
	    dev_unmask_irqs(xsp->dev, mask);
	    return -EINTR;
      }

      dev_unmask_irqs(xsp->dev, mask);

      return 0;
}

/*
 * Flushing a channel causes the pending write buffer to be marked as
 * ready to be sent to the target board. This only blocks on the lack
 * of space on the buffer ring, and does not guarantee that the data
 * has been read by the target board.
 */
static int flush_channel(struct Instance*xsp,
			 struct ChannelData*xpd,
			 struct ccp_t*ccp)
{
      int rc;

      if (xpd->out_off == 0)
	    return 0;

      rc = wait_for_write_ring(xsp, xpd, ccp);
      if (rc == -EINTR)
	    return rc;

	/* Set the count for the buffer that I am working on. */
      xpd->table->out[xpd->table->next_out_idx].count = xpd->out_off;

	/* Initialize the count of the next buffer that I am going to
	   be workin on shortly, and clear my offset pointer. */
      rc = NEXT_OUT_IDX(xpd->table->next_out_idx);
      xpd->table->out[rc].count = PAGE_SIZE;
      xpd->out_off = 0;

	/* Tell the target board that the next_out pointer has
	   moved. This causes the board to notice that the buffer is
	   ready. */
      xpd->table->next_out_idx = rc;
      dev_set_bells(xsp->dev, CHANGE_BELLMASK);

      return 0;
}

static int file_mark_channel(struct Instance*xsp,
			     struct ChannelData*xpd,
			     struct ccp_t*ccp)
{
      int rc;
      rc = flush_channel(xsp, xpd, ccp);
      if (rc == -EINTR)
	    return rc;

      rc = wait_for_write_ring(xsp, xpd, ccp);
      if (rc == -EINTR)
	    return rc;

	/* This sends a file mark by writing a 0 count. The target
	   understands that 0 count to be a file mark. */

      xpd->table->out[xpd->table->next_out_idx].count = 0;
      rc = NEXT_OUT_IDX(xpd->table->next_out_idx);
      xpd->table->out[rc].count = PAGE_SIZE;
      xpd->out_off = 0;

      xpd->table->next_out_idx = rc;
      dev_set_bells(xsp->dev, CHANGE_BELLMASK);
      return 0;
}

/*
 * This function doesn't change what data is pending, it just waits
 * until that data has been consumed by the board. It is useful, for
 * example, when waiting for pending channel to be written before
 * closing that channel.
 */
static int sync_channel(struct Instance*xsp,
			struct ChannelData*xpd,
			struct ccp_t*ccp)
{
      unsigned long mask = dev_mask_irqs(xsp->dev);

      while (xpd->table->first_out_idx != xpd->table->next_out_idx) {

	    if (debug_flag & UCR_TRACE_CHAN)
		  printk(DEVICE_NAME "%u.%u (d): wait for data"
			 " in write ring to be consumed.\n",
			 xsp->number, xpd->channel);

	    add_to_pending(ccp);
	    dev_unmask_irqs(xsp->dev, mask);
	    return -EINTR;
      }

      dev_unmask_irqs(xsp->dev, mask);
      return 0;
}

static long do_sync(struct Instance*xsp,
		    struct ChannelData*xpd,
		    struct ccp_t*ccp)
{
      if (debug_flag & UCR_TRACE_CHAN)
	    printk(DEVICE_NAME "%u.%u (d): Request sync.\n",
		   xsp->number, xpd->channel);

      if (flush_channel(xsp, xpd, ccp) == -EINTR)
	    return -EINTR;

      if (sync_channel(xsp, xpd, ccp) == -EINTR)
	    return -EINTR;

      return 0;
}

/*
 * When a new file is opened, create a ChannelData structure to be
 * associated with that descriptor. The file by default gets channel
 * 0, but the application must somehow assure that there are no two
 * files opened to the same channel, once real I/O occurs.
 *
 * If no other processes have this device open, then try to connect to
 * the ISE board by delivering the root table.
 */
int ucr_open(struct Instance*xsp, struct ChannelData*xpd)
{
      unsigned idx;
      struct root_table*newroot;

      if (xsp->channels == 0) {
	    int rc = root_to_board(xsp, xsp->root->self);
	    if (rc < 0)
		  return rc;
      }

	/* Prevent a duplicate open of channel 0. */
      if (channel_by_id(xsp, 0))
	    return -EBUSY;

      xpd->channel = 0;
      xpd->xsp = xsp;
      xpd->in_off = 0;
      xpd->out_off = 0;

      xpd->read_timeout = UCRX_TIMEOUT_OFF;
      xpd->read_timeout_flag = 0;
	/*xpd->read_timing = 0;*/

      xpd->table = alloc_memory(sizeof(*xpd->table));

      xpd->table->magic = CHANNEL_TABLE_MAGIC;
      xpd->table->self  = virt_to_bus(xpd->table);
      xpd->table->frame = 0;
      xpd->table->first_out_idx = 0;
      xpd->table->next_out_idx  = 0;
      xpd->table->first_in_idx  = 0;
      xpd->table->next_in_idx   = 0;

      for (idx = 0 ;  idx < CHANNEL_IBUFS ;  idx += 1) {
	    xpd->in[idx] = alloc_page();
	    xpd->table->in[idx].ptr = virt_to_bus(xpd->in[idx]);
	    xpd->table->in[idx].count = PAGE_SIZE;
      }

      for (idx = 0 ;  idx < CHANNEL_OBUFS ;  idx += 1) {
	    xpd->out[idx] = alloc_page();
	    xpd->table->out[idx].ptr = virt_to_bus(xpd->out[idx]);
	    xpd->table->out[idx].count = PAGE_SIZE;
      }


	/* Put the channel information into the channel list so that
	   arriving packets can be properly dispatched to the
	   process. */

      if (xsp->channels == 0) {
	    xsp->channels = xpd;
	    xpd->next = xpd;
	    xpd->prev = xpd;
      } else {
	    xpd->next = xsp->channels;
	    xpd->prev = xsp->channels->prev;
	    xpd->next->prev = xpd;
	    xpd->prev->next = xpd;
      }

	/* Update the root table to contain the new channel. This
	   involves some communication with the target board, so there
	   is opportunity for blocking here. */
      newroot = duplicate_root(xsp);
      newroot->chan[0].magic = xpd->table->magic;
      newroot->chan[0].ptr   = xpd->table->self;
      root_to_board_free(xsp, newroot);

      return 0;
}

/*
 * Release the file. Take the ChannelData structure out of the channel
 * list and return all the unread buffers.
 */

void ucr_release(struct Instance*xsp, struct ChannelData*xpd)
{
      unsigned idx;
      struct root_table*newroot;


	/* Remove the channel from the root table that the ISE board
	   is using. Do this early so that I am free to clean up the
	   channel table afterwords. */

      newroot = duplicate_root(xsp);
      newroot->chan[xpd->channel].magic = 0;
      newroot->chan[xpd->channel].ptr = 0;
      root_to_board_free(xsp, newroot);

      if (debug_flag&UCR_TRACE_CHAN)
	    printk(DEVICE_NAME "%u.%u (d): releasing buffers\n",
		   xsp->number, xpd->channel);

      for (idx = 0 ;  idx < CHANNEL_IBUFS ;  idx += 1) {
	    free_page((unsigned long)xpd->in[idx]);
      }

      for (idx = 0 ;  idx < CHANNEL_OBUFS ;  idx += 1) {
	    free_page((unsigned long)xpd->out[idx]);
      }


	/* No more communication with the target channel, so remove
	   the ChannelData structure from the channel list, remove
	   buffers, and release the xpd object. */

      if (xsp->channels == xpd)
	    xsp->channels = xsp->channels->next;
      if (xsp->channels == xpd)
	    xsp->channels = 0;
      else {
	    xpd->prev->next = xpd->next;
	    xpd->next->prev = xpd->prev;
      }

      xpd->table->magic = 0;
      xpd->table->self = 0;
      free_memory(xpd->table, sizeof(*xpd->table));

      if (debug_flag&UCR_TRACE_CHAN)
	    printk(DEVICE_NAME "%u.%u (d): channel closed.\n",
		   xsp->number, xpd->channel);

      if (xsp->channels == 0)
	    root_to_board(xsp, 0);
}

/*
 * Read works by waiting for at least one packet to be dispatched to
 * the channel. I then copy the bytes out of the packet into the
 * reader's buffer, and as I drain packets return them to the read
 * pool.
 *
 * If the block_flag is true, then this read will block until at least
 * some bytes are transferred. Otherwise, do not block.
 */
long ucr_read(struct Instance*xsp, struct ChannelData*xpd, struct ccp_t*ccp,
	      char *bytes, unsigned long count, int block_flag)
{
      unsigned tcount = count;

      if (debug_flag & UCR_TRACE_CHAN)
	    printk(DEVICE_NAME
		   "%u.%u (d): ucr_read %u bytes, block=%s\n",
		   xsp->number, xpd->channel, count,
		   block_flag?"true":"false");


      while (tcount > 0) {

	    unsigned long mask;
	    unsigned trans, siz;
	    void*buf;

	    if (CHANNEL_IN_EMPTY(xpd)) {

		    /* If I transferred at least a few bytes to the
		       caller, then that is good enough. Don't block. */
		  if (tcount < count)
			break;

		    /* If this re-run is caused by a timeout,
		       don't block. */
		  if (xpd->read_timeout_flag)
			goto read_timeout;

		    /* I'm about to block. If the caller doesn't want
		       that, then break from the read loop here. */
		  if (!block_flag)
			break;

		    /* It is prudent before blocking on a read to
		       flush the write buffer. I may be blocking on a
		       response to something in the write buffer! */
		  if (flush_channel(xsp, xpd, ccp) == -EINTR)
			return -EINTR;

		    /* Finally, block. In truth, this marks the IRP
		       pending and returns -EINTR. The ucr_read will
		       be called again in the future to continue the
		       read operation. */
		  if (wait_for_read_data(xsp, xpd, ccp) == -EINTR)
			return -EINTR;
	    }

	    buf = xpd->in[xpd->table->first_in_idx];
	    siz = xpd->table->in[xpd->table->first_in_idx].count;

	    trans = tcount;
	    if ((trans + xpd->in_off) > siz)
		  trans = siz - xpd->in_off;

	    if (debug_flag & UCR_TRACE_CHAN)
		  printk(DEVICE_NAME
			 "%u.%u (d): read %u of %u bytes "
			 "[in_off=%u]\n", xsp->number,
			 xpd->channel, trans, siz, xpd->in_off);

	    copy_to_user(bytes, (char*)buf + xpd->in_off, trans);

	    tcount -= trans;
	    bytes += trans;
	    xpd->in_off += trans;

	      /* If I get to the end of a buffer, release the current
		 one and notify the ISE board that the channel has
		 changed. This may get me more read buffers. */
	    if (xpd->in_off == siz) {
		  xpd->in_off = 0;
		  xpd->table->in[xpd->table->first_in_idx].count = PAGE_SIZE;
		  INCR_IN_IDX(xpd->table->first_in_idx);
		  dev_set_bells(xsp->dev, CHANGE_BELLMASK);
	    }
      }

 read_timeout:
      xpd->read_timeout_flag = 0;

	/* Calculate the return value. */
      count = count - tcount;

      if (debug_flag & UCR_TRACE_CHAN)
	    printk(DEVICE_NAME "%u.%u (d): ucr_read complete %d bytes\n",
		   xsp->number, xpd->channel, count);


      return count;

}


/*
 * The write works by waiting for the previous DMA write to finish,
 * then starting a new one for this data. The DMA operation may
 * proceed without me waiting around in the write, but I can't start a
 * new one until the DMA completes. For this to work, the mark bit 0
 * must start out as 1.
 */

long ucr_write(struct Instance*xsp, struct ChannelData*xpd, struct ccp_t*ccp,
	       const char*bytes, const unsigned long count)
{
      int rc;
      unsigned tcount = count;

      if (debug_flag & UCR_TRACE_CHAN)
	    printk(DEVICE_NAME "%u.%u (d): ucr_write %u bytes\n",
		   xsp->number, xpd->channel, count);

      while (tcount > 0) {
	    unsigned trans = tcount;
	    void*buf;
	    __u32 bus;
	    unsigned idx;

	    idx = xpd->table->next_out_idx;
	    buf = xpd->out[idx];

	    if ((xpd->out_off + trans) > xpd->table->out[idx].count)
		  trans = xpd->table->out[idx].count - xpd->out_off;

	    if (debug_flag & UCR_TRACE_CHAN)
		  printk(DEVICE_NAME "%u.%u (d): write %u bytes to out_ptr=%u"
			 "(buf=0x%x) offset=%u\n", xsp->number,
			 xpd->channel, trans, idx, buf, xpd->out_off);

	    copy_from_user((char*)buf + xpd->out_off, bytes, trans);

	    xpd->out_off += trans;
	    tcount -= trans;
	    bytes  += trans;

	      /* If the current buffer is full, then send it to the
		 ISE board for reading. Block only if I am so far
		 ahead that there is no place in the circular buffer
		 for this message. */
	    if (xpd->out_off == xpd->table->out[idx].count) {

		  if (tcount < count)
			return count - tcount;

		  if (flush_channel(xsp, xpd, ccp) == -EINTR)
			return -EINTR;
	    }
      }


      if (debug_flag & UCR_TRACE_CHAN)
	    printk(DEVICE_NAME "%u.%u (d): ucr_write complete %d bytes\n",
		   xsp->number, xpd->channel, count);

      return count;
}

int ucr_ioctl(struct Instance*xsp, struct ChannelData*xpd, struct ccp_t*ccp,
	      unsigned int cmd, unsigned long arg)
{
      int rc;

      switch (cmd) {

	  case UCR_FLUSH:
	    if (debug_flag & UCR_TRACE_CHAN)
		  printk(DEVICE_NAME "%u.%u (d): Request flush.\n",
			 xsp->number, xpd->channel);

	    return flush_channel(xsp, xpd, ccp);

	  case UCR_SYNC:
	    return do_sync(xsp, xpd, ccp);

	  case UCR_CHANNEL:
	    if (debug_flag & UCR_TRACE_CHAN)
		  printk(DEVICE_NAME "%u.%u (d): switch to channel %u\n",
			 xsp->number, xpd->channel, arg);

	    if (arg >= ROOT_TABLE_CHANNELS) return -EINVAL;
	    if (xpd->channel == arg) return 0;
	    if (channel_by_id(xsp, (__u16)arg)) return -EBUSY;
	    flush_channel(xsp, xpd, ccp);
	    sync_channel(xsp, xpd, ccp);
	    switch_channel(xsp, xpd, arg);

	    if (debug_flag & UCR_TRACE_CHAN)
		  printk(DEVICE_NAME "%u.%u (d): switch complete\n",
			 xsp->number, xpd->channel);

	    return 0;

	  case UCR_ATTENTION:
	    if (debug_flag & UCR_TRACE_CHAN)
		  printk(DEVICE_NAME "%u.%u (d): software attention %u\n",
			 xsp->number, xpd->channel, arg);

	    printk("XXXX Attention not implemented\n");
	    return -ENOSYS;

	  case UCR_SEND_FILE_MARK:
	    if (debug_flag & UCR_TRACE_CHAN)
		  printk(DEVICE_NAME "%u.%u (d): send file mark\n",
			 xsp->number, xpd->channel);

	    return file_mark_channel(xsp, xpd, ccp);

	  case UCR_MAKE_FRAME: {
		unsigned id = 0xf & (arg >> 28);
		return make_and_set_frame(xsp, id, arg&0x0fffffff);
	  }

	  case UCR_FREE_FRAME: {
		unsigned id = 0xf & (arg >> 28);
		return free_and_set_frame(xsp, id);
	  }

	  case UCR_SEND_FRAME: {
		unsigned id = 0xf & (arg >> 28);
		printk("XXXX send frame not implemented.\n");
		return -ENOSYS;
	  }

      }

      return -ENOTTY;
}


int ucr_irq(struct Instance*xsp)
{
      unsigned long mask = dev_get_bells(xsp->dev);

	/* This bell happens when the target board responds to my
	   changing the root table. */
      if (mask & ROOT_TABLE_BELLMASK)
	    wake_up(&xsp->root_sync);

	/* This bell happens when the target board responds to my
	   sending a status signal. */
      if (mask & STATUS_BELLMASK)
	    ;

	/* This bell happens when it tells me that *it* has changed a
	   channel table. */
      if (mask & CHANGE_BELLMASK)
	    wake_up(&xsp->dispatch_sync);

      return mask != 0;
}

/*
 * $Log$
 * Revision 1.3  2001/07/12 20:31:05  steve
 *  Support UCRX_TIMEOUT_FORCE
 *
 * Revision 1.2  2001/04/03 01:56:05  steve
 *  Simplify the code path for pending operations, and
 *  use buffered I/O instead of direct.
 *
 * Revision 1.1  2001/03/05 20:11:40  steve
 *  Add NT4 driver to ISE source tree.
 *
 * Revision 1.15  2000/06/26 22:07:45  steve
 *  Close some channel races.
 *
 * Revision 1.14  2000/02/18 19:40:55  steve
 *  More channel diagnostics.
 *
 * Revision 1.13  1999/07/15 16:37:37  steve
 *  isolate read and write activities under NT.
 *
 * Revision 1.12  1999/04/07 01:48:28  steve
 *  More frame tracing.
 *
 * Revision 1.11  1999/04/02 00:27:45  steve
 *  IRQ sharing fix for NT.
 *
 * Revision 1.10  1998/12/02 00:52:29  steve
 *  Improved frame handling and debugging.
 *
 * Revision 1.9  1998/09/23 23:12:57  steve
 *  Better detect diagnose broken interrupts.
 *
 * Revision 1.8  1998/08/14 21:59:51  steve
 *  Get rid of warning.
 *
 * Revision 1.7  1998/07/16 22:50:05  steve
 *  Detailed frame test prints.
 *
 * Revision 1.6  1998/07/10 22:54:23  steve
 *  Work around HalTranslateBusAddress bug.
 *
 * Revision 1.5  1998/05/30 01:55:49  steve
 * lose test print.
 *
 * Revision 1.4  1998/05/30 01:49:52  steve
 *  Handle startup after reset.
 *
 * Revision 1.3  1998/05/29 18:11:22  steve
 *  Proper cancel/abort behavior for NT.
 *
 * Revision 1.2  1998/05/28 22:53:02  steve
 *  NT port.
 *
 * Revision 1.1  1998/05/26 16:16:34  steve
 *  New channel protocol
 *
 */
