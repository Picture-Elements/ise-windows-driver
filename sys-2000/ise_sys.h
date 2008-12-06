#ifndef __ise_sys_H
#define __ise_sys_H
/*
 * Copyright (c) 2001 Picture Elements, Inc.
 *    Stephen Williams (steve@picturel.com)
 *
 * $Id$
 */

# include  <wdm.h>
# include  "ucrif.h"
# include  "ise_tables.h"

struct instance_t;
struct channel_t;
typedef NTSTATUS (*callback_t)(struct instance_t*xsp, IRP*irp);
typedef void  (*vcallback_t)(struct instance_t*xsp, IRP*irp);


/* TheISE driver is the communication path into the target
   board. There is one of these for each ISE board detected. */

#define FILE_DEVICE_ISE  0x8001
extern DEVICE_OBJECT* create_ise(DRIVER_OBJECT*drv, unsigned dev_no);
extern void remove_ise(DEVICE_OBJECT*fdo);

extern NTSTATUS pnp_start_ise(DEVICE_OBJECT*fdo, IRP*irp);
extern void     pnp_stop_ise(DEVICE_OBJECT*fdo);

extern NTSTATUS dev_create(DEVICE_OBJECT*fdo, IRP*irp);
extern NTSTATUS dev_close(DEVICE_OBJECT*fdo, IRP*irp);
extern NTSTATUS dev_ioctl(DEVICE_OBJECT*fdo, IRP*irp);
extern NTSTATUS dev_read(DEVICE_OBJECT*fdo, IRP*irp);
extern NTSTATUS dev_write(DEVICE_OBJECT*fdo, IRP*irp);


/*
 * The channel pointer is stored in the FsContext of an open file
 * handle, so that subsequent operations can figure out the channel
 * from the IRP.
 */
#define get_channel(irp) ((struct channel_t*)(IoGetCurrentIrpStackLocation(irp)->FileObject->FsContext))

extern struct channel_t* channel_by_id(struct instance_t*xsp,
				       unsigned short id);

/* The ISEX driver is a control path into the target board. Only the
   ioctl operation is supported here in general. */

#define FILE_DEVICE_UCRX 0x8003

extern DEVICE_OBJECT* create_isex(DRIVER_OBJECT*drv, struct instance_t*xsp);
extern void remove_isex(DEVICE_OBJECT*fdx);

extern NTSTATUS isex_cleanup(DEVICE_OBJECT*dev, IRP*irp);
extern NTSTATUS isex_ioctl(DEVICE_OBJECT*dev, IRP*irp);
extern NTSTATUS isex_run_program(DEVICE_OBJECT*dev, IRP*irp);



/*
 * Parts of the ise device use these functions to manage root tables
 * for the ISE board.
 *
 * duplicate_root
 *    Create a new root table that can be edited by the caller and
 *    passed to the board later.
 *
 * root_to_board
 *    Pass a new root table to the ISE board. The fun parameter is a
 *    callback to be invoked when the target board receives the table.
 *
 * flush_channel
 *    Cause the current buffer to be handed off to the device, whether
 *    it is full or not. If the write ring is full, this will
 *    block. At any rate, the callback will be called when there is
 *    space and the flush is completed.
 *
 *    If the IRP is cancelled, the cancel callback is called
 *    instead. It is up to the caller of flush_channel to supply a
 *    cancel routine that completes the IRP.
 *
 *    This uses IRP DriverContext members 0, 1, 2 and 3.
 *
 * complete_success
 *    A commonly used callback, that just completes the IRP.
 */

struct root_table*duplicate_root(struct instance_t*xsp, PHYSICAL_ADDRESS*ptrl);
extern NTSTATUS root_to_board(struct instance_t*xsp, IRP*irp,
			      struct root_table*root, PHYSICAL_ADDRESS rootl,
			      callback_t fun, callback_t timeout_fun);

extern NTSTATUS flush_channel(struct instance_t*xsp, IRP*irp,
			      callback_t callback,
			      vcallback_t cancel);

extern NTSTATUS complete_success(struct instance_t*xsp, IRP*irp);


/* The ISE driver has a single console device (independent of the
   number of physical ISE boards) that is used to carry driver debug
   messages. */

#define FILE_DEVICE_ISE_CONS 0x8002
extern NTSTATUS create_console(DRIVER_OBJECT*drv);
extern void unload_console(DRIVER_OBJECT*drv);

extern NTSTATUS cons_read(DEVICE_OBJECT*dev, IRP*irp);
extern void printk(const char*fmt, ...);

extern unsigned long debug_flag;

struct irp_counter {
      unsigned scheduled;
      unsigned complete;
      unsigned cancelled;
};

      
/*
 * The device driver creates a DEVICE_OBJECT, an "fdo" object, to
 * carry all the device specific support for the hardware. Included in
 * that fdo is a DeviceExtension that is described here.
 */
struct instance_t {
	/* ID for the device this ID refers to. */
      unsigned id;
	/* This is the device object that the PnP beast creates. */
      DEVICE_OBJECT*pdo;
	/* This is the device object for the board. */
      DEVICE_OBJECT*fdo;
	/* This is used to keep track of the fdo in the dev stack */
      DEVICE_OBJECT*next_dev;
	/* This is the device object for the isex node. */
      DEVICE_OBJECT*fdx;
        /* Device operations */
      const struct ise_ops_tab*dev_ops;
	/* This is the mapped bar for the device. */
      void*bar0;
      unsigned bar0_size;
	/* These are our reference to the interrupt. */
      PKINTERRUPT irq;
      KIRQL irql;
      unsigned ivec;
      KAFFINITY affinity;
	/* Cookie for allocating common memory. */
      DMA_ADAPTER*dma;

	/* Mutex for multi-processor synchronization */
      KSPIN_LOCK mutex;

	/* The root table pointer for the device is saved here. Save
	   the physical address and the kernel virtual address.*/
      PHYSICAL_ADDRESS rootl;
      struct root_table*root;

	/* This is a root table buffer that is on standby. Save ones
	   that I've freed, so that it can be reused. */
      PHYSICAL_ADDRESS rootl_standby;

      struct root_table*root_standby;
      unsigned root_standby_leak;

      unsigned char config_buf[4];

	/* These members hold the context of the root transfer. This
	   implies that there is only one pending root transfer active
	   at a time. */
      IRP*root_to_board_pending;
      KDPC root_to_board_dpc;
      PHYSICAL_ADDRESS rootl2;
      struct root_table*root2;
      IRP*root_irp;
      callback_t root_callback;

	/* Root table manipulations take limited time. */
      KTIMER root_timer;
      KDPC root_timer_dpc;
      callback_t root_timeout_callback;


	/* Manage IRPs blocked on write space with these members. The
	   pending_write_irps list holds a queue of IRPS waiting for
	   write space, and the pending_write_dpc schedules irps that
	   get that space. */
      LIST_ENTRY pending_write_irps;
      KDPC pending_write_dpc;
      KSPIN_LOCK pending_write_sync;

      LIST_ENTRY pending_read_irps;
      KDPC pending_read_dpc;
      KSPIN_LOCK pending_read_sync;
      struct irp_counter pending_read_count;

	/* Keep information about the frames for this device.

	   The frame_tab is a virtual address to the actual frame
	   table. This table in turn contains the bus addresses of the
	   pages of the frame. The pages are all PAGE_SIZE bytes. The
	   bus address of the frame_tab is stored in the root table.

	   The frame_mdl_irp is the irp of the DeviceIoControl that
	   created the mapping. This irp is held so that it can be
	   cancelled after an unmap_unmake, or when the device is
	   closed.  The irp->MdlAddress is the Mdl for the frame. */
      struct frame_table*frame_tab[16];
      IRP               *frame_mdl_irp[16];
      DMA_ADAPTER       *frame_dma[16];
      IRP               *frame_wait_irp[16];

      int                frame_done_flags;
      int                frame_cleanup_mask;
      PIO_WORKITEM       frame_cleanup_work;

	/* Keep a list of the currently open channels. */
      struct channel_t *channels;

      struct channel_t *channel_standby_list;
};

/*
 * One of these exists for each open channel on the ISE board. They
 * are kept listed in the instance_t object for the device.
 */
struct channel_t {
	/* Number for this channel */
      unsigned short channel;
	/* Back-pointer to the device. */
      struct instance_t*xsp;

	/* This is the read timeout, in milliseconds. */
      long read_timeout;
	/* The actual timer for read timeouts. */
      KTIMER read_timer;
	/* The DPC to handle read timeouts. */
      KDPC read_timer_dpc;
	/* This is the IRP that is waiting for a read. */
      IRP*volatile read_pending;
      volatile unsigned read_pstate;

	/* This is the table that represents the channel for the ISE
	   board and the driver. */
      PHYSICAL_ADDRESS tablel;
      struct channel_table*table;

	/* Input and Output buffers */
      struct {
	    PHYSICAL_ADDRESS ptrl;
	    unsigned char*ptr;
      } in[CHANNEL_IBUFS], out[CHANNEL_OBUFS];

      unsigned in_off, out_off;

      struct channel_t *next, *prev;
};

struct ise_ops_tab {
      const char*full_name;

      void (*init_hardware)(struct instance_t*xsp);
      void (*clear_hardware)(struct instance_t*xsp);
      unsigned long (*mask_irqs)(struct instance_t*xsp);
      void (*unmask_irqs)(struct instance_t*xsp, unsigned long mask);
      void (*set_root_table_base)(struct instance_t*xsp, __u32 value);
      void (*set_root_table_resp)(struct instance_t*xsp, __u32 value);
      __u32 (*get_root_table_resp)(struct instance_t*xsp);
      __u32 (*get_status_resp)(struct instance_t*xsp);
      void (*set_status_value)(struct instance_t*xsp, __u32 value);

      void (*set_bells)(struct instance_t*xsp, unsigned long mask);
      unsigned long (*get_bells)(struct instance_t*xsp);

	/* Diagnose functions. */
      void (*diagnose0_dump)(struct instance_t*xsp);
      void (*diagnose1_dump)(struct instance_t*xsp);
};

extern const struct ise_ops_tab ise_operations;
extern const struct ise_ops_tab jse_operations;
extern const struct ise_ops_tab ejse_operations;

# define dev_clear_hardware(xsp)   (xsp)->dev_ops->clear_hardware(xsp)
# define dev_init_hardware(xsp)    (xsp)->dev_ops->init_hardware(xsp)
# define dev_mask_irqs(xsp)        (xsp)->dev_ops->mask_irqs(xsp)
# define dev_unmask_irqs(xsp,mask) (xsp)->dev_ops->unmask_irqs(xsp, mask)
# define dev_set_root_table_base(xsp,val) (xsp)->dev_ops->set_root_table_base(xsp,val)
# define dev_set_root_table_resp(xsp,val) (xsp)->dev_ops->set_root_table_resp(xsp,val)
# define dev_get_root_table_resp(xsp)  (xsp)->dev_ops->get_root_table_resp(xsp)
# define dev_get_status_resp(xsp)      (xsp)->dev_ops->get_status_resp(xsp)
# define dev_set_status_value(xsp,val) (xsp)->dev_ops->set_status_value(xsp,val)

# define dev_get_bells(xsp)      (xsp)->dev_ops->get_bells(xsp)
# define dev_set_bells(xsp,mask) (xsp)->dev_ops->set_bells(xsp,mask)

# define ROOT_TABLE_BELLMASK 0x01
# define STATUS_BELLMASK     0x02
# define CHANGE_BELLMASK     0x04


/*
 * These are functions for manipulating frames.
 */
#if 0
extern void ise_free_frame(struct instance_t*xsp, unsigned fidx);
extern unsigned long ise_make_frame(struct instance_t*xsp, unsigned fidx,
				    unsigned long frame_size);
#endif
extern int ise_map_frame(struct instance_t*xsp, unsigned fidx, MDL*mdl);
extern void ise_unmap_frame(struct instance_t*xsp, unsigned fidx);

/*
 * These are DPCs that are extern so that the initialization code can
 * make the KDPC objects.
 */
extern void pending_write_dpc(KDPC*dpc, void*ctx, void*arg1, void*arg2);
extern void pending_read_dpc(KDPC*dpc, void*ctx, void*arg1, void*arg2);
extern void read_timeout(KDPC*dpc, void*ctx, void*arg1, void*arg2);

/*
 * $Log$
 * Revision 1.15  2008/12/06 03:27:08  steve
 *  Add EJSE support.
 *
 * Revision 1.14  2005/07/26 01:17:32  steve
 *  New method of mapping frames for version 2.5
 *
 * Revision 1.13  2005/04/30 03:00:43  steve
 *  Put timeout on root-to-board operations.
 *
 * Revision 1.12  2004/07/15 04:19:26  steve
 *  Extend to support JSE boards.
 *
 * Revision 1.11  2002/06/14 16:09:29  steve
 *  spin locks around root table manipulations.
 *
 * Revision 1.10  2002/04/11 00:49:30  steve
 *  Move FreeCommonBuffers to PASSIVE_MODE using standby lists.
 *
 * Revision 1.9  2001/09/28 18:09:53  steve
 *  Create a per-device mutex to manage multi-processor access
 *  to the instance object.
 *
 *  Fix some problems with timeout handling.
 *
 *  Add some diagnostic features for tracking down locking
 *  or delay problems.
 *
 * Revision 1.8  2001/09/06 22:53:56  steve
 *  Flush can be cancelled.
 *
 * Revision 1.7  2001/09/06 18:28:43  steve
 *  Read timeouts.
 *
 * Revision 1.6  2001/09/05 22:05:55  steve
 *  protect mappings from misused or forgotten unmaps.
 *
 * Revision 1.5  2001/09/04 02:47:09  steve
 *  Add frame allocate/free/map/unmap controls.
 *
 * Revision 1.4  2001/08/14 22:25:30  steve
 *  Add SseBase device
 *
 * Revision 1.3  2001/08/03 17:39:41  steve
 *  Use status method to run programs.
 *
 * Revision 1.2  2001/07/30 21:32:42  steve
 *  Rearrange the status path to follow the return codes of
 *  the callbacks, and preliminary implementation of the
 *  RUN_PROGRAM ioctl.
 *
 * Revision 1.1  2001/07/26 00:31:30  steve
 *  Windows 2000 driver.
 *
 */
#endif
