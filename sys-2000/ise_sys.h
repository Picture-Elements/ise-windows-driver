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
typedef void (*callback_t)(struct instance_t*xsp, IRP*irp);


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

extern NTSTATUS isex_ioctl(DEVICE_OBJECT*dev, IRP*irp);



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
 * complete_success
 *    A commonly used callback, that just completes the IRP.
 */

struct root_table*duplicate_root(struct instance_t*xsp, PHYSICAL_ADDRESS*ptrl);
extern void root_to_board(struct instance_t*xsp, IRP*irp,
			  struct root_table*root, PHYSICAL_ADDRESS rootl,
			  callback_t fun);

extern NTSTATUS flush_channel(struct instance_t*xsp, IRP*irp,
			      callback_t callback);

extern void complete_success(struct instance_t*xsp, IRP*irp);


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
	/* This is used to keep track of the fdo in the dev stack */
      DEVICE_OBJECT*next_dev;
	/* This is the device object for the isex node. */
      DEVICE_OBJECT*fdx;
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

	/* The root table pointer for the device is saved here. Save
	   the physical address and the kernel virtual address.*/
      PHYSICAL_ADDRESS rootl;
      struct root_table*root;

	/* These members hold the context of the root transfer. This
	   implies that there is only one pending root transfer active
	   at a time. */
      IRP*root_to_board_pending;
      KDPC root_to_board_dpc;
      PHYSICAL_ADDRESS rootl2;
      struct root_table*root2;
      IRP*root_irp;
      callback_t root_callback;


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

	/* Keep a list of the currently open channel. */
      struct channel_t *channels;
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


extern void dev_init_hardware(struct instance_t*xsp);
extern void dev_clear_hardware(struct instance_t*xsp);

extern void dev_set_root_table_base(struct instance_t*xsp, __u32 value);
extern void dev_set_root_table_resp(struct instance_t*xsp, __u32 value);

extern unsigned long dev_get_root_table_resp(struct instance_t*xsp);

# define ROOT_TABLE_BELLMASK 0x01
# define STATUS_BELLMASK     0x02
# define CHANGE_BELLMASK     0x04

extern unsigned long dev_get_bells(struct instance_t*xsp);
extern void dev_set_bells(struct instance_t*xsp, unsigned long mask);

extern unsigned long dev_mask_irqs(struct instance_t*xsp);
extern void dev_unmask_irqs(struct instance_t*xsp, unsigned long mask);


/*
 * These are DPCs that are extern so that the initialization code can
 * make the KDPC objects.
 */
extern void pending_write_dpc(KDPC*dpc, void*ctx, void*arg1, void*arg2);
extern void pending_read_dpc(KDPC*dpc, void*ctx, void*arg1, void*arg2);

/*
 * $Log$
 * Revision 1.1  2001/07/26 00:31:30  steve
 *  Windows 2000 driver.
 *
 */
#endif
