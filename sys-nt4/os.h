#ifndef __os_winnt_H
#define __os_winnt_H
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
#if !defined(WINNT)
#ident "$Id$"
#endif

#include  <ntddk.h>
#include  "ntmsg.h"

extern void wake_up(struct wait_queue**);

/* MS products to not seem to support inline. */
# define __inline__

/* MS products deal with processor compatibility by defining them to
   all be 32bit ix86. Thus, specific size types are kinda obvious. */
typedef unsigned long  __u32;
typedef unsigned short __u16;

struct timer_list {
      KTIMER timer;
      KDPC dpc;
      void (*fun)(unsigned long);
      unsigned long arg;
};


extern void* alloc_memory(unsigned long s);
#define free_memory(m,s) MmFreeContiguousMemory((m))

#define alloc_page() alloc_memory(PAGE_SIZE)
#define free_page(x) free_memory((void*)(x),PAGE_SIZE)

#define virt_to_bus(x) ((MmGetPhysicalAddress((void*)(x))).LowPart)

/* Define some compatibility constants. */
# define EBUSY 1
# define EINTR 2
# define EINVAL 3
# define ENOTTY 4
# define EIO 5
# define ENOMEM 6
# define EAGAIN 7
# define ENOSYS 8

#define readl(off) READ_REGISTER_ULONG((unsigned long*)(off))
#define writel(val,off) WRITE_REGISTER_ULONG((unsigned long*)(off),(val))
#define memcpy_fromio(b,r,c) READ_REGISTER_BUFFER_UCHAR((r),(b),(c))


/* The various support in the winnt stuff arranges for the memory in
   all cases to be mapped into kernel virtual address space. Thus,
   copying is rather simple. */
#define copy_to_user(d,s,c) RtlCopyMemory((d), (s), (c))
#define copy_from_user(d,s,c) RtlCopyMemory((d),(s),(c))

/* These are version of the BIOS32 functions for NT */
extern int pcibios_read_config_dword(unsigned char bus, unsigned char dev_fn,
				     unsigned char off, unsigned*val);
extern int pcibios_read_config_word(unsigned char bus, unsigned char dev_fn,
				    unsigned char off, unsigned short*val);
extern int pcibios_read_config_byte(unsigned char bus, unsigned char dev_fn,
				    unsigned char off, unsigned char*val);
extern int pcibios_write_config_dword(unsigned char bus, unsigned char dev_fn,
				      unsigned char off, unsigned val);
extern int pcibios_write_config_byte(unsigned char bus, unsigned char dev_fn,
				     unsigned char off, unsigned char val);
# define PCIBIOS_SUCCESSFUL 0x00
# define PCIBIOS_DEVICE_NOT_FOUND 0x86
# define PCIBIOS_BAD_REGISTER_NUMBER 0x87

struct counters {
      unsigned long read, read_pend;
      unsigned long write, write_pend;
      unsigned long sleep, sleep_pend;
};

/* The win nt version of the driver needs these members stored in the
   instance to remember the irq information for later unload, and also
   needs to remember if MmMap... was used to map the registers. */
#define OS_INST_MEMBERS \
    int dev_is_mmapped; \
    KIRQL irql; \
    PKINTERRUPT irq; \
    KSPIN_LOCK pending_lock; \
    LIST_ENTRY pending_read_irps, pending_write_irps,  pending_ioctl_irps; \
    LIST_ENTRY tmp_queue;\
    KDPC pending_io_dpc; \
    struct counters cnt; \
    struct FrameCell*fb[16];

struct ccp_t {
      IRP*irp;
      LIST_ENTRY*pend_list;
      BOOLEAN pend_flag;
};

#define OS_CHAN_MEMBERS \
    unsigned long block_flag; \
    struct ccp_t ccp_read, ccp_write, ccp_ioctl; \
    struct counters cnt;

/* Take the device_fn of the i960RP ATU and generate the matching
   device_fn for the bridge. This works by subtracting 1 from the
   function unit. */
#define i960rp_to_bridge(x) ((x)-0x20)

/* This is the type code for the device object that represents the
   physical board. Instance structures are connected to these devices. */
#define FILE_DEVICE_ISE  0x8001
extern NTSTATUS create_device(DRIVER_OBJECT*drv, unsigned bus_no,
			      unsigned slot, unsigned num);
extern void unload_device(DRIVER_OBJECT*drv, DEVICE_OBJECT*dev);

extern NTSTATUS dev_create(DEVICE_OBJECT*dev, IRP*irp);
extern NTSTATUS dev_close(DEVICE_OBJECT*dev, IRP*irp);
extern NTSTATUS dev_read(DEVICE_OBJECT*dev, IRP*irp, BOOLEAN retry_flag);
extern NTSTATUS dev_write(DEVICE_OBJECT*dev, IRP*irp, BOOLEAN retry_flag);
extern NTSTATUS dev_ioctl(DEVICE_OBJECT*dev, IRP*irp, BOOLEAN retry_flag);

/* The UCRX driver is a control path into the target board. Only the
   ioctl operation is supported here in general. */
#define FILE_DEVICE_UCRX 0x8003
extern NTSTATUS create_devx(DRIVER_OBJECT*drv, struct Instance*xsp);
extern void unload_devx(DEVICE_OBJECT*dev);

extern NTSTATUS devx_ioctl(DEVICE_OBJECT*dev, IRP*irp);
extern void devx_diag1(struct Instance*xsp);


/* The ISE driver has a single console device (independent of the
   number of physical ISE boards) that is used to carry driver debug
   messages. */
#define FILE_DEVICE_ISE_CONS 0x8002
extern NTSTATUS create_console(DRIVER_OBJECT*drv);

/* The console device has these dispatch routines, and support the
   debug output operations. These definitions relate to that
   functionality. */

extern void unload_console(DEVICE_OBJECT*dev);
extern NTSTATUS cons_read(DEVICE_OBJECT*dev, IRP*irp);

extern void printk(const char*fmt, ...);

/* This function initializes the NT frame pool with the given size. The frame
   pool is preallocated because NT is unlikely to give me the space othersize. */
extern int ucr_init_nt_frame_pool(unsigned long size);
extern void ucr_deinit_nt_frame_pool();


/*
 * $Log$
 * Revision 1.2  2001/04/03 01:56:05  steve
 *  Simplify the code path for pending operations, and
 *  use buffered I/O instead of direct.
 *
 * Revision 1.1  2001/03/05 20:11:40  steve
 *  Add NT4 driver to ISE source tree.
 *
 * Revision 1.11  1999/08/18 00:05:59  steve
 *  Fix SMP synchronization problem.
 *
 * Revision 1.10  1999/07/15 16:37:37  steve
 *  isolate read and write activities under NT.
 */


#endif
