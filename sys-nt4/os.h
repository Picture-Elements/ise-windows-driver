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
#define free_page(x) free_memory((x),PAGE_SIZE)

#define virt_to_bus(x) ((MmGetPhysicalAddress((x))).LowPart)

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
 * Revision 1.1  2001/03/05 20:11:40  steve
 *  Add NT4 driver to ISE source tree.
 *
 * Revision 1.11  1999/08/18 00:05:59  steve
 *  Fix SMP synchronization problem.
 *
 * Revision 1.10  1999/07/15 16:37:37  steve
 *  isolate read and write activities under NT.
 *
 * Revision 1.9  1999/07/15 02:11:12  steve
 *  Count pending IRPs.
 *
 * Revision 1.8  1999/03/21 01:36:08  steve
 *  Fix synchronization of IoMarpIrpPending.
 *
 * Revision 1.7  1998/09/29 00:39:36  steve
 *  Add os specific state dump.
 *
 * Revision 1.6  1998/09/23 23:12:57  steve
 *  Better detect diagnose broken interrupts.
 *
 * Revision 1.5  1998/07/11 18:56:51  steve
 *  Frames for multiple devices.
 *
 * Revision 1.4  1998/06/18 20:32:41  steve
 *  memcpy_fromio implementation
 *
 * Revision 1.3  1998/05/30 18:59:43  steve
 *  Allocate contiguous memory.
 *
 * Revision 1.2  1998/05/29 18:11:22  steve
 *  Proper cancel/abort behavior for NT.
 *
 * Revision 1.1  1998/05/28 22:53:02  steve
 *  NT port.
 *
 * Revision 1.13  1998/04/25 05:05:26  steve
 *  Preallocate frame space under NT.
 *
 * Revision 1.12  1998/04/22 19:06:57  steve
 *  Some write cancel support.
 *
 * Revision 1.11  1998/04/18 02:43:54  steve
 *  Fix trouble removing device lines at unload.
 *  Add support for read cancellation under NT.
 *  isecons device (nt only) now throws away old
 *  data instead of truncating new.
 *
 * Revision 1.10  1998/03/23 03:30:24  steve
 *  non-blocking reading under NT.
 *
 * Revision 1.9  1998/03/23 00:07:38  steve
 *  initial frame support for winnt.
 *
 * Revision 1.8  1998/03/21 21:06:43  steve
 *  Rearrange frame handling code.
 *
 * Revision 1.7  1997/12/07 02:16:42  steve
 *  Fix NT blocking problems, redo the console to
 *  not block and keep a limited buffer, trace the
 *  ucrx operations, and ucrx operations work on NT.
 *
 * Revision 1.6  1997/12/06 05:15:51  steve
 *  Add the control device to the NT port.
 *
 * Revision 1.5  1997/12/02 02:29:03  steve
 *  Add support for ISE board reset.
 *
 * Revision 1.4  1997/11/16 11:37:43  steve
 *  Make NT driver asynchronous.
 *
 * Revision 1.3  1997/11/11 11:19:46  steve
 *  Add support for frames.
 *
 * Revision 1.2  1997/09/12 05:18:06  steve
 *  Port to linux 2.1
 *
 * Revision 1.1  1997/09/11 21:58:36  steve
 *  Port the uCR device driver to Windows NT,
 *  make the configure select the target board
 *  and target host interface headers.
 *
 */


#endif
