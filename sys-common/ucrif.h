#ifndef __ucrif_H
#define __ucrif_H
/*
 * Copyright (c) 1996-2000 Picture Elements, Inc.
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
 * This is the interface for the ucr channel driver.
 *
 * Each device supports up to 256 channels, from 0 to 0xff. Each
 * channel may be opened by no more then one process at a time. Once a
 * process opens a channel, reads and writes cause data to move
 * between the host and the target board, through the selected
 * channel. A process can change the channel association by using the
 * UCR_CHANNEL ioctl.
 *
 * Normally, the driver will buffer writes to the board up to a page
 * size, and may have a few buffers filled before a write will
 * block. Also, the application may read bytes from the channel
 * without affecting the write buffer. However, if a read blocks on a
 * channel, the write buffers will be flushed. This fits well with the
 * request/response nature of typical communications with the
 * board. In general, all will work transparently.
 *
 * The application may explicitly flush the write buffer, thus
 * guaranteeing that all written bytes are delivered to the target
 * board, by issuing the UCR_FLUSH ioctl.
 *
 * A UCR_FLUSH does *not* guarantee that all the data has been read by
 * the target -- only that the target has been told about it. UCR_SYNC
 * will block until the target board reads all the data in the channel.
 *
 * Reads from a channel will not block unless there are no bytes
 * ready. In other words, if you read N bytes, the read may actually
 * read 1 to N bytes. The read return code tells you what you have
 * got. If non-blocking mode is enabled (by fcntl under linux or the
 * UCR_NONBLOCK under NT) read will return 0 bytes if there are no
 * bytes ready to be read.
 *
 * Finally, closing a channel causes the channel and host buffer
 * resources to be removed. The close does NOT flush the buffers. It
 * is recommended that a UCR_SYNC preceed a close to prevend a loss of
 * output data.
 *
 * NOTE: Channel 0 is special. When a process first opens a ucr
 * device, it attempts to connect to channel 0. If another process has
 * connected to channel 0, the open will fail and return EBUSY. The
 * open will succeed only if channel 0 is opened by no one else.
 *
 * NOTE: When no process has the device open, the driver completely
 * detaches from the board. This allows the board to be reset without
 * confusing the driver.
 *
 * NOTE ON SELECT: The driver does in fact support the select system
 * call under Linux, but for input data only. NT does not have a
 * general purpose select system call, use overlapped IO instead.
 *
 * NOTE ON Windows NT: The IOCTL codes are compatible with Windows NT
 * encoding using BUFFERED I/O of the parameters. The FILE_DEVICE type
 * bits and the rw bits are also consistent with the use by the driver
 * under NT. Linux ignores the bit patterns, so there is no
 * portability issue.
 */

# define UCR_READFLAG  0x4000
# define UCR_WRITEFLAG 0x8000
# define UCR_(rw,x)  (0x80010000+(rw)+((x)<<2))
# define UCRX_(rw,x) (0x80030000+(rw)+((x)<<2))

/*
 * The driver is free to buffer bytes being sent to a channel. This
 * allows the driver to send larger bursts. The driver will certainly
 * flush on close and channel changes, but sometimes it is necessary
 * to send buffered data immediately.
 *
 * SYNC is more thorough then FLUSH. After a sync, not only is the
 * buffer flushed to the target, but the target has actually READ the
 * data.
 */
# define UCR_FLUSH UCR_(0,0)
# define UCR_SYNC UCR_(0,12)

/*
 * Use this ioctl to set the channel used for reads and writes. The
 * value is 0 when the device is first opened, and can be any number
 * from 0 to 255 (0xff).
 */
# define UCR_CHANNEL UCR_(0,1)

/*
 * Use this to send an attention signal to the uCR target. The single
 * argument is sent to the target as an argument to the attention
 * handler for the channel. (The channel used is set by the
 * UCR_CHANNEL ioctl above.)
 *
 * The target application channel sees attentions delivered this way
 * with the arguments arg1==1 and arg2 from the argument of the ioctl.
 */
# define UCR_ATTENTION UCR_(UCR_WRITEFLAG,2)

/*
 * Use this to send a FILE_MARK to the channel on the uCR target. The
 * target will read this as an end-of-file so generally this is
 * considered the end of the data stream.
 *
 * However, the target is capable of turning OFF the end-of-file, so
 * it can also be used to separate a stream of files.
 */
# define UCR_SEND_FILE_MARK UCR_(UCR_WRITEFLAG,3)

#if !defined(WINNT) && !defined(_WIN32)
/*
 * The driver can manage up to 16 frames, each no larger then
 * 256Meg. Use UCR_MAKE_FRAME to create a frame with the given size.
 * The argument is of the form 0xfsssssss where f is the frame
 * identifier and s is the size, in bytes.
 *
 * If the kernel cannot allocate the frame, the ioctl return
 * ENOMEM. If the frame already exists, it returns the size of the
 * existing frame. If the existing frame is too small, it can only be
 * made bigger by freeing it and remaking it.
 *
 * UCR_FREE_FRAME takes the same format and releases the frame from
 * the kernel memory. The size bits are ignored.
 *
 * Once the frame exists on the host, the host and board can access
 * the frame independent of any channels. The host can map the frame
 * and close its channels. The target can read/write the frame as long
 * as it exists.
 *
 *  - Persistence Of Frames -
 * On the host, the frames last until the module is unloaded (possibly
 * automaticly) from the kernel, or the frame is explicitly freed. As
 * long as the frame is not freed, any data in it is preserved.
 *
 * A frame will also be implicitly released when the board is reset
 * with a UCRX_RESET ioctl. Since the UCRX_RESET only works if there
 * are no open references to the board, and the board is being reset
 * so also has no access to the frame, it is safe to release the frame
 * at this point.
 *
 * On Linux, modules are automatically unloaded if there are no
 * processes with opened channels or mapped frames. The operating
 * system automatically notices that the driver has been idle for a
 * while and arranges for it to be unloaded.
 *
 * NT does not support such a facility, so frames will persist either
 * until the driver module is unloaded manually, or the frame is
 * released explicitly.
 *
 *  - NOTE wrt UCR_SEND_FRAME -
 * UCR_SEND_FRAME is obsolete. It is no longer implemented in the
 * driver, and should not be used.
 */
#if defined(linux)
# define UCR_MAKE_FRAME UCR_(0,4)
# define UCR_FREE_FRAME UCR_(0,5)
/* # define UCR_SEND_FRAME UCR_(0,6) */
#endif
#endif


#if defined(WINNT) || defined(_WIN32)
/*
 * This is Windows NT support for mapping a frame. Windows does not
 * have a mmap call, so the driver supports a device specific ioctl
 * that does the job.
 *
 * Both the MMAP and MUNMAP take as input a UcrMmapInfo structure. For
 * mapping, the off and size are used to select the location within
 * the area. The offset must be on a page boundry. The MMAP also
 * returns a structure with all the fields, including the base, filled
 * in. If the size is passed as 0, the entire frame is mapped and the
 * size will be filled in by the driver.
 *
 * For MUNMAP, the driver unmaps the region at the base, and returns
 * nothing as a result.
 */
struct IsexMmapInfo {
      unsigned frame_id;
};

/*
 * These are DEVICE#0x8003, FILE_ACCESS_ANY. the MMAP is code#20(0x14)
 * and METHOD_OUT_DIRECT. The OUT_DIRECT part is so that the output buffer
 * is passed in as an MDL. That is the region that is mapped. The
 * MAKE_MAP_FRAME will *not* complete until another thread unmaps the
 * frame, or the process dies.
 *
 * The UNMAK_UNMAKE_FRAME is METHOD_BUFFERED as we don't need to
 * include the mapping in the unmap method. This just unmaps the
 * specified frame.
 */
# define ISEX_MAKE_MAP_FRAME     0x80030052
# define ISEX_UNMAP_UNMAKE_FRAME 0x80030054
# define ISEX_WAIT_MAP_FRAME     0x80030058

#endif

#if defined(WINNT) || defined(_WIN32)
/*
 * This is windows NT support for setting the channel reader to
 * non-blocking mode. What happens normally, is a read with no bytes
 * available will block until at least one byte can be
 * returned. ReadFile won't return 0 bytes. However, this IOCTL can be
 * used to turn on non-blocking mode. In that case, ReadFile will
 * return 0 if there is nothing to read.
 *
 * This ioctl takes a single long parameter, set to 1 to turn on
 * non-blocking mode.
 */
# define UCR_NONBLOCK UCR_(0, 10)
#endif

/*
 * This IOCTL takes to parameters and returns no result. It is for use
 * during testing, the prod the device driver.
 */
# define UCR_TEST UCR_(0, 11)

/*
 * The uCR includes a command/control device separate from the normal
 * device. These commands are often board/os specific.
 */

/*
 * UCRX_RESET_BOARD
 * This causes any boot operations on the target board to happen. It
 * is pretty drastic, as the i960 core is driven reset along with most
 * of the function unit it is attached to.
 *
 * No channels may be open when this is called.
 */
# define UCRX_RESET_BOARD UCRX_(0,0)

/*
 * UCRX_RESTART_BOARD
 * This is a bit less drastic then the board reset, and should be
 * preferred in general. It does a soft reset of the board, which
 * means an interrupt is sent to the processor, telling it to re-
 * initialize.
 *
 * As with the RESET_BOARD control, no channels may be open when this
 * is invoked.
 */
# define UCRX_RESTART_BOARD UCRX_(0,10)

# define UCRX_RUN_PROGRAM UCRX_(0,1)

# define UCRX_DIAGNOSE UCRX_(0,2)

/*
 * This sets the driver trace bits.
 */
# define UCRX_SET_TRACE UCRX_(0,3)
# define UCRX_GET_TRACE UCRX_(0,4)
# define UCR_TRACE_CHAN  0x0001
# define UCR_TRACE_FRAME 0x0002
# define UCR_TRACE_PROTO 0x0004
# define UCR_TRACE_UCRX  0x0008

/*
 * UCRX_TIMEOUT
 * Set read timeout parameters for the specified open channel. The
 * channel is created with no timeout at all. This device control
 * sets a timeout, or removes one previously set.
 *
 * The timeout value is given in milli-seconds. If 0, then reads do
 * not block at all (polling read). If -1, then the timeout is removed
 * and later reads will block until data is available. If -2, the
 * timeout is forced on any currently blocked reads, but an existing
 * timeout setting is preserved.
 */

# define UCRX_TIMEOUT_OFF -1
# define UCRX_TIMEOUT_FORCE -2
struct ucrx_timeout_s {
      unsigned short id;
      long read_timeout;
};
# define UCRX_TIMEOUT UCRX_(0,12)


/*
 * The controls only exist in the Linux device driver. They allow the
 * board to be removed (powered off by a bus extender) and
 * replaced.
 *
 * The UCRX_REMOVE saves the board configuration, locks the board in
 * the driver, and disables the board in its configuration space. When
 * this is done, the board can be powered off and removed, without
 * disturbing the operating system.
 *
 * The UCRX_REPLACE command causes the saved board configuration to be
 * used to reconfigure a new board in the same position. The board
 * must be of a compatible type for this to work properly.
 */
# define UCRX_REMOVE  UCRX_(0,13)
# define UCRX_REPLACE UCRX_(0,14)

/*
 * This DeviceIoControl returns an integer code that represents the
 * major board type. The result is one of the following integer
 * values:
 *
 *   UCRX_BOARD_TYPE_ISE
 *      ISE/SSE board
 *
 *   UCRX_BOARD_TYPE_JSE
 *      JSE board
 *
 * Note: This is not intended for general use. Instead, applications
 * should use the ise_version results to identify the board type with
 * more precision. The return type is an int.
 */
# define UCRX_BOARD_TYPE UCRX_(0,15)
# define UCRX_BOARD_TYPE_INVAL (-1)
# define UCRX_BOARD_TYPE_ISE   (0)
# define UCRX_BOARD_TYPE_JSE   (1)

/*
 * $Log$
 * Revision 1.5  2008/09/02 17:47:50  steve-icarus
 *  Select Linux specific ioctl code definitions automatically.
 *
 * Revision 1.4  2005/08/12 22:32:45  steve
 *  New 2.5 driver API.
 *
 * Revision 1.3  2004/08/02 23:45:49  steve
 *  Add UCRX_BOARD_TYPE control
 *
 * Revision 1.2  2002/03/26 03:13:08  steve
 *  Add REMOVE/REPLACE ioctl definitions.
 *
 * Revision 1.1  2001/07/11 23:47:38  steve
 *  Add ucrx_timeout device controls.
 *
 * Revision 1.2  2001/07/06 19:24:55  steve
 *  Add the new RESTART_BOARD control
 *
 * Revision 1.1  2001/03/03 01:30:49  steve
 *  Make an isolated linux driver source tree.
 */


#endif
