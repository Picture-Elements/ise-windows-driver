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

# include  "os.h"
# include  "t-config.h"
# include  "ucrpriv.h"

struct wait_queue {
      KEVENT event;
      KDPC   dpc;
      struct wait_queue*next;
};

static void wake_up_dpc_fun(PKDPC kdpc, void*context, void*arg1, void*arg2);

/*
 * This NT implementation of the atomic_sleep_on uses a DPC to
 * communicate with the interrupt handler. With the target interrupts
 * still blocked, set up the wait queue with the current thread and
 * create the DPC object and sync event. Then release the board
 * interrupts and wait for the interrupt to wake me up.
 *
 * The wake_up happens by the wake_up function, which may use the
 * wake_up_dpc_fun, to signal the even that I'm waiting on.
 */


int atomic_sleep_on(struct Instance*xsp,
		     unsigned long mask,
		     struct wait_queue**sync)
{
      struct wait_queue wait_cell;
      xsp->cnt.sleep += 1;
      xsp->cnt.sleep_pend += 1;

      KeInitializeDpc(&wait_cell.dpc, wake_up_dpc_fun, &wait_cell);
      KeInitializeEvent(&wait_cell.event, NotificationEvent, FALSE);
      wait_cell.next = *sync;
      *sync = &wait_cell;

      dev_unmask_irqs(xsp->dev, mask);

      KeWaitForSingleObject(&wait_cell.event, Executive, KernelMode,
			    FALSE, 0);

      dev_mask_irqs(xsp->dev);

      xsp->cnt.sleep_pend -= 1;
      return 0;
}

static void wake_up_dpc_fun(PKDPC kdpc, void*context, void*arg1, void*arg2)
{
      struct wait_queue*cur = (struct wait_queue*)context;
      KeSetEvent(&cur->event, 0, FALSE);
}

void wake_up(struct wait_queue**waitp)
{
      if (KeGetCurrentIrql() > DISPATCH_LEVEL)
	    while (*waitp) {
		  struct wait_queue*cur = *waitp;
		  *waitp = cur->next;
		  KeInsertQueueDpc(&cur->dpc, 0, 0);
	    }
      else
	    while (*waitp) {
		  struct wait_queue*cur = *waitp;
		  *waitp = cur->next;
		  wake_up_dpc_fun(0, cur, 0, 0);
	    }
}

static void time_dpc_fun(PKDPC kdpc, void*context, void*arg1, void*arg2)
{
      struct timer_list*tim = (struct timer_list*)context;
      (tim->fun)(tim->arg);
}

void set_time_delay(struct Instance*xsp,
		    struct timer_list*tim,
		    void (*fun)(unsigned long))
{
      LARGE_INTEGER delay;
      delay.LowPart = 4 * 10000000;
      delay.HighPart = 0;

      tim->fun = fun;
      tim->arg = (unsigned long)xsp;
      KeInitializeTimer(&tim->timer);
      KeInitializeDpc(&tim->dpc, time_dpc_fun, tim);

      KeSetTimer(&tim->timer, RtlLargeIntegerNegate(delay), &tim->dpc);
}

void cancel_time_delay(struct timer_list*tim)
{
      KeCancelTimer(&tim->timer);
}

/*
 * $Log$
 * Revision 1.1  2001/03/05 20:11:40  steve
 *  Add NT4 driver to ISE source tree.
 *
 * Revision 1.2  1999/08/18 00:06:00  steve
 *  Fix SMP synchronization problem.
 *
 * Revision 1.1  1998/05/28 22:53:03  steve
 *  NT port.
 *
 * Revision 1.3  1997/12/06 05:15:22  steve
 *  Remove a warning
 *
 * Revision 1.2  1997/11/16 11:37:44  steve
 *  Make NT driver asynchronous.
 *
 * Revision 1.1  1997/09/11 21:58:37  steve
 *  Port the uCR device driver to Windows NT,
 *  make the configure select the target board
 *  and target host interface headers.
 *
 */

