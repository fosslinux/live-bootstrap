/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 *
 * This file is part of GNU Mes.
 *
 * GNU Mes is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * GNU Mes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <linux/syscall.h>
#include <syscall.h>
#include <unistd.h>
#include <signal.h>

#if __i386__
#else
void
_restorer_for_siginfo (void)
{
  _sys_call (SYS_rt_sigreturn);
}
#endif

sighandler_t
signal (int signum, sighandler_t action)
{
#if __i386__
  return _sys_call2 (SYS_signal, signum, action);
#else
  static struct sigaction setup_action = { 0 };
  static struct sigaction old = { 0 };
  unsigned short bitindex;
  unsigned short itembitcount;

  setup_action.sa_handler = action;
  /* The sa_restorer is not used.  Reason:
     * The kernel uses its own restorer anyway by pretending we gave one on
     our side (no, really).
     glibc still has a custom restorer because they want to be able to
     backtrace.  gdb has a second part that detects a specific instruction
     sequence and then fixes up the backtrace.  Since we don't use that
     specific instruction sequence and also the non-siginfo restorer
     is complicated (pops foreign item off the stack--so needs inline
     assembly) and we don't support great debuggability in other places,
     I've decided to take the easy way out for now. */
  /*setup_action.sa_restorer = _restorer_for_siginfo;*/
  bitindex = signum - 1;
  itembitcount = 8 * sizeof(setup_action.sa_mask.items[0]);
  setup_action.sa_mask.items[bitindex / itembitcount] = 1UL << (bitindex % itembitcount);
  old.sa_handler = SIG_DFL;
  setup_action.sa_flags = SA_RESTART;
#if __x86_64__
  /* Tell kernel that we use a restorer, on penalty of segfault.  */
  setup_action.sa_flags |= SA_RESTORER;
#endif
  int r = _sys_call4 (SYS_rt_sigaction, signum, (long)&setup_action, (long)&old, sizeof (sigset_t));
  if (r)
    return 0;
  return old.sa_handler;
#endif
}
