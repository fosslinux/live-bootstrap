/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#ifndef __MES_LINUX_X86_64_SYSCALL_H
#define __MES_LINUX_X86_64_SYSCALL_H 1

// libc-mini
// #define SYS_write   0x01
// #define SYS_exit    0x3c

// libc
#define SYS_fork    0x39
#define SYS_read    0x00
#define SYS_open    0x02
//#define SYS_waitpid
#define SYS_wait4   0x3d
#define SYS_execve  0x3b
#define SYS_chmod   0x5a
#define SYS_access  0x15
#define SYS_brk     0x0c
#define SYS_ioctl   0x10
#define SYS_fsync   0x4a

// libc+tcc
#define SYS_close  0x03
#define SYS_time   0xc9
#define SYS_lseek  0x08
#define SYS_unlink 0x57
#define SYS_rmdir  0x54
#define SYS_gettimeofday 0x60
#define SYS_stat   0x04
#define SYS_getcwd 0x4f

// libc+gnu
#define SYS_chdir     0x50
#define SYS_link      0x56
#define SYS_getpid    0x27
#define SYS_getuid    0x66
#define SYS_kill      0x3e
#define SYS_rename    0x52
#define SYS_mkdir     0x53
#define SYS_dup       0x20
#define SYS_pipe      0x16
#define SYS_getgid    0x68
#define SYS_rt_sigaction 0x0d
#define SYS_rt_sigreturn 0x0f
#define SYS_fcntl     0x48
#define SYS_dup2      0x21
#define SYS_getrusage 0x62
#define SYS_lstat     0x06
#define SYS_setitimer 0x26
#define SYS_fstat     0x05
#define SYS_nanosleep 0x33
#define SYS_getdents  0x4e
#define SYS_clock_gettime 0xe4

// bash
#define SYS_setuid    0x69
#define SYS_setgid    0x6a
#define SYS_geteuid   0x6b
#define SYS_getegid   0x6c
#define SYS_getppid   0x6e

// make+SYSTEM_LIBC
#define SYS_rt_sigprocmask 0x0e

// tar
#define SYS_symlink   0x58
#define SYS_readlink  0x59
#define SYS_mknod     0x85

#endif // __MES_LINUX_X86_64_SYSCALL_H
