/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#ifndef __MES_LINUX_X86_SYSCALL_H
#define __MES_LINUX_X86_SYSCALL_H 1

// libc-mini
// #define SYS_exit    0x01
// #define SYS_write   0x04

// libc
#define SYS_fork    0x02
#define SYS_read    0x03
#define SYS_open    0x05
#define SYS_waitpid 0x07
#define SYS_wait4   0x72
#define SYS_execve  0x0b
#define SYS_chmod   0x0f
#define SYS_access  0x21
#define SYS_brk     0x2d
#define SYS_ioctl   0x36
#define SYS_fsync   0x76

// libc+tcc
#define SYS_close  0x06
#define SYS_time   0x0d
#define SYS_lseek  0x13
#define SYS_unlink 0x0a
#define SYS_rmdir  0x28
#define SYS_gettimeofday 0x4e
#define SYS_stat   0x6a
#define SYS_getcwd 0xb7

// libc+gnu

#define SYS_chdir     0x0c
#define SYS_link      0x09
#define SYS_getpid    0x14
#define SYS_getuid    0x18
#define SYS_kill      0x25
#define SYS_rename    0x26
#define SYS_mkdir     0x27
#define SYS_dup       0x29
#define SYS_pipe      0x2a
#define SYS_getgid    0x2f
#define SYS_signal    0x30
#define SYS_sigaction 0x43
#define SYS_rt_sigaction 0xae
#define SYS_signal    0x30
#define SYS_fcntl     0x37
#define SYS_dup2      0x3f
#define SYS_getrusage 0x4d
#define SYS_lstat     0x6b
#define SYS_setitimer 0x68
#define SYS_fstat     0x6c
#define SYS_nanosleep 0xa2
#define SYS_getdents  0x8d
#define SYS_clock_gettime 0x109

// bash
#define SYS_setuid    0x17
#define SYS_geteuid   0x31
#define SYS_getegid   0x32
#define SYS_setgid    0x3e
#define SYS_getppid   0x40

// make+POSIX
#define SYS_sigprocmask 0x7e

// tar
#define SYS_symlink   0x53
#define SYS_readlink  0x55
#define SYS_mknod     0x0e

#endif // __MES_LINUX_X86_SYSCALL_H
