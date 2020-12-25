/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#ifndef __MES_UNISTD_H
#define __MES_UNISTD_H 1

#if SYSTEM_LIBC
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#undef __MES_UNISTD_H
#include_next <unistd.h>

#else // ! SYSTEM_LIBC

#if defined (BOOTSTRAP_WITH_POSIX)
#define _POSIX_VERSION 199009L
#endif

#include <sys/types.h>
#ifndef NULL
#define NULL 0
#endif

#ifndef STDIN_FILENO
#define	STDIN_FILENO  0
#define	STDOUT_FILENO 1
#define	STDERR_FILENO 2
#endif // STDIN_FILENO

#ifndef STDIN_FILE_NO
#define	STDIN_FILE_NO  0
#define	STDOUT_FILE_NO 1
#define	STDERR_FILE_NO 2
#endif // STDIN_FILE_NO

#ifndef R_OK
#define F_OK 0
#define X_OK 1
#define W_OK 2
#define R_OK 4
#endif

int access (char const *s, int mode);
unsigned int alarm (unsigned int seconds);
int close (int fd);
int execv (char const *file_name, char *const argv[]);
int execl (char const *file_name, char const *arg, ...);
int execlp (char const *file_name, char const *arg, ...);
int execve (char const *file, char *const argv[], char *const env[]);
int execvp (char const *file, char *const argv[]);
int fork (void);
int fsync (int filedes);
char *getcwd (char *buf, size_t size);
uid_t getuid (void);
gid_t getgid (void);
int setgid (gid_t newgid);
int setuid (uid_t newuid);
uid_t geteuid (void);
gid_t getegid (void);
pid_t getpgrp (void);
pid_t getpid (void);
pid_t getppid (void);
int getpgid (pid_t pid);
int isatty (int fd);
int link (char const *old_name, char const *new_name);
off_t lseek (int fd, off_t offset, int whence);
ssize_t read (int fd, void *buffer, size_t size);
ssize_t readlink (char const *file_name, char *buffer, size_t size);
#if __SBRK_CHAR_PTRDIFF
/* xmalloc in binutils <= 2.10.1 uses this old prototype */
char *sbrk (ptrdiff_t delta);
#else
void *sbrk (intptr_t delta);
#endif
int symlink (char const *old_name, char const *new_name);
int unlink (char const *file_name);
ssize_t write (int filedes, void const *buffer, size_t size);

#endif // ! SYSTEM_LIBC

#endif // __MES_UNISTD_H
