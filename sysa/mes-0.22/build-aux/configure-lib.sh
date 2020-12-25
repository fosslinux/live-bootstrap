#! /bin/sh

# GNU Mes --- Maxwell Equations of Software
# Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
#
# This file is part of GNU Mes.
#
# GNU Mes is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# GNU Mes is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.

set -e
set -u

V=${V-1}

if [ "$V" = 2 ]; then
    set -x
fi

. ./config.sh

libc_mini_shared_SOURCES="
lib/mes/eputs.c
lib/mes/oputs.c
"

if test $mes_libc = mes; then
    libc_mini_shared_SOURCES="$libc_mini_shared_SOURCES
lib/$mes_kernel/$mes_cpu-mes-$compiler/mini.c
lib/stdlib/exit.c
lib/stdlib/puts.c
lib/string/strlen.c
"

    if test $mes_kernel = gnu; then
        libc_mini_shared_SOURCES="$libc_mini_shared_SOURCES
lib/gnu/_exit.c
lib/gnu/exec-startup-get-data.c
lib/gnu/fd-get.c
lib/gnu/fd-write.c
lib/gnu/io-write.c
lib/gnu/_write.c
lib/gnu/hurd-start.c
lib/gnu/proc-mark-exit.c
lib/gnu/syscall.c
lib/gnu/task-get-special-port.c
lib/gnu/task-terminate.c
lib/gnu/vm-statistics.c
lib/mach/mach-init.c
lib/mach/mach_host_self.S
lib/mach/mach_msg_trap.S
lib/mach/mach_reply_port.S
lib/mach/mach_task_self.S
lib/mach/mach_thread_self.S
lib/mach/msg.c
lib/string/argz-extract.c
"
    fi
fi

libc_mini_SOURCES="$libc_mini_shared_SOURCES"

if test $mes_libc = mes; then
    libc_mini_SOURCES="$libc_mini_SOURCES
lib/mes/mini-write.c
"
fi

libmes_SOURCES="
$libc_mini_shared_SOURCES
lib/ctype/isnumber.c
lib/mes/abtol.c
lib/mes/div.c
lib/mes/eputc.c
lib/mes/fdgetc.c
lib/mes/fdputc.c
lib/mes/fdputs.c
lib/mes/fdungetc.c
lib/mes/itoa.c
lib/mes/ltoa.c
lib/mes/ltoab.c
lib/mes/mes_open.c
lib/mes/ntoab.c
lib/mes/oputc.c
lib/mes/ultoa.c
lib/mes/utoa.c
"

if test $mes_libc = mes; then
    libmes_SOURCES="$libmes_SOURCES
lib/ctype/isdigit.c
lib/ctype/isspace.c
lib/ctype/isxdigit.c
lib/posix/write.c
lib/stdlib/atoi.c
"
    if test $mes_kernel = gnu; then
        libmes_SOURCES="$libmes_SOURCES
lib/stub/lseek.c
"
    fi
    if test $mes_kernel = linux; then
        libmes_SOURCES="$libmes_SOURCES
lib/linux/lseek.c
"
    fi
else
    libmes_SOURCES="$libmes_SOURCES
lib/mes/abtod.c
lib/mes/dtoab.c
"
fi

libc_SOURCES="
$libmes_SOURCES
lib/mes/__assert_fail.c
lib/mes/__buffered_read.c
lib/mes/__mes_debug.c
lib/posix/execv.c
lib/posix/getcwd.c
lib/posix/getenv.c
lib/posix/isatty.c
lib/posix/open.c
lib/posix/buffered-read.c
lib/posix/setenv.c
lib/posix/wait.c
lib/stdio/fgetc.c
lib/stdio/fputc.c
lib/stdio/fputs.c
lib/stdio/getc.c
lib/stdio/getchar.c
lib/stdio/putc.c
lib/stdio/putchar.c
lib/stdio/ungetc.c
lib/stdlib/free.c
lib/stdlib/malloc.c
lib/stdlib/realloc.c
lib/string/memchr.c
lib/string/memcmp.c
lib/string/memcpy.c
lib/string/memmove.c
lib/string/memset.c
lib/string/strcmp.c
lib/string/strcpy.c
lib/string/strncmp.c
"

if test $mes_kernel = gnu; then
    libc_SOURCES="$libc_SOURCES
lib/gnu/_open3.c
lib/gnu/_read.c
lib/gnu/dir-lookup.c
lib/gnu/fd-read.c
lib/gnu/io-read.c
lib/gnu/malloc.c
lib/gnu/vm-allocate.c
lib/stub/access.c
lib/stub/brk.c
lib/stub/chmod.c
lib/stub/clock_gettime.c
lib/stub/dup2.c
lib/stub/dup.c
lib/stub/execve.c
lib/stub/fork.c
lib/stub/_getcwd.c
lib/stub/gettimeofday.c
lib/stub/ioctl.c
lib/stub/time.c
lib/stub/unlink.c
lib/stub/waitpid.c
"
fi

if test $mes_kernel = linux; then
    libc_SOURCES="$libc_SOURCES
lib/linux/access.c
lib/linux/brk.c
lib/linux/chmod.c
lib/linux/clock_gettime.c
lib/linux/dup.c
lib/linux/dup2.c
lib/linux/execve.c
lib/linux/fork.c
lib/linux/fsync.c
lib/linux/_getcwd.c
lib/linux/gettimeofday.c
lib/linux/ioctl.c
lib/linux/_open3.c
lib/linux/_read.c
lib/linux/time.c
lib/linux/unlink.c
lib/linux/waitpid.c
lib/linux/$mes_cpu-mes-$compiler/syscall.c
"
fi

libtcc1_SOURCES="
lib/libtcc1.c
"

libc_tcc_SOURCES="
$libc_SOURCES
lib/ctype/islower.c
lib/ctype/isupper.c
lib/ctype/tolower.c
lib/ctype/toupper.c
lib/mes/abtod.c
lib/mes/dtoab.c
lib/mes/search-path.c
lib/posix/execvp.c
lib/stdio/fclose.c
lib/stdio/fdopen.c
lib/stdio/ferror.c
lib/stdio/fflush.c
lib/stdio/fopen.c
lib/stdio/fprintf.c
lib/stdio/fread.c
lib/stdio/fseek.c
lib/stdio/ftell.c
lib/stdio/fwrite.c
lib/stdio/printf.c
lib/stdio/remove.c
lib/stdio/snprintf.c
lib/stdio/sprintf.c
lib/stdio/sscanf.c
lib/stdio/vfprintf.c
lib/stdio/vprintf.c
lib/stdio/vsnprintf.c
lib/stdio/vsprintf.c
lib/stdio/vsscanf.c
lib/stdlib/calloc.c
lib/stdlib/qsort.c
lib/stdlib/strtod.c
lib/stdlib/strtof.c
lib/stdlib/strtol.c
lib/stdlib/strtold.c
lib/stdlib/strtoll.c
lib/stdlib/strtoul.c
lib/stdlib/strtoull.c
lib/string/memmem.c
lib/string/strcat.c
lib/string/strchr.c
lib/string/strlwr.c
lib/string/strncpy.c
lib/string/strrchr.c
lib/string/strstr.c
lib/string/strupr.c
lib/stub/sigaction.c
lib/stub/ldexp.c
lib/stub/mprotect.c
lib/stub/localtime.c
lib/stub/sigemptyset.c
lib/$mes_cpu-mes-$compiler/setjmp.c
"

if test $mes_kernel = linux; then
    libc_tcc_SOURCES="$libc_tcc_SOURCES
lib/linux/close.c
lib/linux/rmdir.c
lib/linux/stat.c
"
fi

if test $mes_kernel = gnu; then
    libc_tcc_SOURCES="$libc_tcc_SOURCES
lib/stub/close.c
lib/stub/rmdir.c
lib/stub/stat.c
"
fi

libc_gnu_SOURCES="
$libc_tcc_SOURCES
lib/ctype/isalnum.c
lib/ctype/isalpha.c
lib/ctype/isascii.c
lib/ctype/iscntrl.c
lib/ctype/isgraph.c
lib/ctype/isprint.c
lib/ctype/ispunct.c
lib/dirent/__getdirentries.c
lib/dirent/closedir.c
lib/dirent/opendir.c
lib/dirent/readdir.c
lib/math/ceil.c
lib/math/fabs.c
lib/math/floor.c
lib/mes/fdgets.c
lib/posix/alarm.c
lib/posix/execl.c
lib/posix/execlp.c
lib/posix/mktemp.c
lib/posix/raise.c
lib/posix/sbrk.c
lib/posix/sleep.c
lib/posix/unsetenv.c
lib/stdio/clearerr.c
lib/stdio/feof.c
lib/stdio/fgets.c
lib/stdio/fileno.c
lib/stdio/freopen.c
lib/stdio/fscanf.c
lib/stdio/perror.c
lib/stdio/vfscanf.c
lib/stdlib/__exit.c
lib/stdlib/abort.c
lib/stdlib/abs.c
lib/stdlib/alloca.c
lib/stdlib/atexit.c
lib/stdlib/atof.c
lib/stdlib/atol.c
lib/stdlib/mbstowcs.c
lib/string/bcmp.c
lib/string/bcopy.c
lib/string/bzero.c
lib/string/index.c
lib/string/rindex.c
lib/string/strcspn.c
lib/string/strdup.c
lib/string/strerror.c
lib/string/strncat.c
lib/string/strpbrk.c
lib/string/strspn.c
lib/stub/__cleanup.c
lib/stub/atan2.c
lib/stub/bsearch.c
lib/stub/chown.c
lib/stub/cos.c
lib/stub/ctime.c
lib/stub/exp.c
lib/stub/fpurge.c
lib/stub/freadahead.c
lib/stub/frexp.c
lib/stub/getgrgid.c
lib/stub/getgrnam.c
lib/stub/getlogin.c
lib/stub/getpgid.c
lib/stub/getpgrp.c
lib/stub/getpwnam.c
lib/stub/getpwuid.c
lib/stub/gmtime.c
lib/stub/log.c
lib/stub/mktime.c
lib/stub/modf.c
lib/stub/pclose.c
lib/stub/popen.c
lib/stub/pow.c
lib/stub/rand.c
lib/stub/rewind.c
lib/stub/setbuf.c
lib/stub/setgrent.c
lib/stub/setlocale.c
lib/stub/setvbuf.c
lib/stub/sigaddset.c
lib/stub/sigblock.c
lib/stub/sigdelset.c
lib/stub/sigsetmask.c
lib/stub/sin.c
lib/stub/sqrt.c
lib/stub/strftime.c
lib/stub/sys_siglist.c
lib/stub/system.c
lib/stub/times.c
lib/stub/ttyname.c
lib/stub/umask.c
lib/stub/utime.c
"

if test $mes_kernel = linux; then
    libc_gnu_SOURCES="$libc_gnu_SOURCES
lib/linux/chdir.c
lib/linux/fcntl.c
lib/linux/fstat.c
lib/linux/getdents.c
lib/linux/getegid.c
lib/linux/geteuid.c
lib/linux/getgid.c
lib/linux/getpid.c
lib/linux/getppid.c
lib/linux/getrusage.c
lib/linux/getuid.c
lib/linux/kill.c
lib/linux/link.c
lib/linux/lstat.c
lib/linux/mkdir.c
lib/linux/mknod.c
lib/linux/nanosleep.c
lib/linux/pipe.c
lib/linux/readlink.c
lib/linux/rename.c
lib/linux/setgid.c
lib/linux/settimer.c
lib/linux/setuid.c
lib/linux/signal.c
lib/linux/sigprogmask.c
lib/linux/symlink.c
"
fi

mes_SOURCES="
src/gc.c
src/hash.c
src/lib.c
src/math.c
src/mes.c
src/module.c
src/posix.c
src/reader.c
src/string.c
src/struct.c
src/vector.c
"
