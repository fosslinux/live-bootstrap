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

set -ex

################################################################################
# SYSTEM_LIBC build

## Clean ##
rm -rf out-system-libc
mkdir out-system-libc

## Configure ##
mes_cpu=x86_64
mes_bits=64
cat > include/mes/config.h <<EOF
#define SYSTEM_LIBC 1
#define MES_VERSION "git"
EOF

## Build ##
gcc -g -D HAVE_CONFIG_H=1 -I include\
    -o out-system-libc/mes\
    \
    lib/mes/eputs.c\
    lib/mes/oputs.c\
    \
    lib/mes/div.c\
    lib/mes/itoa.c\
    lib/mes/ltoa.c\
    lib/mes/ltoab.c\
    lib/mes/ultoa.c\
    lib/mes/utoa.c\
    lib/mes/eputc.c\
    lib/mes/fdgetc.c\
    lib/mes/fdputc.c\
    lib/mes/fdputs.c\
    lib/mes/fdungetc.c\
    lib/mes/mes_open.c\
    lib/mes/ntoab.c\
    lib/mes/oputc.c\
    \
    src/gc.c\
    src/hash.c\
    src/lib.c\
    src/math.c\
    src/mes.c\
    src/module.c\
    src/posix.c\
    src/reader.c\
    src/string.c\
    src/struct.c\
    src/vector.c

## Check ##

# Any light?
out-system-libc/mes --help

# Simplest of tests
echo '(display "hello\n")' | MES_BOOT=boot-01.scm out-system-libc/mes

# Basic test.  We should be safe, but there are ~30 more in tests/*.test.
MES_DEBUG=4 MES=out-system-libc/mes tests/base.test

# GC test
MES_DEBUG=3 MES_ARENA=10000 MES_MAX_ARENA=10000 MES_BOOT=scaffold/gc-test.scm out-system-libc/mes

# MesCC test
MES_DEBUG=2 MES=out-system-libc/mes sh -x scripts/mescc -m $mes_bits -nostdlib\
         -I include -I include/$mes_kernel/$mes_cpu\
         -o out-system-libc/hello\
         lib/linux/$mes_cpu-mes-mescc/crt1.c\
         \
         lib/mes/eputs.c\
         \
         lib/linux/$mes_cpu-mes-mescc/mini.c\
         \
         lib/mes/write.c\
         lib/string/strlen.c\
         \
         scaffold/hello.c
set +e
out-system-libc/hello
r=$?
if [ $r != 42 ]; then
   exit 1
fi

################################################################################
# Mes C lib build

# To get a i686-unknown-linux-gnu-gcc, you may do:
#    guix environment -l guix.scm
# or
#    guix environment --ad-hoc -e '(begin (use-modules (gnu packages cross-base)) (list (cross-binutils "i686-unknown-linux-gnu") (cross-gcc "i686-unknown-linux-gnu")))'

## Clean ##
rm -rf out-mes
mkdir out-mes

## Configure ##
mes_kernel=linux
CC=gcc
mes_cpu=x86_64
#CC=i686-unknown-linux-gnu-gcc
#mes_cpu=x86
cat > include/mes/config.h <<EOF
// #define SYSTEM_LIBC 0
#define MES_VERSION "git"
EOF

## Build ##
compiler=gcc     # not configurable
$CC -g -D HAVE_CONFIG_H=1 -I include -I include/$mes_kernel/$mes_cpu\
    -nostdinc -nostdlib -fno-builtin -fno-stack-protector\
    -o out-mes/mes\
    \
    lib/linux/$mes_cpu-mes-gcc/crt1.c\
    \
    lib/mes/eputs.c\
    lib/mes/oputs.c\
    \
    lib/posix/write.c\
    lib/string/strlen.c\
    lib/stdlib/puts.c\
    lib/stdlib/exit.c\
    lib/$mes_kernel/$mes_cpu-mes-$compiler/mini.c\
    \
    lib/mes/div.c\
    lib/mes/itoa.c\
    lib/mes/ltoa.c\
    lib/mes/ltoab.c\
    lib/mes/ultoa.c\
    lib/mes/utoa.c\
    lib/mes/eputc.c\
    lib/mes/fdgetc.c\
    lib/mes/fdputc.c\
    lib/mes/fdputs.c\
    lib/mes/fdungetc.c\
    lib/mes/mes_open.c\
    lib/mes/ntoab.c\
    lib/mes/oputc.c\
    \
    lib/stdlib/atoi.c\
    lib/mes/abtol.c\
    lib/ctype/isdigit.c\
    lib/ctype/isnumber.c\
    lib/ctype/isspace.c\
    lib/ctype/isxdigit.c\
    \
    lib/mes/__assert_fail.c\
    lib/mes/__buffered_read.c\
    lib/mes/__mes_debug.c\
    lib/posix/execv.c\
    lib/posix/getcwd.c\
    lib/posix/getenv.c\
    lib/posix/isatty.c\
    lib/posix/open.c\
    lib/posix/read.c\
    lib/posix/setenv.c\
    lib/posix/wait.c\
    lib/stdio/fgetc.c\
    lib/stdio/fputc.c\
    lib/stdio/fputs.c\
    lib/stdio/getc.c\
    lib/stdio/getchar.c\
    lib/stdio/putc.c\
    lib/stdio/putchar.c\
    lib/stdio/ungetc.c\
    lib/stdlib/free.c\
    lib/stdlib/malloc.c\
    lib/stdlib/realloc.c\
    lib/string/memchr.c\
    lib/string/memcmp.c\
    lib/string/memcpy.c\
    lib/string/memmove.c\
    lib/string/memset.c\
    lib/string/strcmp.c\
    lib/string/strcpy.c\
    lib/string/strncmp.c\
    \
    lib/linux/lseek.c\
    \
    lib/linux/access.c\
    lib/linux/brk.c\
    lib/linux/chmod.c\
    lib/linux/clock_gettime.c\
    lib/linux/dup.c\
    lib/linux/dup2.c\
    lib/linux/execve.c\
    lib/linux/fork.c\
    lib/linux/fsync.c\
    lib/linux/_getcwd.c\
    lib/linux/gettimeofday.c\
    lib/linux/ioctl.c\
    lib/linux/_open3.c\
    lib/linux/_read.c\
    lib/linux/time.c\
    lib/linux/unlink.c\
    lib/linux/waitpid.c\
    lib/linux/$mes_cpu-mes-$compiler/syscall.c\
    \
    src/gc.c\
    src/hash.c\
    src/lib.c\
    src/math.c\
    src/mes.c\
    src/module.c\
    src/posix.c\
    src/reader.c\
    src/string.c\
    src/struct.c\
    src/vector.c

## Check ##

# Any light?
out-mes/mes --help

# Simplest of tests
echo '(display "hello\n")' | MES_BOOT=boot-01.scm out-mes/mes

# Basic test.  We should be safe, but there are ~30 more in tests/*.test.
MES_DEBUG=4 MES=out-mes/mes tests/base.test

# GC test
MES_DEBUG=3 MES_ARENA=10000 MES_MAX_ARENA=10000 MES_BOOT=scaffold/gc-test.scm out-mes/mes

# MesCC test
MES_DEBUG=2 MES=out-mes/mes sh -x scripts/mescc -m $mes_bits -nostdlib\
         -I include -I include/$mes_kernel/$mes_cpu\
         -o out-mes/hello\
         lib/linux/$mes_cpu-mes-mescc/crt1.c\
         \
         lib/mes/eputs.c\
         \
         lib/linux/$mes_cpu-mes-mescc/mini.c\
         \
         lib/mes/write.c\
         lib/string/strlen.c\
         \
         scaffold/hello.c
set +e
out-mes/hello
r=$?
if [ $r != 42 ]; then
   exit 1
fi
