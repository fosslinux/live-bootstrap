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

. ./config.sh
. ${srcdest}build-aux/configure-lib.sh
. ${srcdest}build-aux/trace.sh
. ${srcdest}build-aux/cc.sh

trap 'test -f .log && cat .log' EXIT

mkdir -p $mes_cpu-mes
compile lib/$mes_kernel/$mes_cpu-mes-$compiler/crt1.c
cp crt1.o $mes_cpu-mes
if test -e crt1.s; then
    cp crt1.s $mes_cpu-mes
fi

archive libc-mini.a $libc_mini_SOURCES
cp libc-mini.a $mes_cpu-mes
if test -e libc-mini.s; then
    cp libc-mini.s $mes_cpu-mes
fi

archive libmes.a $libmes_SOURCES
cp libmes.a $mes_cpu-mes
if test -e libmes.s; then
    cp libmes.s $mes_cpu-mes
fi

if test $mes_libc = mes; then
    archive libc.a $libc_SOURCES
    cp libc.a $mes_cpu-mes
    if test -e libc.s; then
        cp libc.s $mes_cpu-mes
    fi
fi

archive libc+tcc.a $libc_tcc_SOURCES
cp libc+tcc.a $mes_cpu-mes
if test -e libc+tcc.s; then
    cp libc+tcc.s $mes_cpu-mes
fi

if $courageous; then
    exit 0
fi

archive libc+gnu.a $libc_gnu_SOURCES
cp libc+gnu.a $mes_cpu-mes
if test -e libc+gnu.s; then
    cp libc+gnu.s $mes_cpu-mes
fi

archive libtcc1.a $libtcc1_SOURCES
cp libtcc1.a $mes_cpu-mes
if test -e libtcc1.s; then
    cp libtcc1.s $mes_cpu-mes
fi

archive libgetopt.a lib/posix/getopt.c
cp libgetopt.a $mes_cpu-mes
if test -e libgetopt.s; then
    cp libgetopt.s $mes_cpu-mes
fi
