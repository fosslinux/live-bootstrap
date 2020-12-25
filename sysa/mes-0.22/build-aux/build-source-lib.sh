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

if test "$V" = 2; then
    set -x
fi

set -u
srcdest=${srcdest-}
. ${srcdest}build-aux/configure-lib.sh

mkdir -p $mes_cpu-mes
cp ${srcdest}lib/$mes_kernel/$mes_cpu-mes-$compiler/crt*.c $mes_cpu-mes

rm -f libc+gnu.c
cat > libc+gnu.c <<EOF
// Generated from Mes -- do not edit
// compiler: $compiler
// cpu:      $mes_cpu
// bits:     $mes_bits
// libc:     $mes_libc
// kernel:   $mes_kernel
// system:   $mes_system

EOF
for c in $libc_gnu_SOURCES; do
    echo "// $c" >> libc+gnu.c
    cat ${srcdest}$c >> libc+gnu.c
    echo >> libc+gnu.c
done
cp libc+gnu.c $mes_cpu-mes

cp ${srcdest}lib/libtcc1.c $mes_cpu-mes
cp ${srcdest}lib/posix/getopt.c $mes_cpu-mes/libgetopt.c
