# GNU Mes --- Maxwell Equations of Software
# Copyright © 2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

objects=
compile () {
    c=${srcdest}$1
    b=$(basename $c .c)
    o=$b.o
    objects="$objects $o"
    if test ! -e $o -o $c -nt $o; then
        trace "CC         $c" $CC -c $AM_CPPFLAGS $CPPFLAGS $AM_CFLAGS $CFLAGS -o $o $c
        $CC -c $AM_CPPFLAGS $CPPFLAGS $AM_CFLAGS $CFLAGS -o $o $c
    fi
}

archive () {
    archive=$1
    shift
    sources="$@"
    objects=
    for c in $sources; do
        b=$(basename $c .c)
        o=$b.o
        compile $c
    done
    trace "AR         $archive" $AR crD $archive $objects
    objects=
}

link () {
    out=$1
    d=$(dirname $out)
    mkdir -p $d
    if test $mes_libc = system; then
        crt1=
    else
        crt1=crt1.o
    fi
    trace "CCLD       $out" $CC $AM_CFLAGS $CFLAGS $AM_LDFLAGS $LDFLAGS -o $out $crt1 $objects $LIBS
    objects=
}
