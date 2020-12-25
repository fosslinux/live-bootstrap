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

V=${V-}

if [ "$V" = 2 ]; then
    set -x
fi

. ./config.sh
. ${srcdest}build-aux/trace.sh
. ${srcdest}build-aux/cc.sh

trap 'test -f .log && cat .log' EXIT

mes_sources="
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

for c in $mes_sources; do
    compile $c
done
if test $mes_libc = system; then
    LIBS=-lmes
fi
link bin/mes-$compiler
cp bin/mes-$compiler bin/mes
