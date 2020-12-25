#! /bin/sh

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

set -e

if [ "$V" = 2 ]; then
    set -x
fi

t=${1-scaffold/boot/00-zero.scm}
b=$(basename "$t" .scm)

if [ "$(basename $MES)" = guile ]; then
    $MES -L ${srcdest}module -C module -L . -c '(begin (use-modules (mes guile)) (include-from-path "'"$t"'"))'
elif [ -z "${b/5[0-9]-*/}" ]; then
    cat "$t" | MES_BOOT=boot-00.scm $MES
elif [ -z "${b/6[0-9]-*/}" ]; then
    cat "$t" | MES_BOOT=boot-01.scm $MES
else
    MES_BOOT=$t $MES;
fi
