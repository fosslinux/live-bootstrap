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

AM_CPPFLAGS="
-D HAVE_CONFIG_H=1
-I ${srcdest}include
-I ${srcdest}include/$mes_kernel/$mes_cpu
"

if test $mes_kernel = gnu; then
    AM_CPPFLAGS="$AM_CPPFLAGS
-I /usr/include
"
fi

AM_CFLAGS=

if test $mes_libc = mes; then
    AM_CFLAGS="$AM_CFLAGS
-static
-nostdinc
-nostdlib
-fno-builtin
"
fi

AM_LDFLAGS="
-L .
"
if test $mes_libc = mes; then
    AM_LDFLAGS="$AM_LDFLAGS
-static
-nostdlib
"
fi
LIBS=-lc

export AM_CFLAGS CFLAGS
export AM_CPPFLAGS CPPFLAGS
export AM_LDFLAGS LDFLAGS
export LIBS
