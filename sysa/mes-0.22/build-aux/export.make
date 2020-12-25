# GNU Mes --- Maxwell Equations of Software
# Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

export PACKAGE
export PACKAGE_NAME
export PACKAGE_BUGREPORT
export VERSION

export abs_top_builddir
export abs_top_srcdir
export top_builddir
export config.make
export srcdest
export srcdir

export prefix

export datadir
export docdir

export bindir
export guile_site_ccache_dir
export guile_site_dir
export infodir
export libdir
export mandir
export moduledir
export sysconfdir

export bootstrap
export compiler
export courageous
export mes_system
export mes_cpu
export mes_bits
export mes_libc
export mes_kernel

ifdef V
export V
endif

ifdef DESTDIR
export DESTDIR
endif

ifdef CC
export CC
endif

ifdef BLOOD_ELF
export BLOOD_ELF
endif

ifdef DOT
export DOT
endif

ifdef M1
export M1
endif

ifdef MES
export MES
endif

ifdef MES_FOR_BUILD
export MES_FOR_BUILD
endif

ifdef MESCC
export MESCC
endif

ifdef HEX2
export HEX2
endif

ifdef HELP2MAN
export HELP2MAN
endif

ifdef GUILE
export GUILE
endif

ifdef GUILD
export GUILD
endif

ifdef GUIX
export GUIX
endif

ifdef PERL
export PERL
endif

ifdef SCHEME
export SCHEME
endif

ifdef SHELL
export SHELL
endif

ifdef GUILE_LOAD_PATH
export GUILE_LOAD_PATH
endif

ifdef GUILE_LOAD_COMPILED_PATH
export GUILE_LOAD_COMPILED_PATH
endif

ifdef AM_CFLAGS
export AM_CFLAGS
endif

ifdef AM_CPPFLAGS
export AM_CPPFLAGS
endif

ifdef AM_LDFLAGS
export AM_LDFLAGS
endif

ifdef CFLAGS
export CFLAGS
endif

ifdef CPPFLAGS
export CPPFLAGS
endif

ifdef LDFLAGS
export LDFLAGS
endif

ifdef HEX2FLAGS
export HEX2FLAGS
endif

ifdef M1FLAGS
export M1FLAGS
endif
