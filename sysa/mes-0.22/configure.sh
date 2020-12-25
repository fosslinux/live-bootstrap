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

VERSION=0.22
srcdir=${srcdir-$(dirname $0)}
srcdest=
if test "$srcdir" != "."; then
    srcdest=$srcdir/
fi
. ${srcdest}build-aux/trace.sh

# parse arguments
while [ $# -gt 0 ]; do
    case $1 in
        (--with-courage)
            courageous=true
            ;;
        (--with-system-libc)
            mes_libc=system
            ;;
        (--build=*)
            build=${1#--build=}
            host=${host-$build}
            ;;
        (--host=*)
            host=${1#--host=}
            ;;
        (--prefix=*)
            prefix=${1#--prefix=}
            ;;
        (--program-prefix=*)
            program_prefix=${1#--program-prefix=}
            ;;
    esac
    shift
done

prefix=${prefix-/usr/local}
mes_libc=${mes_libc-mes}
courageous=${courageous-false}

BASH=${BASH-$(command -v bash || command -v sh)}
BLOOD_ELF=${BLOOD_ELF-$(command -v blood-elf)}
CC=${CC-$(command -v gcc)} || true
CFLAGS=${CFLAGS-}
CPPFLAGS=${CPPFLAGS-}
DIFF=${DIFF-$(command -v diff || echo $PWD/pre-inst-env diff.scm)}
GUILD=${GUILD-$(command -v guild)} || true
GUILE_TOOLS=${GUILE_TOOLS-$(command -v guile-tools)} || true
if test ! "$GUILD"; then
    if test "$GUILE_TOOLS"; then
        GUILD=$GUILE_TOOLS
    else
        GUILD=true
    fi
fi
GUILE=${GUILE-$(command -v guile)} || true
HEX2=${HEX2-$(command -v hex2)}
LDPFLAGS=${LDFLAGS-}
M1=${M1-$(command -v M1)}
MES_FOR_BUILD=${MES_FOR_BUILD-$(command -v mes || command -v guile || echo mes)}
GIT=${GIT-$(command -v git)} || true
PERL=${PERL-$(command -v perl)} || true
SHELL=${BASH-$SHELL}

if test "$srcdir" = .; then
    top_builddir=.
else
    srcdest=${srcdest}
    top_builddir=$PWD
fi
abs_top_srcdir=${abs_top_srcdir-$(cd ${srcdir} && pwd)}
abs_top_builddir=$PWD

if test -z "$GUILE" -o "$GUILE" = true; then
    GUILE_EFFECTIVE_VERSION=${GUILE_EFFECTIVE_VERSION-2.2}
else
    GUILE_EFFECTIVE_VERSION=${GUILE_EFFECTIVE_VERSION-$(guile -c '(display (effective-version))')}
fi

numbered_arch=false
mes_tools=named
if ( $HEX2 --help 2>&1 ) | grep -q "\-\-Architecture 12345"; then
    numbered_arch=true
    mes_tools=numbered
fi

bindir=$(eval echo ${bindir-$prefix/bin})
datadir=$(eval echo ${datadir-$prefix/share})
docdir=$(eval echo ${docdir-$datadir/doc/mes})
infodir=$(eval echo ${infodir-$datadir/info})
includedir=$(eval echo ${includedir-$prefix/include})
libdir=$(eval echo ${libdir-$prefix/lib})
pkgdatadir=$(eval echo ${pkgdatadir-$datadir/mes})
mandir=$(eval echo ${mandir-$datadir/man})
guile_site_dir=$(eval echo ${guile_site_dir-$prefix/share/guile/site/$GUILE_EFFECTIVE_VERSION})
guile_site_ccache_dir=$(eval echo ${guile_site_ccache_dir-$prefix/lib/guile/$GUILE_EFFECTIVE_VERSION/site-ccache})

subst () {
    echo "creating $2"
    sed \
    -e s,"@PACKAGE@,$PACKAGE,"\
    -e s,"@PACKAGE_NAME@,$PACKAGE_NAME,"\
    -e s,"@PACKAGE_BUGREPORT@,$PACKAGE_BUGREPORT,"\
    -e s,"@VERSION@,$VERSION,"\
    -e s,"@bootstrap@,true,"\
    -e s,"@build@,$build,"\
    -e s,"@host@,$host,"\
    -e s,"@compiler@,$compiler,"\
    -e s,"@courageous@,$courageous,"\
    -e s,"@mes_bits@,$mes_bits,"\
    -e s,"@mes_kernel@,$mes_kernel,"\
    -e s,"@mes_cpu@,$mes_cpu,"\
    -e s,"@mes_libc@,$mes_libc,"\
    -e s,"@mes_system@,$mes_system,"\
    -e s,"@abs_top_srcdir@,$abs_top_srcdir,"\
    -e s,"@abs_top_builddir@,$abs_top_builddir,"\
    -e s,"@top_builddir@,$top_builddir,"\
    -e s,"@srcdest@,$srcdest,"\
    -e s,"@srcdir@,$srcdir,"\
    -e s,"@prefix@,$prefix,"\
    -e s,"@program_prefix@,$program_prefix,"\
    -e s,"@bindir@,$bindir,"\
    -e s,"@datadir@,$datadir,"\
    -e s,"@docdir@,$docdir,"\
    -e s,"@guile_site_dir@,$guile_site_dir,"\
    -e s,"@guile_site_ccache_dir@,$guile_site_ccache_dir,"\
    -e s,"@infodir@,$infodir,"\
    -e s,"@includedir@,$includedir,"\
    -e s,"@libdir@,$libdir,"\
    -e s,"@mandir@,$mandir,"\
    -e s,"@pkgdatadir@,$pkgdatadir,"\
    -e s,"@sysconfdir@,$sysconfdir,"\
    -e s,"@GUILE_EFFECTIVE_VERSION@,$GUILE_EFFECTIVE_VERSION,"\
    -e s,"@GUILE_LOAD_PATH@,$GUILE_LOAD_PATH,"\
    -e s,"@V@,$V,"\
    -e s,"@AR@,$AR,"\
    -e s,"@BASH@,$BASH,"\
    -e s,"@BLOOD_ELF@,$BLOOD_ELF,"\
    -e s,"@CC@,$CC,"\
    -e s,"@DIFF@,$DIFF,"\
    -e s,"@GIT@,$GIT,"\
    -e s,"@GUILD@,$GUILD,"\
    -e s,"@GUILE@,$GUILE,"\
    -e s,"@PERL@,$PERL,"\
    -e s,"@CFLAGS@,$CFLAGS,"\
    -e s,"@HEX2@,$HEX2,"\
    -e s,"@HEX2FLAGS@,$HEX2FLAGS,"\
    -e s,"@M1@,$M1,"\
    -e s,"@M1FLAGS@,$M1FLAGS,"\
    -e s,"@MES_FOR_BUILD@,$MES_FOR_BUILD,"\
    -e s,"@numbered_arch@,$numbered_arch,"\
    -e s,"@SHELL@,$SHELL,"\
    $1 > $2
}

host=${host-$($CC -dumpmachine 2>/dev/null)}
if test -z "$host$host_type"; then
    mes_cpu=${arch-$(get_machine || uname -m)}
else
    mes_cpu=${host%%-*}
fi
if test "$mes_cpu" = i386\
        || test "$mes_cpu" = i486\
        || test "$mes_cpu" = i586\
        || test "$mes_cpu" = i686; then
    mes_cpu=x86
fi
if test "$mes_cpu" = armv4\
        || test "$arch" = armv7l; then
    mes_cpu=arm
fi
if test "$mes_cpu" = amd64; then
    mes_cpu=x86_64
fi

case "$host" in
    *linux-gnu|*linux)
        mes_kernel=linux;;
    *gnu)
        mes_kernel=gnu;;
    *freebsd*)
        mes_kernel=frreebsd;;
    *)
        mes_kernel=linux;;
esac

case "$mes_cpu" in
    x86_64)
        mes_bits=64;;
    *)
        mes_bits=32;;
esac
#
if $CC --version | grep gcc; then
    compiler=gcc
elif $CC --version | grep tcc; then
    compiler=gcc
else
    compiler=mescc
    AR=${AR-$PWD/scripts/mesar}
fi

AR=${AR-$(command -v ar)} || true

mes_system=$mes_cpu-$mes_kernel-mes

mkdir -p scripts
subst ${srcdest}build-aux/GNUmakefile.in GNUmakefile
subst ${srcdest}build-aux/config.sh.in config.sh
subst ${srcdest}build-aux/bootstrap.sh.in bootstrap.sh
chmod +x bootstrap.sh
subst ${srcdest}build-aux/build.sh.in build.sh
chmod +x build.sh
subst ${srcdest}build-aux/check.sh.in check.sh
chmod +x check.sh
subst ${srcdest}build-aux/install.sh.in install.sh
chmod +x install.sh
subst ${srcdest}build-aux/pre-inst-env.in pre-inst-env
chmod +x pre-inst-env
subst ${srcdest}scripts/mesar.in scripts/mesar
chmod +x scripts/mesar
subst ${srcdest}scripts/mescc.scm.in scripts/mescc.scm
chmod +x scripts/mescc.scm
subst ${srcdest}scripts/mescc.in scripts/mescc
chmod +x scripts/mescc
subst ${srcdest}build-aux/uninstall.sh.in uninstall.sh
chmod +x uninstall.sh

mkdir -p include/mes
rm -f include/mes/config.h
if test $mes_libc = system; then
    cat >> include/mes/config.h <<EOF
#define SYSTEM_LIBC 1
EOF
else
    cat >> include/mes/config.h <<EOF
#undef SYSTEM_LIBC
EOF
fi
cat >> include/mes/config.h <<EOF
#define MES_VERSION "$VERSION"
EOF

cat <<EOF
GNU Mes is configured for
   compiler:   $compiler
   cpu:        $mes_cpu
   bits:       $mes_bits
   libc:       $mes_libc
   kernel:     $mes_kernel
   system:     $mes_system
   tools:      $mes_tools arch
   courageous: $courageous
   bootstrap:  yes

Run:
  sh bootstrap.sh  to bootstrap build mes
  sh check.sh      to check mes
  sh install.sh    to install mes
EOF
