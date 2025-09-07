# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    rm libtoolize.in

    rm -f libltdl/config/ltmain.sh libtool libltdl/m4/ltversion.m4

    rm -f doc/*.info
    rm -f tests/testsuite tests/defs.in tests/package.m4

    find tests -name configure | while read d; do
        rm -r "$(dirname "$d")"
    done

    AUTOMAKE=automake-1.10 ACLOCAL=aclocal-1.10 AUTOM4TE=autom4te-2.61 AUTOCONF=autoconf-2.61 AUTOHEADER=autoheader-2.61 AUTORECONF=autoreconf-2.61 ./bootstrap
}

src_configure() {
    LD=tcc CC=tcc AR="true" RANLIB=true ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --disable-shared \
        --disable-ltdl-install \
        --host=i386-unknown-linux \
        --target=i386-unknown-linux \
        --build=i386-unknown-linux \
        ac_path_EGREP="egrep" \
        ac_path_FGREP="fgrep" \
        ac_path_GREP="grep" \
        ac_path_SED="sed"
}

src_compile() {
    make "${MAKEJOBS}" AUTOM4TE=autom4te-2.61 MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"

    sed -i -e "s/{EGREP=.*/{EGREP='egrep'}/" \
        -e "s/{FGREP=.*/{FREGP='fgrep'}/" \
        -e "s/{GREP=.*/{GREP='grep'}/" \
        -e "s/{SED=.*/{SED='sed'}/" \
        "${DESTDIR}/usr/bin/libtool"
}
