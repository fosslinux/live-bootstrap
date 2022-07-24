# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    rm -f libltdl/config/ltmain.sh

    rm -f doc/*.info

    AUTOMAKE=automake-1.9 ACLOCAL=aclocal-1.9 AUTOM4TE=autom4te-2.61 AUTOCONF=autoconf-2.61 AUTOHEADER=autoheader-2.61 AUTORECONF=autoreconf-2.61 ./bootstrap
}

src_configure() {
    CC=tcc ./configure \
        --prefix="${PREFIX}" \
        --libdir="${PREFIX}/lib/musl" \
        --disable-shared \
        --host=i386-unknown-linux \
        --target=i386-unknown-linux \
        --build=i386-unknown-linux \
        ac_path_EGREP="egrep" \
        ac_path_FGREP="fgrep" \
        ac_path_GREP="grep" \
        ac_path_SED="sed"
}

src_compile() {
    make AUTOM4TE=autom4te-2.61 MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"

    sed -i -e "s/{EGREP=.*/{EGREP='egrep'}/" \
        -e "s/{FGREP=.*/{FREGP='fgrep'}/" \
        -e "s/{GREP=.*/{GREP='grep'}/" \
        -e "s/{SED=.*/{SED='sed'}/" \
        "${DESTDIR}/usr/bin/libtool"
}
