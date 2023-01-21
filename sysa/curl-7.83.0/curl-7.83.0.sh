# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # Regnerate src/tool_cb_prg.c
    sed -i "53,74d" src/tool_cb_prg.c
    sed -i "53 s/^/$(perl sinus.pl | sed "s/, $//")\n/" src/tool_cb_prg.c

    rm src/tool_help.c src/tool_help.h src/tool_listhelp.c src/tool_hugehelp.c

    # Rebuild libtool files
    rm config.guess config.sub ltmain.sh
    libtoolize

    AUTOMAKE=automake-1.10 ACLOCAL=aclocal-1.10 AUTOM4TE=autom4te-2.64 AUTOCONF=autoconf-2.64 autoreconf-2.64 -fi
}

src_configure() {
    LDFLAGS="-static" ./configure \
        --prefix="${PREFIX}" \
        --libdir="${PREFIX}/lib/musl" \
        --build=i386-unknown-linux-gnu \
        --enable-ipv6 \
        --without-ssl \
        --disable-hsts
}

src_install() {
    default
    install -m 755 scripts/mk-ca-bundle.pl "${DESTDIR}/usr/bin/mk-ca-bundle"
}
