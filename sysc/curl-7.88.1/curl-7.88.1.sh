# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # Regnerate src/tool_cb_prg.c
    sed -i "55,76d" src/tool_cb_prg.c
    sed -i "55 s/^/$(perl sinus.pl | sed "s/, $//")\n/" src/tool_cb_prg.c

    rm src/tool_help.c src/tool_help.h src/tool_listhelp.c src/tool_hugehelp.c

    # Rebuild libtool files
    rm config.guess config.sub ltmain.sh
    libtoolize

    AUTOMAKE=automake-1.15 ACLOCAL=aclocal-1.15 autoreconf-2.69 -fi
}

src_configure() {
    LDFLAGS="-static" ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --enable-ipv6 \
        --with-openssl \
        --with-ca-bundle=/etc/ssl/certs.pem \
        --build=i386-unknown-linux-musl
}
