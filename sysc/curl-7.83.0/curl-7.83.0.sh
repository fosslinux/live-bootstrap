# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

urls="http://master.dl.sourceforge.net/project/curl.mirror/curl-7_83_0/curl-7.83.0.tar.xz?viasf=1"

src_prepare() {
    default

    # Regnerate src/tool_cb_prg.c
    sed -i "53,74d" src/tool_cb_prg.c
    sed -i "53 s/^/$(perl sinus.pl | sed "s/, $//")\n/" src/tool_cb_prg.c

    rm src/tool_help.c src/tool_help.h src/tool_listhelp.c src/tool_hugehelp.c

    # Rebuild libtool files
    rm config.guess config.sub ltmain.sh
    libtoolize

    autoreconf -fi
}

src_configure() {
    LDFLAGS="-static" ./configure \
        --prefix="${PREFIX}" \
        --build=i386-unknown-linux-gnu \
        --with-openssl \
        --with-ca-bundle=/etc/ssl/certs.pem
}
