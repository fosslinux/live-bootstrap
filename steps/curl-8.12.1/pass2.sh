# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # Regnerate src/tool_cb_prg.c
    sed '/sinus/q' src/tool_cb_prg.c > src/tool_cb_prg.c.new
    perl sinus.pl | sed "s/, $//" >> src/tool_cb_prg.c.new
    sed '1,/^[0-9, ]*[0-9]$/d' src/tool_cb_prg.c >> src/tool_cb_prg.c.new
    mv src/tool_cb_prg.c.new src/tool_cb_prg.c

    # pregenerated files
    rm src/tool_listhelp.c src/tool_hugehelp.c lib/easyoptions.c
    rm docs/libcurl/libcurl-symbols.md
    rm tests/certs/*.der

    AUTOMAKE=automake-1.15 ACLOCAL=aclocal-1.15 autoreconf-2.69 -fi
}

src_configure() {
    LDFLAGS="-static" ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --build="${TARGET}" \
        --enable-ipv6 \
        --with-openssl \
        --without-libpsl \
        --with-ca-bundle=/etc/ssl/certs.pem
}

src_compile() {
    # Recreate tool_help.h
    sed '/bitmask output/{n; n; n; n; q}' src/tool_help.h > src/tool_help.h.new
    make -s -C docs/cmdline-opts listcats >> src/tool_help.h.new
    sed '1,/CURLHELP_VERBOSE/d' src/tool_help.h >> src/tool_help.h.new
    mv src/tool_help.h.new src/tool_help.h

    # tool_listhelp.c and easyoptions.c
    make -C src listhelp
    make -C lib optiontable

    default
}
