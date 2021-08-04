# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space> 
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    rm doc/make.info
    touch doc/make.info

    # We don't have autopoint from gettext yet
    AUTOPOINT=true autoreconf -fi
}

src_configure() {
    ./configure \
        --prefix="${PREFIX}" \
        --disable-nls
}

src_compile() {
    make MAKEINFO="true"
}

src_install() {
    make install MAKEINFO="true" DESTDIR="${DESTDIR}"
}
