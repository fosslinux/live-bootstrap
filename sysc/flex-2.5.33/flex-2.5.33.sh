# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    AUTOPOINT=true autoreconf -fi

    # Remove pregenerated files
    rm parse.c parse.h scan.c 

    # Remove pregenerated .info
    rm doc/flex.info
}

src_configure() {
    ./configure \
        --prefix="${PREFIX}" \
        --program-suffix=-2.5.33
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
