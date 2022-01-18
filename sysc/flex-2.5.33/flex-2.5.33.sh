# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=f9ef1a90b9472c75a5d40a8fca6f7a2539541e28026c6dddf3ca4762edb30814

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
        --libdir="${PREFIX}/lib/musl" \
        --program-suffix=-2.5.33
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
