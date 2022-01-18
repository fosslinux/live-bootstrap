# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=452767f7321aef2eeff9d3a935a574f0680f834f89f2589f3a43951a159b3948

src_prepare() {
    autoreconf-2.71 -fi
}

src_configure() {
    # CFLAGS needed on musl
    ./configure \
        --prefix="${PREFIX}" \
        --libdir="${PREFIX}/lib/musl" \
        --disable-shared \
        CFLAGS='-D_GNU_SOURCE -DNO_GETCONTEXT -DSEARCH_FOR_DATA_START -DUSE_MMAP -DHAVE_DL_ITERATE_PHDR'
}
