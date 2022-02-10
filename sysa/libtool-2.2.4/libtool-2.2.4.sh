# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=7e6a1082a4785a7b58928865a40ed2c93940af54972a2dc30ff10185da97b491

src_prepare() {
    rm libltdl/config/ltmain.sh

    rm doc/*.info

    ./bootstrap
}

src_configure() {
    CC=tcc \
        EGREP="/usr/bin/grep -E" \
        FGREP="/usr/bin/grep -F" \
        GREP="/usr/bin/grep" \
        SED="/usr/bin/sed" \
        ./configure \
        --prefix="${PREFIX}" \
        --libdir="${PREFIX}/lib/musl" \
        --disable-shared \
        --host=i386-unknown-linux \
        --target=i386-unknown-linux \
        --build=i386-unknown-linux
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"
}
