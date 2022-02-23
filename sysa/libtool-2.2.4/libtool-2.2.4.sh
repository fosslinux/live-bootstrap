# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    rm libltdl/config/ltmain.sh

    rm doc/*.info

    ./bootstrap
}

src_configure() {
    CC=tcc ./configure \
        --prefix="${PREFIX}" \
        --libdir="${PREFIX}/lib/musl" \
        --disable-shared \
        --host=i386-unknown-linux \
        --target=i386-unknown-linux \
        --build=i386-unknown-linux \
        ac_path_EGREP="/usr/bin/grep -E" \
        ac_path_FGREP="/usr/bin/grep -F" \
        ac_path_GREP="/usr/bin/grep" \
        ac_path_SED="/usr/bin/sed"
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"
}
