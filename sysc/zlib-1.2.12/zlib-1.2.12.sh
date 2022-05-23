# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

urls="https://zlib.net/zlib-1.2.12.tar.xz"

src_configure() {
    ./configure --prefix="${PREFIX}" --libdir="${PREFIX}/lib/musl" --static
}
