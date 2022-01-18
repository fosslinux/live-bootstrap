# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=be2c295f7e55e7f26c2d21a8497d38a2da2f5d2c5f629337531e43e6570c41e1

src_configure() {
    ./configure --prefix="${PREFIX}" --libdir="${PREFIX}/lib/musl" --static
}
