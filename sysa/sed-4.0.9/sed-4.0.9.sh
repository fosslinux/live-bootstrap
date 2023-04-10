# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    touch config.h
}

src_compile() {
    make -j1 PREFIX="${PREFIX}"
}

src_install() {
    default
}
