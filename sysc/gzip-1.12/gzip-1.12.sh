#!/bin/sh

# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

ssrc_prepare() {
    default
}

src_configure() {
./configure --prefix=/usr
}

src_compile() {
make "${MAKEJOBS}"
}

src_install() {
    default
}
