# SPDX-FileCopyrightText: 2024 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # texinfo
    rm doc/*.info

    # bison
    rm awkgram.c command.c
}

src_configure() {
    ./configure --prefix="${PREFIX}"
}
