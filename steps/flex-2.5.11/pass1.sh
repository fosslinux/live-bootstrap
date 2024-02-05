# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# When we rebuild flex it no longer needs patching
# and can use simplified makefile
src_prepare() {
    default

    touch config.h
    rm parse.c parse.h scan.c skel.c
}

src_compile() {
    make -j1 PREFIX="${PREFIX}"
}
