# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# When we rebuild flex it no longer needs patching
# and can use simplified makefile
src_prepare() {
    default

    rm -r to.do

    touch config.h
}

src_compile() {
    make -j1 PREFIX="${PREFIX}"
}
