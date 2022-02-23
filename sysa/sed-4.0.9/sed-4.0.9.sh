# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    cp lib/regex_.h lib/regex.h
    touch config.h
}

src_install() {
    default
}
