# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=2cd5ac019b73c1be35bf08bf7a879b49962d666020a8fdf2823e249d2a13a9e5

src_prepare() {
    default

    touch config.h
}
