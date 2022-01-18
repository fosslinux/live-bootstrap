# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=aa900b99ec2fedc1bf9a1bc366b208f73f3f866c2b82d27ed8f4fccf538cb0e5

src_prepare() {
    default

    cp lib/regex_.h lib/regex.h
    touch config.h
}

src_install() {
    default
}
