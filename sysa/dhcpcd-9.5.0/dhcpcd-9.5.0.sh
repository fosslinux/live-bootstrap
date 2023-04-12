# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    rm src/dhcpcd-embedded.c.in
}

src_configure() {
    CC=gcc ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --sbindir="${PREFIX}/bin" \
        --disable-embedded \
        --disable-auth
}

src_install() {
    default
    mkdir -p "${DESTDIR}/var/db/dhcpcd"
    mkdir -p "${DESTDIR}/var/run/dhcpcd"
}
