# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

urls="http://gentoo.osuosl.org/distfiles/pkg-config-0.29.2.tar.gz"

src_prepare() {
    autoreconf -fi
}

src_configure() {
    # We use internal glib because standalone glib library depends on
    # pkg-config and other software (python/meson) that we do not have.
    ./configure \
        --prefix="${PREFIX}" \
        --build=i386-unknown-linux-musl \
        --with-internal-glib
}
