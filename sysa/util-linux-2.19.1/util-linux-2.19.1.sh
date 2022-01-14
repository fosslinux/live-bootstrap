# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later


src_prepare() {
    default

    # We don't have gettext (autopoint) yet.
    AUTOPOINT=true autoreconf -fi
}

src_configure() {
    ./configure --prefix=${PREFIX} \
        --bindir="${PREFIX}/bin" \
        --sbindir="${PREFIX}/bin" \
        --libdir="${PREFIX}/lib/musl" \
        --disable-libuuid \
        --without-ncurses \
        --enable-static \
        --enable-static-programs=losetup,mount,umount,fdsik,sfdisk,blkid \
        --enable-shared=no \
        --disable-wall \
        ac_cv_type_loff_t=yes
}
