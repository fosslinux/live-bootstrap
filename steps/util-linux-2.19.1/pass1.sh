# SPDX-FileCopyrightText: 2021-23 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # gettext translation files
    rm po/*.gmo

    # NOTE: misc-utils/scriptreplay.1 was originally a Pod::Man generated
    # manpage. However, it *has* been manually edited since then. In this
    # case, it is fine not to delete it; there is little-no risk.

    # generated/unauditable testdata
    rm -r tests/ts/blkid/images-fs tests/ts/cramfs/*.img

    # We don't have gettext (autopoint) yet.
    AUTOPOINT=true AUTOMAKE=automake-1.10 AUTOCONF=autoconf-2.64 ACLOCAL=aclocal-1.10 AUTOM4TE=autom4te-2.64 autoreconf-2.64 -fi
}

src_configure() {
    ./configure --prefix="${PREFIX}" \
        --bindir="${PREFIX}/bin" \
        --sbindir="${PREFIX}/bin" \
        --libdir="${LIBDIR}" \
        --build=i386-unknown-linux-gnu \
        --disable-cramfs \
        --without-ncurses \
        --enable-static \
        --enable-static-programs=losetup,mount,umount,fdisk,sfdisk,blkid \
        --enable-shared=no \
        --disable-wall \
        ac_cv_type_loff_t=yes
}

src_install() {
    default

    # A weird behaviour I can't find the source of
    mv "${DESTDIR}${PREFIX}/i386-unknown-linux-musl/"* "${DESTDIR}${LIBDIR}/"
    rmdir "${DESTDIR}${PREFIX}/i386-unknown-linux-musl/"
}
