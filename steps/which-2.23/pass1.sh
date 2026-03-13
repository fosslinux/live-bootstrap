# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_unpack() {
    default
    mv cwm4* cwm4
}

src_prepare() {
    default
    rmdir cwm4
    ln -s ../cwm4 cwm4
    touch ChangeLog which.1 which.info m4/submodules.m4 version.texi
    ./autogen.sh
    sed -i 's/@CW_SUBDIRS@//' Makefile.in
}

src_configure() {
    ./configure --prefix="${PREFIX}"
}

src_install() {
    default

    rm "${DESTDIR}/${PREFIX}/share/man/man1/which.1"
}
