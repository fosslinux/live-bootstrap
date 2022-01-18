# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=e0e13b46d2aaee18d6dd0bbd86c47c1ca951c78e7c5e8e76f79fd14c2502e6ea

src_prepare() {
    rm configure Makefile.in */Makefile.in */*/Makefile.in aclocal.m4 automake.info*
    aclocal-1.6
    autoconf-2.52
    automake-1.6
}

src_configure() {
    ./configure --prefix="${PREFIX}"
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    # cleanup old manual install
    rm "${PREFIX}"/bin/automake-1.6
    rm "${PREFIX}"/bin/aclocal-1.6
    rm -rf "${PREFIX}"/share/automake-1.6
    rm -rf "${PREFIX}"/share/aclocal-1.6

    make install MAKEINFO=true DESTDIR="${DESTDIR}"
}
