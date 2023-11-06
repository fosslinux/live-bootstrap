# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    ACLOCAL=aclocal-1.8 AUTOMAKE=automeke-1.8 autoreconf-2.59 -f

    rm help2man.info
    touch help2man.info
    rm help2man*.1
}

src_configure() {
    CC=tcc ./configure --prefix="${PREFIX}" --disable-nls
}

src_compile() {
    make "${MAKEJOBS}" MAKEINFO=true
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
