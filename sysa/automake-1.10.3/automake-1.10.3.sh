# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    rm doc/amhello-1.0.tar.gz doc/automake.info*

    ./bootstrap
}

src_configure() {
    ./configure CC=tcc --prefix="${PREFIX}"
}

src_compile() {
    make MAKEINFO=true CC=tcc
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"
    rm "${DESTDIR}${PREFIX}/share/doc/automake/amhello-1.0.tar.gz"

    # Delete some stray directories
    rm -r "${DESTDIR}${SOURCES}"
}
