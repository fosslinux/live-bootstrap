# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    rm doc/amhello-1.0.tar.gz

    ./bootstrap

    rm doc/automake-history.info doc/automake.info*

    cp "${PREFIX}/bin/help2man" doc/
}

src_configure() {
    ./configure --prefix="${PREFIX}"
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"
    rm "${DESTDIR}${PREFIX}/share/doc/automake/amhello-1.0.tar.gz"
}
