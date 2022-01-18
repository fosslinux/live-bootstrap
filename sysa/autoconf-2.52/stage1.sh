# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=7c92699f3b1c2f63a18b8a771d00128aa8992403c47a9e552cccad741ced61ac

src_compile() {
    rm doc/standards.info doc/autoconf.info
    cp autoconf.in autoconf
    sed -i -e "s# @SHELL@#/bin/sh#" -e 's/@M4@/m4/' -e 's/@AWK@/awk/' \
	-e  's/@PACKAGE_NAME@/Autoconf/' -e 's/@VERSION@/2.52/' \
	-e "s#@datadir@#${PREFIX}/share/autoconf-2.52#" autoconf
    chmod +x autoconf

    m4 autoconf.m4 --freeze-state=autoconf.m4f
}

src_install() {
    install -D autoconf ${DESTDIR}${PREFIX}/bin/autoconf-2.52
    mkdir -p ${DESTDIR}${PREFIX}/share/autoconf-2.52
    cp -r *.m4* ${DESTDIR}${PREFIX}/share/autoconf-2.52/
}
