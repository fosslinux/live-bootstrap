# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_compile() {
    cp autoconf.in autoconf
    sed -i -e "s# @SHELL@#/bin/sh#" -e 's/@M4@/m4/' -e 's/@AWK@/awk/' \
	-e  's/@PACKAGE_NAME@/Autoconf/' -e 's/@VERSION@/2.52/' \
	-e "s#@datadir@#${PREFIX}/share/autoconf-2.52#" autoconf
    chmod +x autoconf

    m4 autoconf.m4 --freeze-state=autoconf.m4f
}

src_install() {
    install autoconf ${DESTDIR}${PREFIX}/bin/autoconf-2.52
    mkdir -p ${DESTDIR}${PREFIX}/share/autoconf-2.52
    cp -r *.m4* ${DESTDIR}${PREFIX}/share/autoconf-2.52/
}
