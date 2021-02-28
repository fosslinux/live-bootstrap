# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_compile() {
    cp autoconf.in autoconf
    sed -i "s# @SHELL@#/bin/sh#" autoconf
    sed -i 's/@M4@/m4/' autoconf
    sed -i 's/@AWK@/awk/' autoconf
    sed -i 's/@PACKAGE_NAME@/Autoconf/' autoconf
    sed -i 's/@VERSION@/2.52/' autoconf
    sed -i "s#@datadir@#${PREFIX}/share/autoconf-2.52#" autoconf
    chmod +x autoconf

    m4 autoconf.m4 --freeze-state=autoconf.m4f
}

src_install() {
    install autoconf ${PREFIX}/bin/autoconf-2.52
    mkdir -p ${PREFIX}/share/autoconf-2.52
    cp -r *.m4* ${PREFIX}/share/autoconf-2.52/
}
