# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    rm -- configure Makefile.in */Makefile.in */*/Makefile.in aclocal.m4 automake.info*
    cp aclocal.in aclocal
    cp m4/amversion.in m4/amversion.m4
}

src_compile() {
    sed -i -e 's/@VERSION@/1.6.3/' -e 's/@APIVERSION@/1.6/' m4/amversion.m4

    sed -i -e "s#@PERL@#${PREFIX}/bin/perl#" -e 's/@PACKAGE@/automake/' \
	-e 's/@APIVERSION@/1.6/' -e 's/@VERSION@/1.6.3/' \
	-e "s#@prefix@#${PREFIX}#" -e "s#@datadir@#${PREFIX}/share#" aclocal
}

src_install() {
    mkdir -p "${DESTDIR}${PREFIX}"/share/automake-1.6/Automake
    cp lib/Automake/*.pm "${DESTDIR}${PREFIX}"/share/automake-1.6/Automake/

    install -D aclocal "${DESTDIR}${PREFIX}"/bin/aclocal-1.6
    mkdir -p "${DESTDIR}${PREFIX}"/share/aclocal-1.6
    cp -r m4/*.m4 "${DESTDIR}${PREFIX}"/share/aclocal-1.6/
}
