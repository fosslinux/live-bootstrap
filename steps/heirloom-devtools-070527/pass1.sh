#!/bin/sh

# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2024 Gábor Stefanik <netrolller.3d@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_compile() {
    cd lex
    make -f Makefile.mk CC=tcc AR=tcc\ -ar LDFLAGS=-static RANLIB=true
    cd ..
}

src_install() {
    mkdir -p "${DESTDIR}${BINDIR}" "${DESTDIR}${LIBDIR}/lex"
    install lex/lex "${DESTDIR}${BINDIR}"
    install lex/libl.a "${DESTDIR}${LIBDIR}"
    install -m 644 lex/ncform "${DESTDIR}${LIBDIR}/lex"
}

