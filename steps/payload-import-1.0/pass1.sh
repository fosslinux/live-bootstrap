#!/bin/bash
#
# SPDX-FileCopyrightText: 2026 live-bootstrap contributors
# SPDX-License-Identifier: MIT

src_get() {
    :
}

src_unpack() {
    dirname=.
    cp -r ../src .
}

src_compile() {
    gcc -m32 -march=i386 -std=c89 -static -o payload-import src/payload-import.c
}

src_install() {
    install -D payload-import "${DESTDIR}${BINDIR}/payload-import"
}
