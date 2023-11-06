#!/bin/bash
# SPDX-FileCopyrightText: 2023 Richard Masters <grick23@gmail.com>
# SPDX-License-Identifier: MIT
src_get() {
    :
}

src_unpack() {
    dirname=kexec-linux-1.0.0
    mkdir ${dirname}
}

src_install() {
    install -D "kexec-linux" "${DESTDIR}${PREFIX}/bin/kexec-linux"
}
