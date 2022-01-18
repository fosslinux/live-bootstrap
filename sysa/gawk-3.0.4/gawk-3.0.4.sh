# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=4c151b8fac8731d6f7ffb5279353d4f16e2cbfe7374d48c48a571ae09d7a9c50

src_prepare() {
    default
    rm awktab.c
}

src_install() {
    default

    # Install data files
    install -d "${DESTDIR}${PREFIX}/share/awk/"
    for file in awklib/eg/lib/*.awk; do
        install -m 644 "$file" "${DESTDIR}${PREFIX}/share/awk/"
    done
}
