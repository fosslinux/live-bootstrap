# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

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
