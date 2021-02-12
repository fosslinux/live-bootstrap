# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default_src_prepare
    rm awktab.c
}

src_install() {
    default_src_install

    # Install data files
    install -d "${PREFIX}/share/awk/"
    for file in awklib/eg/lib/*.awk; do
        install -m 644 "$file" "${PREFIX}/share/awk/"
    done
}
