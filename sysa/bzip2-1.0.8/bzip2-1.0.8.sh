# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    :
}

src_compile() {
   make CC=tcc AR="tcc -ar" bzip2
}

src_install() {
    # Manually install bzip2 (cannot replace binary while running)
    install -D bzip2 "${PREFIX}/bin/bzip2"
    mkdir -p "${DESTDIR}${PREFIX}/bin"
    ln -sf "${PREFIX}/bin/bzip2" "${DESTDIR}${PREFIX}/bin/bunzip2"
    ln -sf "${PREFIX}/bin/bzip2" "${DESTDIR}${PREFIX}/bin/bzcat"
}
