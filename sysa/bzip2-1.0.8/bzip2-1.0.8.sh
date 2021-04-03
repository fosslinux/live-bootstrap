# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_unpack() {
    src_dir="${base_dir}/src"
    tar -xf "${src_dir}/${pkg}.tar"
}

src_prepare() {
    :
}

src_compile() {
   make CC=tcc AR="tcc -ar" bzip2
}

src_install() {
    install bzip2 ${DESTDIR}${PREFIX}/bin
    ln -sf ${PREFIX}/bin/bzip2 ${DESTDIR}${PREFIX}/bin/bunzip2
    ln -sf ${PREFIX}/bin/bzip2 ${DESTDIR}${PREFIX}/bin/bzcat
}
