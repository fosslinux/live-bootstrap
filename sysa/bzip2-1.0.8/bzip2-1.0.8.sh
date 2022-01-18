# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=02c4d6e6a01c911593eb20bdbc72761cdc977ff48c3ca3b46af4255083cd2679

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
    install -D bzip2 ${DESTDIR}${PREFIX}/bin/bzip2
    ln -sf ${PREFIX}/bin/bzip2 ${DESTDIR}${PREFIX}/bin/bunzip2
    ln -sf ${PREFIX}/bin/bzip2 ${DESTDIR}${PREFIX}/bin/bzcat
}
