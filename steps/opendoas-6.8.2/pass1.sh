# SPDX-FileCopyrightText: 2024 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_configure() {
    ./configure --prefix="${PREFIX}" \
        --without-pam
}

src_compile() {
    make -f GNUmakefile "${MAKEJOBS}" PREFIX="${PREFIX}"
}

src_install() {
    make -f GNUmakefile install PREFIX="${PREFIX}" DESTDIR="${DESTDIR}"

    ln -s doas "${DESTDIR}${PREFIX}/bin/sudo"

    install -D -m 400 ../../files/doas.conf "${DESTDIR}/etc/doas.conf"
}
