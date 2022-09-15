# SPDX-FileCopyrightText: 2022 Dor Askayo <dor.askayo@gmail.com>
# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

urls="https://musl.libc.org/releases/musl-1.2.3.tar.gz"

src_configure() {
    ./configure \
        --host=i386-unknown-linux-musl \
        --prefix="${PREFIX}" \
        --libdir="${PREFIX}/lib/musl" \
        --includedir="${PREFIX}/include/"
}

src_compile() {
    make CROSS_COMPILE=
}

src_install() {
    default

    # Make dynamic linker symlink relative in ${PREFIX}/lib
    rm "${DESTDIR}/lib/ld-musl-i386.so.1"
    rmdir "${DESTDIR}/lib"
    mkdir -p "${DESTDIR}${PREFIX}/lib"
    ln -sr "${DESTDIR}${PREFIX}/lib/musl/libc.so" "${DESTDIR}${PREFIX}/lib/ld-musl-i386.so.1"

    # Add symlink for ldd
    mkdir -p "${DESTDIR}${PREFIX}/bin"
    ln -s ../lib/ld-musl-i386.so.1 "${DESTDIR}${PREFIX}/bin/ldd"

    # Add library search path configurtion
    mkdir -p "${DESTDIR}/etc"
    cp ../../ld-musl-i386.path "${DESTDIR}/etc"
}
