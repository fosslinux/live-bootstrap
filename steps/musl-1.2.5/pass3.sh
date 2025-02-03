# SPDX-FileCopyrightText: 2022 Dor Askayo <dor.askayo@gmail.com>
# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2023 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_configure() {
    ./configure \
        --host=i386-unknown-linux-musl \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --includedir="${PREFIX}/include/"
}

src_compile() {
    make "${MAKEJOBS}" CROSS_COMPILE=
}

src_install() {
    default

    # Make dynamic linker symlink relative in ${PREFIX}/lib
    rm "${DESTDIR}/lib/ld-musl-i386.so.1"
    rmdir "${DESTDIR}/lib"
    mkdir -p "${DESTDIR}${PREFIX}/lib"
    ln -sr "${DESTDIR}${LIBDIR}/libc.so" "${DESTDIR}${PREFIX}/lib/ld-musl-i386.so.1"

    # Make startup objects available in /usr/lib
    # Expected by GCC 10+
    for i in crt1.o crti.o crtn.o Scrt1.o rcrt1.o; do
        ln -sr "${DESTDIR}${LIBDIR}/${i}" "${DESTDIR}${PREFIX}/lib/${i}"
    done

    # Add symlink for ldd
    mkdir -p "${DESTDIR}${PREFIX}/bin"
    ln -s ../lib/ld-musl-i386.so.1 "${DESTDIR}${PREFIX}/bin/ldd"

    # Add library search path configurtion
    mkdir -p "${DESTDIR}/etc"
    cp ld-musl-i386.path "${DESTDIR}/etc"
}

src_postprocess() {
    # Stripping libc can cause some strange brokenness
    :
}
