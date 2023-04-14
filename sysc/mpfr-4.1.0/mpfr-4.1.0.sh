# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    find . -name '*.info' -delete
    AUTOMAKE=automake-1.15 ACLOCAL=aclocal-1.15 autoreconf-2.69 -fi
}

src_configure() {
    ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --disable-shared

    # Disable tuning as that might cause non-reproducible build
    mv mparam.h src
}

src_compile() {
    make "${MAKEJOBS}" MAKEINFO=true DESTDIR="${DESTDIR}"
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}

src_postprocess() {
    # For some unexplainable reason, stripping mpfr breaks GCC 10 build.
    # I cannot make any sense of the error, so skip for now.
    :
}
