# SPDX-License-Identifier: GPL-3.0-or-later

# Build elfutils against kernel-toolchain dependencies.
: "${KERNEL_SYSROOT:=/kernel-toolchain}"

src_prepare() {
    default

    # Regenerate autotools outputs from source metadata.
    AUTOMAKE=automake-1.15 \
    ACLOCAL=aclocal-1.15 \
    AUTOCONF=autoconf-2.69 \
    AUTOHEADER=autoheader-2.69 \
    autoreconf-2.69 -fi
}

src_configure() {
    mkdir build
    cd build

    PKG_CONFIG_PATH="${KERNEL_SYSROOT}/lib/pkgconfig:${KERNEL_SYSROOT}/share/pkgconfig" \
    PKG_CONFIG_LIBDIR="${KERNEL_SYSROOT}/lib/pkgconfig:${KERNEL_SYSROOT}/share/pkgconfig" \
    CPPFLAGS="-I${KERNEL_SYSROOT}/include" \
    LDFLAGS="-L${KERNEL_SYSROOT}/lib" \
    LIBS="-lfts -largp" \
    CC=gcc \
    AR=ar \
    RANLIB=ranlib \
    ../configure \
        --prefix="${KERNEL_SYSROOT}" \
        --libdir="${KERNEL_SYSROOT}/lib" \
        --includedir="${KERNEL_SYSROOT}/include" \
        --disable-debuginfod \
        --disable-libdebuginfod
}

src_compile() {
    default_src_compile
}

src_install() {
    make "${MAKEJOBS}" install \
        DESTDIR="${DESTDIR}" \
        prefix="${KERNEL_SYSROOT}" \
        libdir="${KERNEL_SYSROOT}/lib" \
        includedir="${KERNEL_SYSROOT}/include"
}
