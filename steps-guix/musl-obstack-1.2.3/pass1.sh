# SPDX-License-Identifier: GPL-3.0-or-later

# Build musl-obstack for the kernel toolchain sysroot.
: "${KERNEL_SYSROOT:=/kernel-toolchain}"

src_prepare() {
    default
    autoreconf -fi
}

src_configure() {
    mkdir build
    cd build

    CC=gcc \
    AR=ar \
    RANLIB=ranlib \
    ../configure \
        --prefix="${KERNEL_SYSROOT}" \
        --libdir="${KERNEL_SYSROOT}/lib" \
        --includedir="${KERNEL_SYSROOT}/include"
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
