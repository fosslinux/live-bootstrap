# SPDX-License-Identifier: GPL-3.0-or-later

# Build argp-standalone for the kernel toolchain sysroot. This is used by
# later kernel-side dependencies (for example elfutils).
: "${KERNEL_SYSROOT:=/kernel-toolchain}"

src_prepare() {
    default
    autoreconf -fi
}

src_configure() {
    mkdir build
    cd build

    # argp-standalone's testsuite expects argp.h from source root even when
    # building out-of-tree.
    CPPFLAGS="-I${PWD}/.." \
    CC=gcc \
    AR=ar \
    RANLIB=ranlib \
    ../configure \
        --prefix="${KERNEL_SYSROOT}" \
        --libdir="${KERNEL_SYSROOT}/lib"
}

src_compile() {
    default_src_compile
}

src_install() {
    make "${MAKEJOBS}" install \
        DESTDIR="${DESTDIR}" \
        prefix="${KERNEL_SYSROOT}" \
        libdir="${KERNEL_SYSROOT}/lib"
}
