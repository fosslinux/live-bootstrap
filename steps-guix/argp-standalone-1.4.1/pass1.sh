# SPDX-License-Identifier: GPL-3.0-or-later

# Build argp-standalone for the kernel toolchain sysroot. This is used by
# later kernel-side dependencies (for example elfutils).
: "${KERNEL_TARGET:=x86_64-unknown-linux-musl}"
: "${KERNEL_SYSROOT:=/kernel-toolchain}"

src_prepare() {
    default
    autoreconf -fi
}

src_configure() {
    export PATH="${KERNEL_SYSROOT}/bin:${PATH}"

    mkdir build
    cd build

    ../configure \
        --prefix="${KERNEL_SYSROOT}" \
        --libdir="${KERNEL_SYSROOT}/lib" \
        --build="${TARGET}" \
        --host="${KERNEL_TARGET}" \
        --target="${KERNEL_TARGET}"
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
