# SPDX-License-Identifier: GPL-3.0-or-later

: "${KERNEL_SYSROOT:=/kernel-toolchain}"
: "${KERNEL_TARGET:=x86_64-unknown-linux-musl}"

kernel_make() {
    make "${MAKEJOBS}" \
        PATH="/usr/bin:/bin" \
        HOSTCC="gcc" \
        HOSTCXX="g++" \
        HOSTLD="ld" \
        HOSTAR="ar" \
        HOSTCFLAGS="-I${KERNEL_SYSROOT}/include" \
        HOSTLDFLAGS="-L${KERNEL_SYSROOT}/lib" \
        LD_LIBRARY_PATH="${KERNEL_SYSROOT}/lib:${LD_LIBRARY_PATH}" \
        ARCH=x86_64 \
        CROSS_COMPILE="${KERNEL_SYSROOT}/bin/${KERNEL_TARGET}-" \
        "$@"
}

src_prepare() {
    default
}

src_configure() {
    cp config.txt .config
}

src_compile() {
    kernel_make
}

src_install() {
    install -D -m 644 arch/x86/boot/bzImage \
        "${DESTDIR}/boot/vmlinuz-linux64"
}
