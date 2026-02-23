# SPDX-License-Identifier: GPL-3.0-or-later

: "${KERNEL_SYSROOT:=/kernel-toolchain}"
: "${KERNEL_TARGET:=x86_64-unknown-linux-musl}"

kernel_env() {
    # Keep host tools on the host toolchain; target tools come from CROSS_COMPILE.
    export PATH="/usr/bin:/bin"
    export HOSTCC="gcc"
    export HOSTCXX="g++"
    export HOSTLD="ld"
    export HOSTAR="ar"
    export HOSTCFLAGS="-I${KERNEL_SYSROOT}/include"
    export HOSTLDFLAGS="-L${KERNEL_SYSROOT}/lib"
    export LD_LIBRARY_PATH="${KERNEL_SYSROOT}/lib:${LD_LIBRARY_PATH}"
}

kernel_make() {
    kernel_env
    make "${MAKEJOBS}" \
        ARCH=x86_64 \
        CROSS_COMPILE="${KERNEL_SYSROOT}/bin/${KERNEL_TARGET}-" \
        "$@"
}

src_prepare() {
    default
}

src_configure() {
    kernel_make defconfig
}

src_compile() {
    kernel_make
}

src_install() {
    install -D -m 644 arch/x86/boot/bzImage \
        "${DESTDIR}/boot/vmlinuz-linux64"
}
