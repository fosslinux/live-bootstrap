# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2024 Gábor Stefanik <netrolller.3d@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

dirname=linux-4.14.336

src_unpack() {
    mkdir "linux-4.14.336"
    default || true # Predictable link errors - not a problem
    rm "${DISTFILES}/linux-4.14.336.tar.xz"
}

generate_autoconf_h() {
    # generate include/linux/autoconf.h -- we do not have gperf rn to do it the normal way
    mkdir -p include/generated
    # Transform each of the CONFIG_* options that are =y into header
    grep -E '=y$' .config | sed 's/=y$/ 1/' | sed 's/^/#define /' >> include/generated/autoconf.h
    # Transform each of the CONFIG_* options that are unset into headers
    grep -E ' is not set$' .config | sed 's/ is not set$//' | sed 's/#/#undef/' >> include/generated/autoconf.h
    # Transform each of the non-boolean options into headers
    grep -E '=.*$' .config | grep -v -E '=y$' | sed 's/=/ /' | sed 's/^/#define /' >> include/generated/autoconf.h
}

src_prepare() {
    default

    mv config .config
    mkdir -p include/config
    cp .config include/config/auto.conf

    generate_autoconf_h

    # Remove shipped files
    find . -name "*_shipped*" -delete

    # Remove documentation to save space
    rm -rf Documentation
}

src_compile() {
    cp .config include/config/auto.conf
    rm include/generated/autoconf.h
    generate_autoconf_h

    # Allow use of patched initramfs_list.sh (which is required anyway)
    make "${MAKEJOBS}" ARCH=i386 prepare

    # Build just the vmlinux, because a full build will not fit our ramdisk
    PATH="${PWD}/usr:${PATH}" make "${MAKEJOBS}" ARCH=i386 vmlinux

    # Clear up more space
    find . -name '*.o' -not -path './drivers/firmware/efi/libstub/*' -delete

    # Now that we have space, build bzImage, taking care not to rebuild what we've just deleted
    PATH="${PWD}/usr:${PATH}" make "${MAKEJOBS}" ARCH=i386 -o vmlinux bzImage

    # Clear up one more time
    find . -name '*.o' -delete
    rm vmlinux
}

src_install() {
    install -D -m 644 arch/i386/boot/bzImage "${DESTDIR}/boot/vmlinuz"
    install -D -m 755 usr/gen_init_cpio "${DESTDIR}${PREFIX}/bin/gen_init_cpio"
    install -D -m 755 scripts/gen_initramfs_list.sh "${DESTDIR}${PREFIX}/bin/gen_initramfs_list.sh"
}
