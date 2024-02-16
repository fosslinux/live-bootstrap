# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_unpack() {
    mkdir "${pkg}"
    default || true # Predictable link errors - not a problem
    rm "${DISTFILES}/${pkg}.tar.xz"
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
    PATH="${PWD}/usr:${PATH}" make "${MAKEJOBS}" ARCH=i386

    # Clear up more space
    find . -name '*.o' -delete
}

src_install() {
    install -D -m 644 arch/i386/boot/bzImage "${DESTDIR}/boot/vmlinuz"
    install -D -m 755 usr/gen_init_cpio "${DESTDIR}${PREFIX}/bin/gen_init_cpio"
    install -D -m 755 scripts/gen_initramfs_list.sh "${DESTDIR}${PREFIX}/bin/gen_initramfs_list.sh"
}
