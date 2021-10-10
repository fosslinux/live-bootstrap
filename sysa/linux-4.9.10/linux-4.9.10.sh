# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_unpack() {
    default || true # Predictable link errors - not a problem

    cp ../src/deblob-4.9 ../src/deblob-check ${pkg}/ 

    # Clear up storage space
    rm -rf ../src
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

    # Deblob the kernel
    chmod +x deblob-4.9 deblob-check
    # Don't use gawk, use sed
    AWK=dosentexist ./deblob-4.9

    # Remove shipped files
    find . -name "*_shipped*" -delete
}

src_compile() {
    cp .config include/config/auto.conf
    rm include/generated/autoconf.h
    generate_autoconf_h

    make ARCH=i386 prepare
    make ARCH=i386
}

src_install() {
    mkdir -p "${PREFIX}/boot"
    cp arch/i386/boot/bzImage "${PREFIX}/boot/linux-4.9.10"
    cp usr/gen_init_cpio "${PREFIX}/bin"
    cp scripts/gen_initramfs_list.sh "${PREFIX}/bin"
}
