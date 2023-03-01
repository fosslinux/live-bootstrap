#!/bin/bash

# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

# shellcheck disable=SC2154
PREFIX="${prefix}"
LIBDIR="${PREFIX}/lib/mes"
# shellcheck disable=SC2154
SOURCES="${sysa}"
DISTFILES="${sysa}/distfiles"
DESTDIR=/tmp/destdir
# shellcheck disable=SC2154
SRCDIR="${srcdir}"

# shellcheck source=sysa/helpers.sh
. helpers.sh

create_sysb() {
    # Copy everything in
    echo "Creating sysb rootfs"
    sys_transfer /sysb_image /sysb gzip patch
    cp -rl /sysc /sysb_image/sysc_src
    echo "Creating sysb initramfs"
    gen_initramfs_list.sh -o "${PREFIX}/boot/initramfs-sysb.cpio.gz" /sysb_image
    rm -rf /sysb /sysb_image # Cleanup
}

go_sysb() {
    # Mount proc for kexec
    mkdir /proc /etc
    mount -t proc proc /proc
    # kexec time
    echo "Loading kernel + sysb initramfs using kexec"
    kexec -l "${PREFIX}/boot/linux-4.9.10" --console-serial \
        --initrd="${PREFIX}/boot/initramfs-sysb.cpio.gz" \
        --append="init=/init console=ttyS0"
    echo "kexecing into sysb"
    kexec -e
}

# Ask some questions
echo
echo "Now that bash has been built, there are potentially some questions for you!"
echo "To give your answer, type your answer, press Enter, and then Control-D."
echo

ask_chroot() {
    read -r CHROOT_STRING
    if [ "${CHROOT_STRING}" = "yes" ] || [ "${CHROOT_STRING}" = "y" ]; then
        CHROOT=True
    elif [ "${CHROOT_STRING}" = "no" ] || [ "${CHROOT_STRING}" = "n" ]; then
        CHROOT=False
    else
        echo "Invalid response. Please give a yes/no answer."
        ask_chroot
    fi
}

if [ -z "${CHROOT}" ]; then
    echo "Currently, it is unknown if live-bootstrap is running in a chroot"
    echo "or not. Is it? (yes/no answer)"
    ask_chroot
    echo

    echo "CHROOT=${CHROOT}" >> "${SOURCES}/bootstrap.cfg"
fi

ask_timestamps() {
    read -r TIMESTAMPS_STRING
    if [ "${TIMESTAMPS_STRING}" = "yes" ] || [ "${TIMESTAMPS_STRING}" = "y" ]; then
        FORCE_TIMESTAMPS=True
    elif [ "${TIMESTAMPS_STRING}" = "no" ] || [ "${TIMESTAMPS_STRING}" = "n" ]; then
        FORCE_TIMESTAMPS=False
    else
        echo "Invalid response. Please give a yes/no answer."
        ask_timestamps
    fi
}

if [ -z "${FORCE_TIMESTAMPS}" ]; then
    echo "Would you like all timestamps to be set to unix time 0"
    echo "(Jan 1 1970 00:00) at the end of the bootstrap? This makes a"
    echo "fully reproducible disk image. (yes/no answer)"
    ask_timestamps
    echo

    echo "FORCE_TIMESTAMPS=${FORCE_TIMESTAMPS}" >> "${SOURCES}/bootstrap.cfg"
fi

echo "Thank you! All done."

echo "ARCH=${ARCH}" >> "${SOURCES}/bootstrap.cfg"

mkdir -p "${DESTDIR}" "${SRCDIR}/repo" /dev

build flex-2.5.11

# Rebuild tcc with some patches
build tcc-0.9.27 tcc-mes-pass2.sh

# shellcheck disable=SC2034
LIBDIR="${PREFIX}/lib/i386-unknown-linux-musl"

build musl-1.1.24

# Rebuild tcc using musl
build tcc-0.9.27 tcc-musl-pass1.sh

# Rebuild musl using tcc-musl
build musl-1.1.24

# Rebuild tcc-musl using new musl
build tcc-0.9.27 tcc-musl-pass2.sh

# Rebuild sed using musl
build sed-4.0.9 sed-4.0.9.sh

# Rebuild bzip2 using musl
build bzip2-1.0.8 bzip2-1.0.8.sh

build m4-1.4.7

build flex-2.6.4

build bison-3.4.1 stage1.sh
build bison-3.4.1 stage2.sh
build bison-3.4.1 stage3.sh

build grep-2.4

build diffutils-2.7

# Rebuild coreutils using musl
build coreutils-5.0 coreutils-5.0.sh patches-musl

# Build only date, mktemp and sha256sum
build coreutils-6.10

build gawk-3.0.4

build perl-5.000

build perl-5.003

build perl5.004-05 '' '' perl5.004_05

build perl5.005-03 '' '' perl5.005_03

build perl-5.6.2

populate_device_nodes

build autoconf-2.52

build automake-1.6.3 stage1.sh
build automake-1.6.3 stage2.sh

build autoconf-2.53

build automake-1.7

build autoconf-2.54

build autoconf-2.55

build automake-1.7.8

build autoconf-2.57

build autoconf-2.59

build automake-1.8.5

build help2man-1.36.4

build autoconf-2.61

build automake-1.9.6

build automake-1.10.3

build autoconf-2.64

build automake-1.11.2

build autoconf-2.69

build libtool-2.2.4

build binutils-2.24

# Build musl with fewer patches
build musl-1.1.24 binutils-rebuild.sh patches-pass3

# Rebuild tcc-musl using new musl
build tcc-0.9.27 tcc-musl-pass3.sh patches-musl-pass3

populate_device_nodes

build gcc-4.0.4 pass1.sh

build findutils-4.2.33

build musl-1.2.3

# This hack fixes a strange bug in mes libc bash
set -x
build linux-headers-5.10.41 '' '' linux-5.10.41
set +x

build gcc-4.0.4 pass2.sh

build util-linux-2.19.1

build e2fsprogs-1.45.7

build dhcpcd-9.4.1 '' '' dhcpcd-dhcpcd-9.4.1-1663155

build kbd-1.15

build make-3.82

if [ "${CHROOT}" = False ]; then
    # Save because linux deletes all distfiles to save space
    cp "${DISTFILES}"/musl-1.2.3.tar.gz "${SOURCES}"/musl-1.2.3
fi

mkdir -p /sysc/distfiles
cp "${DISTFILES}"/curl-7.83.0.tar.bz2 /sysc/distfiles

# Clear up some RAM space
grep '^build' "${SOURCES}/run.sh" | grep -v musl-1.2.3 | sed "s/build //" | sed "s/ .*$//" | while read -r p ; do
    rm -rf "${SOURCES:?}/${p:?}"
done

if [ "${CHROOT}" = False ]; then
    build kexec-tools-2.0.22

    build linux-4.9.10

    build musl-1.2.3 '' no-patches

    create_sysb
    if [ "${KERNEL_BOOTSTRAP}" = False ]; then
        go_sysb
    fi
else
    build musl-1.2.3 '' no-patches

    # In chroot mode transition directly into System C.
    SYSC=/sysc_image
    sys_transfer "${SYSC}" /sysc gzip patch
    if [ "${CHROOT_ONLY_SYSA}" != True ]; then
        exec chroot "${SYSC}" /init
    fi
fi
