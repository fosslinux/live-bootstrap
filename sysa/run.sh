#!/bin/bash

# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -e
# shellcheck source=sysglobal/helpers.sh
. helpers.sh

export PREFIX=${prefix}
export SOURCES=${sources}
export DESTDIR="/tmp/destdir"

create_sysb() {
    # Copy everything in
    echo "Creating sysb rootfs"
    mkdir -p /sysb/usr
    for d in bin include lib libexec share; do
        # Minimise RAM (storage) use - use hard links
        cp -rl "${PREFIX}/${d}" "/sysb/usr/${d}"
    done
    cp "${SOURCES}/bootstrap.cfg" /sysb/usr/src/bootstrap.cfg
    cp -rl "/usr/src/repo" /sysb/usr/src/repo
    populate_device_nodes /sysb
    echo "Creating sysb initramfs"
    gen_initramfs_list.sh -o "${PREFIX}/boot/initramfs-sysb.cpio.gz" /sysb
    rm -rf /sysb # Cleanup
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
fi

echo "Thank you! All done."

# Write to bootstrap.cfg
rm "${SOURCES}/bootstrap.cfg"
for var in CHROOT FORCE_TIMESTAMPS DISK ARCH UPDATE_CHECKSUMS; do
    echo "export ${var}=${!var}" >> "${SOURCES}/bootstrap.cfg"
done

mkdir -p /tmp/destdir /usr/src/repo /dev

build flex-2.5.11

# Patch meslibc to support > 255 command line arguments
build mes-0.23 mes-libc-0.23.sh

build tcc-0.9.27 tcc-meslibc-rebuild.sh checksums/tcc-meslibc-rebuild

build musl-1.1.24 "" checksums/pass1

# Rebuild tcc using musl
build tcc-0.9.27 tcc-musl-pass1.sh checksums/tcc-musl-pass1

# Rebuild musl using tcc-musl
build musl-1.1.24 "" checksums/pass2

# Rebuild tcc-musl using new musl
build tcc-0.9.27 tcc-musl-pass2.sh checksums/tcc-musl-pass2

# Rebuild sed using musl
build sed-4.0.9 sed-4.0.9.sh checksums/pass2

# Rebuild bzip2 using musl
build bzip2-1.0.8 bzip2-1.0.8.sh checksums/bzip2-pass2

build m4-1.4.7

build flex-2.6.4

build bison-3.4.1 stage1.sh checksums/stage1
build bison-3.4.1 stage2.sh checksums/stage2
build bison-3.4.1 stage3.sh checksums/stage3

build grep-2.4

build diffutils-2.7

# Rebuild coreutils using musl
build coreutils-5.0 coreutils-5.0.sh checksums/pass2

# Build only date, mktemp and sha256sum
build coreutils-6.10

build gawk-3.0.4

build perl-5.000

build perl-5.003

build perl5.004-05 '' '' '' perl5.004_05

build perl5.005-03 '' '' '' perl5.005_03

build perl-5.6.2

populate_device_nodes

build autoconf-2.52 stage1.sh

build automake-1.6.3 stage1.sh
build automake-1.6.3 stage2.sh
build automake-1.6.3 stage3.sh

build automake-1.4-p6

build autoconf-2.52 stage2.sh

build autoconf-2.13

build autoconf-2.12

build libtool-1.4

build binutils-2.14

# Build musl with fewer patches
build musl-1.1.24 binutils-rebuild.sh checksums/pass3 patches-pass3
populate_device_nodes

# Rebuild tcc-musl using new musl
build tcc-0.9.27 tcc-musl-pass3.sh checksums/tcc-musl-pass3 patches-musl-pass3

build autoconf-2.53 stage1.sh
build autoconf-2.53 stage2.sh

build automake-1.7 stage1.sh

build autoconf-2.54 stage1.sh
build autoconf-2.54 stage2.sh

build automake-1.7 stage2.sh

build autoconf-2.55

build automake-1.7.8

build autoconf-2.57

build autoconf-2.59

build automake-1.8.5

build help2man-1.36.4

build autoconf-2.61 stage1.sh
build autoconf-2.61 stage2.sh

build automake-1.9.6 stage1.sh
build automake-1.9.6 stage2.sh

build findutils-4.2.33

build libtool-2.2.4

build automake-1.10.3

build autoconf-2.64

build gcc-4.0.4 pass1.sh checksums/pass1

# This hack fixes a strange bug in mes libc bash
set -x
build linux-headers-5.10.41 '' '' '' linux-5.10.41
set +x

build musl-1.2.2

build gcc-4.0.4 pass2.sh checksums/pass2

build util-linux-2.19.1

build kbd-1.15

build make-3.82

# Clear up some RAM space
grep '^pkg=' /after.kaem | sed 's/pkg="//' | sed 's/"$//' | while read -r p ; do
    rm -rf "${SOURCES:?}/${p:?}"
done
rm -rf "${SOURCES}/mes"

grep '^build' "${SOURCES}/run.sh" | sed "s/build //" | sed "s/ .*$//" | while read -r p ; do
    rm -rf "${SOURCES:?}/${p:?}"
done

if [ "${CHROOT}" = False ]; then
    build kexec-tools-2.0.22

    build linux-4.9.10

    create_sysb
    go_sysb
fi

# In chroot mode transition directly into System C.
SYSC="/sysc"
cp -R "${PREFIX}" "${SYSC}"
cp "${SOURCES}/bootstrap.cfg" "${SYSC}/usr/src/"
exec chroot "${SYSC}" /init
