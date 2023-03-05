#!/bin/bash

# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

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

build automake-1.15.1

build binutils-2.30

# Build musl with fewer patches
build musl-1.1.24 binutils-rebuild.sh patches-pass3

# Rebuild tcc-musl using new musl
build tcc-0.9.27 tcc-musl-pass3.sh patches-musl-pass3

populate_device_nodes

build gcc-4.0.4 pass1.sh

build findutils-4.2.33

build musl-1.2.3

build linux-headers-5.10.41 '' '' linux-5.10.41

build gcc-4.0.4 pass2.sh

build util-linux-2.19.1

build e2fsprogs-1.45.7

build dhcpcd-9.4.1 '' '' dhcpcd-dhcpcd-9.4.1-1663155

build kbd-1.15

build make-3.82

build curl-7.83.0

build ed-1.4

build bc-1.07.1

# Clear up some RAM space
grep --no-filename '^build' "${SOURCES}"/run*.sh | sed "s/build //" | sed "s/ .*$//" | while read -r p ; do
    rm -rf "${SOURCES:?}/${p:?}"
done

if [ "${CHROOT}" = False ]; then
    build kexec-tools-2.0.22

    build linux-4.9.10

    create_sysb
    go_sysb
fi

# In chroot mode transition directly into System C.
SYSC=/sysc_image
sys_transfer "${SYSC}" /sysc gzip patch
if [ "${CHROOT_ONLY_SYSA}" != True ]; then
    exec chroot "${SYSC}" /init
fi
