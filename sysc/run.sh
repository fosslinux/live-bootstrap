#!/usr/bin/bash

# SPDX-FileCopyrightText: © 2023 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

# shellcheck source=/dev/null
. .env

trap bash EXIT

# shellcheck source=sysa/helpers.sh
. helpers.sh

umask 0022

create_fhs() {
    # Add the rest of the FHS that we will use and is not created pre-boot
    rm -rf /sbin /usr/sbin
    ln -s bin /usr/sbin # Hack to fix xbps (alternatively need to install which)
    for d in bin lib sbin; do
        ln -s "usr/${d}" "/${d}"
    done
    mkdir -p /etc /run /var
    test -d /proc || (mkdir /proc && mount -t proc proc /proc)
    test -d /sys || (mkdir /sys && mount -t sysfs sysfs /sys)
    # Make /tmp a ramdisk (speeds up configure etc significantly)
    test -d /tmp || (mkdir /tmp && mount -t tmpfs tmpfs /tmp)
    # Add /etc/resolv.conf
    echo 'nameserver 1.1.1.1' > /etc/resolv.conf
}

create_fhs

populate_device_nodes

# Make some swap
dd if=/dev/zero of=/swap bs=1M count=8192
mkswap /swap
swapon /swap

if [ -e "${SOURCES}/distfiles" ]; then
    mv "${SOURCES}/distfiles" /
else
    mkdir -p "${DISTFILES}"
fi

build curl-7.88.1 pass1.sh

# Obtain network connection
if [ "${CHROOT}" = "False" ]; then
    dhcpcd --waitip=4
    # Ensure network accessible
    timeout=120
    while ! curl example.com >/dev/null 2>&1; do
        sleep 1
        # shellcheck disable=SC2219
        let timeout--
        if [ "${timeout}" -le 0 ]; then
            echo "Timeout reached for internet to become accessible"
            false
        fi
    done
fi

build bash-5.2.15

exec env -i bash run2.sh
