#!/usr/bin/bash

# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

# shellcheck source=sysa/helpers.sh
. helpers.sh

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
}

populate_device_nodes ""

create_fhs

build bash-5.1

exec env -i PATH="${PATH}" PREFIX="${PREFIX}" SOURCES="${SOURCES}" DESTDIR="${DESTDIR}" DISTFILES="${DISTFILES}" bash run2.sh
