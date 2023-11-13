#!/bin/bash

# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-License-Identifier: MIT

# Replace this hook if you wish to do more
# Add Dev Nodes 
# Disk Nodes
# 3 disks 3 partitions
major=0
alpha="a b c" 
# For each disk...
for a in ${alpha}; do
        mknod -m 600 "/dev/sd${a}" b 8 "$((major*16))"
        #For each partition do...
	minor=1 
        for p in $(seq 3); do
            mknod -m 600 "/dev/sd${a}${p}" b 8 "$((major*16+minor++))"
        done
	((major++))
done
# NVME Nodes
# 3 NVME disk with 3 partitions
major=0 
# For each disk...
for a in $(seq 3); do
	mknod -m 600 "/dev/nvme${a}" c 259 0 # NVME CHAR node
        mknod -m 600 "/dev/nvme${a}n1" b 259 "$((major))"
	((major++))
        # For each partition...
        for q in $(seq 3); do
            mknod -m 600 "/dev/nvme${a}n1p${q}" b 259 "$((major++))"
        done
done
# add cd-rom drive
mknod -m 600 /dev/sr0 b 11 0
. /usr/src/.env

mkdir /sysa
pushd /sysa

tar xvjf ../sysa.tar.bz2
cp -r /sysa/boot /boot

cat > /sbin/init << 'INIT_END'
#!/usr/bin/bash

# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

PREFIX=/usr
LIBDIR="${PREFIX}/lib/i386-unknown-linux-musl"
SOURCES="${PREFIX}/src"
DESTDIR=/tmp/destdir
DISTFILES=/distfiles
SRCDIR="${SOURCES}"

export PATH="${PREFIX}/bin:${PREFIX}/sbin"
export HOME=/tmp
export SOURCE_DATE_EPOCH=0

# shellcheck source=sysa/helpers.sh
. "${SOURCES}/helpers.sh"

MAKEJOBS="-j${JOBS}"

echo
echo "Installing packages into sysc"

install_tar() {
    echo "${1}: installing package"
    src_apply "$@"
}

# Begin sysc bootstrapping process
cd "${SOURCES}"
exec env -i bash rerun.sh
INIT_END

chmod +x /sbin/init

cat > "${SOURCES}/rerun.sh" << 'RERUN_END'
#!/usr/bin/bash

# SPDX-FileCopyrightText: © 2023 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

# shellcheck source=/dev/null
. .env

trap 'env - PATH=${PREFIX}/bin PS1="\w # " bash -i' EXIT

# shellcheck source=sysa/helpers.sh
. "${SOURCES}/helpers.sh"

umask 0022

create_fhs() {
    # Add the rest of the FHS that we will use and is not created pre-boot
    mkdir -p /etc /run /var
    mkdir -p /proc && mount -t proc proc /proc
    mkdir -p /sys && mount -t sysfs sysfs /sys
    # Make /tmp a ramdisk (speeds up configure etc significantly)
    mkdir -p /tmp && mount -t tmpfs tmpfs /tmp
}

create_fhs

# Activate our previously made some swap
swapon /swap

# Obtain network connection
dhcpcd --waitip=4

cd /root
env - PATH="${PREFIX}/bin" PS1="\w # " HOME="/root" bash --login -i
reboot
RERUN_END

chmod +x "${SOURCES}/rerun.sh"

popd

# TODO turn this into a proper build step
mkdir -p /usr/src/grub
pushd /usr/src/grub

curl -O https://ftp.gnu.org/gnu/grub/grub-2.06.tar.xz
tar xf grub-2.06.tar.xz
cd grub-2.06
autoreconf
CFLAGS=-Wno-error ./configure --prefix="${PREFIX}"
make ${MAKEJOBS}
make install
grub-install /dev/sda

cat > /boot/grub/grub.cfg << 'GRUB_END'
# Config for GNU GRand Unified Bootloader (GRUB)
# /boot/grub/grub.cfg

# Timeout for menu
set timeout=5

# Default boot entry
set default=0

# Menu Colours
set menu_color_normal=white/black
set menu_color_highlight=white/green

menuentry 'Linux live-bootstrap (4.9.10, serial console)' {
	insmod part_msdos
	set root='hd0,msdos1'
	linux /boot/linux-4.9.10 root=/dev/sda1 rw console=ttyS0
}

menuentry 'Linux live-bootstrap (4.9.10, graphical console)' {
	vbeinfo
	set gfxpayload=keep
	set gfxmode=1024x768x32
	terminal_output gfxterm
	insmod part_msdos
	set root='hd0,msdos1'
	linux /boot/linux-4.9.10 root=/dev/sda1 rw console=tty1
}

menuentry "Shutdown" {
    halt
}

menuentry "Reboot" {
    reboot
}
GRUB_END

popd

cat > /sbin/reboot << 'REBOOT_END'
echo 1 > /proc/sys/kernel/sysrq
echo s > /proc/sysrq-trigger
sleep 1
echo u > /proc/sysrq-trigger
sleep 1
echo b > /proc/sysrq-trigger
REBOOT_END

chmod +x /sbin/reboot

mkdir /root
cat > /root/.profile << 'PROFILE'
. /usr/src/.env
. "${SOURCES}/helpers.sh"
PROFILE

sync

cd "/root"
env - PATH="${PREFIX}/bin" PS1="\w # " HOME="/root" bash --login -i
reboot
