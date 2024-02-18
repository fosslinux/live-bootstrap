#!/bin/sh

# SPDX-FileCopyrightText: 2024 Gábor Stefanik <netrolller.3d@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# find the physical disk name
PHYSICAL=${DISK}

# take care of e.g. "sda1"
if echo "${DISK}" | grep -Eq '^[a-z]{3}[0-9]+$'
then
    PHYSICAL=$(echo "${DISK}" | sed -E 's/^([a-z]{3})[0-9]+$/\1/')
fi

# take care of e.g. "mmcblk0p1"
if echo "${DISK}" | grep -Eq '^[a-z0-9]{3,}p[0-9]+$'
then
    PHYSICAL=$(echo "${DISK}" | sed -E 's/^([a-z0-9]{3,})p[0-9]+$/\1/')
fi

grub-install "/dev/${PHYSICAL}"

cat > /boot/grub/grub.cfg <<- EOF
set timeout=5
set default=0
menuentry 'Linux live-bootstrap' {
    insmod part_msdos
    set root='$(grub-probe -d /dev/${DISK} -t bios_hints | sed -e 's/ //g')'
    set gfxpayload=auto
    linux /boot/vmlinuz root=/dev/${DISK} rootwait rw $(cat /proc/cmdline)
}
EOF

cat > /init <<- 'EOF'
#!/usr/bin/bash
cd /steps
. ./bootstrap.cfg
. ./env
. ./helpers.sh
trap 'env PATH=${PREFIX}/bin PS1="[TRAP] \w # " bash -i' ERR
# /dev is automounted by the kernel at this point
mount | grep '/proc' &> /dev/null || (mkdir -p /proc; mount -t proc proc /proc)
mount | grep '/sys' &> /dev/null || (mkdir -p /sys; mount -t sysfs sysfs /sys)
# Make /tmp a ramdisk (speeds up configure etc significantly)
mount | grep '/tmp' &> /dev/null || (mkdir -p /tmp; mount -t tmpfs tmpfs /tmp)
mount | grep '/dev/shm' &> /dev/null || (mkdir -p /dev/shm; mount -t tmpfs tmpfs /dev/shm)

if test -f /swapfile; then
    swapon /swapfile
fi

if [ "${CHROOT}" = False ]; then
    dhcpcd --waitip=4
fi

env - PATH=${PREFIX}/bin PS1="\w # " setsid openvt -fec1 -- bash -i

# ignore errors due to fstab or swapfile not existing
swapoff -a &> /dev/null || true
sync
# sysrq to avoid device busy; then mount to wait for it to finish
echo u > /proc/sysrq-trigger
mount -o remount,ro /
echo o > /proc/sysrq-trigger # power off
while true; do sleep 1; done
EOF

chmod +x /init
