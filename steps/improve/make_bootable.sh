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

setup_kernel_devices() {
    mount | grep ' on /dev ' &> /dev/null || (mkdir -p /dev; mount -t devtmpfs devtmpfs /dev)
    mount | grep ' on /proc ' &> /dev/null || (mkdir -p /proc; mount -t proc proc /proc)
    mount | grep ' on /sys ' &> /dev/null || (mkdir -p /sys; mount -t sysfs sysfs /sys)
    # Make /tmp a ramdisk (speeds up configure etc significantly)
    mount | grep ' on /tmp ' &> /dev/null || (mkdir -p /tmp; mount -t tmpfs tmpfs /tmp)
    mount | grep ' on /dev/pts ' &> /dev/null || (mkdir -p /dev/pts; mount -t devpts devpts /dev/pts)
    mount | grep ' on /dev/shm ' &> /dev/null || (mkdir -p /dev/shm; mount -t tmpfs tmpfs /dev/shm)

    test -c /dev/console || mknod -m 666 /dev/console c 5 1
    test -c /dev/tty || mknod -m 666 /dev/tty c 5 0
    test -c /dev/ptmx || mknod -m 666 /dev/ptmx c 5 2
    test -c /dev/tty0 || mknod -m 666 /dev/tty0 c 4 0
    test -c /dev/tty1 || mknod -m 666 /dev/tty1 c 4 1
    test -c /dev/tty2 || mknod -m 666 /dev/tty2 c 4 2
    test -c /dev/ttyS0 || mknod -m 666 /dev/ttyS0 c 4 64
}

verify_kernel_devices() {
    mount | grep ' on /dev ' &> /dev/null &&
    mount | grep ' on /proc ' &> /dev/null &&
    mount | grep ' on /sys ' &> /dev/null &&
    mount | grep ' on /tmp ' &> /dev/null &&
    mount | grep ' on /dev/pts ' &> /dev/null &&
    mount | grep ' on /dev/shm ' &> /dev/null &&
    test -c /dev/console &&
    test -c /dev/tty &&
    test -c /dev/ptmx &&
    test -c /dev/tty0 &&
    test -c /dev/tty1 &&
    test -c /dev/tty2 &&
    test -c /dev/ttyS0
}

setup_kernel_devices
verify_kernel_devices || {
    echo "Kernel device setup verification failed." >&2
    exit 1
}

if test -f /swapfile; then
    swapon /swapfile
fi

if [ "${CHROOT}" = False ]; then
    dhcpcd --waitip=4
fi
EOF

if [ "${BUILD_GUIX_ALSO}" = True ]; then
cat >> /init <<- 'EOF'
run_steps_guix_if_requested() {
    if [ "${BUILD_GUIX_ALSO}" != True ]; then
        return 1
    fi
    if [ ! -f /steps-guix/manifest ]; then
        echo "BUILD_GUIX_ALSO is True but /steps-guix/manifest is missing." >&2
        exit 1
    fi

    sed -i '/^BUILD_GUIX_ALSO=/d' /steps/bootstrap.cfg
    echo 'BUILD_GUIX_ALSO=False' >> /steps/bootstrap.cfg

    /script-generator /steps-guix/manifest /steps
    bash /steps-guix/0.sh
    return 0
}

run_steps_guix_if_requested || true
EOF
fi

cat >> /init <<- 'EOF'
if [ "${QEMU}" = True ] && [ "${BARE_METAL}" = False ]; then
    if [ -c /dev/ttyS0 ]; then
        env - PATH=${PREFIX}/bin PS1="\w # " bash -i </dev/ttyS0 >/dev/ttyS0 2>&1
    else
        env - PATH=${PREFIX}/bin PS1="\w # " bash -i </dev/console >/dev/console 2>&1
    fi
else
    env - PATH=${PREFIX}/bin PS1="\w # " setsid openvt -fec1 -- bash -i
fi

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
