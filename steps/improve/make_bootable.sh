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

shutdown_system() {
    status="${1:-0}"
    echo "init: shutting down (status=${status})" >&2
    # ignore errors due to fstab or swapfile not existing
    swapoff -a &> /dev/null || true
    sync
    # sysrq to avoid device busy; then mount to wait for it to finish
    echo u > /proc/sysrq-trigger
    mount -o remount,ro / || true
    echo o > /proc/sysrq-trigger # power off
    while true; do sleep 1; done
}

on_error() {
    status=$?
    trap - ERR
    echo "init: error detected, aborting bootstrap path." >&2
    shutdown_system "${status}"
}

trap 'on_error' ERR

setup_kernel_devices() {
    mount | grep ' on /dev ' &> /dev/null || (mkdir -p /dev; mount -t devtmpfs devtmpfs /dev)
    mount | grep ' on /proc ' &> /dev/null || (mkdir -p /proc; mount -t proc proc /proc)
    mount | grep ' on /sys ' &> /dev/null || (mkdir -p /sys; mount -t sysfs sysfs /sys)
    # Make /tmp a ramdisk (speeds up configure etc significantly)
    mount | grep ' on /tmp ' &> /dev/null || (mkdir -p /tmp; mount -t tmpfs tmpfs /tmp)
    if ! mount | grep ' on /dev/pts ' &> /dev/null; then
        mkdir -p /dev/pts
        mount -t devpts devpts /dev/pts
    fi
    mount | grep ' on /dev/shm ' &> /dev/null || (mkdir -p /dev/shm; mount -t tmpfs tmpfs /dev/shm)

    test -c /dev/console || mknod -m 666 /dev/console c 5 1
    test -c /dev/tty || mknod -m 666 /dev/tty c 5 0
    test -c /dev/ptmx || mknod -m 666 /dev/ptmx c 5 2
    test -c /dev/tty0 || mknod -m 666 /dev/tty0 c 4 0
    test -c /dev/tty1 || mknod -m 666 /dev/tty1 c 4 1
    test -c /dev/tty2 || mknod -m 666 /dev/tty2 c 4 2
    test -c /dev/ttyS0 || mknod -m 666 /dev/ttyS0 c 4 64
}

have_tty_device() {
    for dev in /dev/tty /dev/tty[0-9]* /dev/ttyS*; do
        if [ -c "${dev}" ]; then
            return 0
        fi
    done
    return 1
}

verify_kernel_devices() {
    mount | grep ' on /dev ' &> /dev/null &&
    mount | grep ' on /proc ' &> /dev/null &&
    mount | grep ' on /sys ' &> /dev/null &&
    mount | grep ' on /tmp ' &> /dev/null &&
    mount | grep ' on /dev/pts ' &> /dev/null &&
    mount | grep ' on /dev/shm ' &> /dev/null &&
    test -c /dev/console &&
    test -c /dev/ptmx &&
    test -c /dev/pts/ptmx &&
    have_tty_device
}

setup_kernel_devices
verify_kernel_devices || {
    echo "Kernel device setup verification failed." >&2
    shutdown_system 1
}

if test -f /swapfile; then
    swapon /swapfile
fi

if [ "${CHROOT}" = False ]; then
    dhcpcd --waitip=4
fi
EOF

cat >> /init <<- 'EOF'
run_extra_builds_if_requested() {
    extra_builds="${EXTRA_BUILDS:-}"
    if [ -z "${extra_builds}" ]; then
        return 0
    fi

    old_ifs="${IFS}"
    IFS=','
    for extra_build in ${extra_builds}; do
        [ -n "${extra_build}" ] || continue
        extra_manifest="/steps-${extra_build}/manifest"
        if [ ! -f "${extra_manifest}" ]; then
            echo "EXTRA_BUILDS includes '${extra_build}' but ${extra_manifest} is missing." >&2
            IFS="${old_ifs}"
            return 1
        fi
        /script-generator "${extra_manifest}" /steps
        bash "/steps-${extra_build}/0.sh" || {
            IFS="${old_ifs}"
            return 1
        }
    done
    IFS="${old_ifs}"
    return 0
}

run_extra_builds_if_requested || shutdown_system $?
EOF

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

shutdown_system 0
EOF

chmod +x /init
