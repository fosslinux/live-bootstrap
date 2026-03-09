#!/bin/bash
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

new_kernel="/boot/vmlinuz-linux64"
current_kernel="/boot/vmlinuz"
backup_kernel="/boot/vmlinuz-32bit.backup"

if [ ! -f "${new_kernel}" ]; then
    echo "Missing new kernel image: ${new_kernel}" >&2
    exit 1
fi

if [ -f "${current_kernel}" ]; then
    cp -af "${current_kernel}" "${backup_kernel}"
fi
cp -af "${new_kernel}" "${current_kernel}"
sync

mkdir -p /etc
if [ "${BARE_METAL}" = True ]; then
    kexec -l "${current_kernel}" \
        --append="root=/dev/sda1 init=/init rw rootwait"
else
    kexec -l "${current_kernel}" --console-serial \
        --append="console=ttyS0 root=/dev/sda1 init=/init rw rootwait"
fi
sync
echo u > /proc/sysrq-trigger || true
mount -o remount,ro / || true
kexec -e
