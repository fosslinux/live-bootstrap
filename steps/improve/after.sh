#!/bin/sh
#
# SPDX-FileCopyrightText: 2024 Gábor Stefanik <netrolller.3d@gmail.com>
# SPDX-FileCopyrightText: 2025 Dor Askayo <dor.askayo@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later
#
# After bootstrap, drop to a shell if needed, then shut down cleanly.

. /steps/bootstrap.cfg
. /steps/env

if [ -d /steps/after ]; then
    after_scripts=$(find /steps/after -maxdepth 1 -type f -name '*.sh' -printf '%f\t%p\n' | sort -k1 -n | cut -f2)
    for script in $after_scripts; do
        bash "$script"
    done
fi

if [ "${BUILD_GUIX_ALSO}" = True ]; then
    if [ ! -f /steps-guix/manifest ]; then
        echo "BUILD_GUIX_ALSO is True but /steps-guix/manifest is missing." >&2
        exit 1
    fi

    sed -i '/^BUILD_GUIX_ALSO=/d' /steps/bootstrap.cfg
    echo 'BUILD_GUIX_ALSO=False' >> /steps/bootstrap.cfg

    /script-generator /steps-guix/manifest /steps
    kaem --file /steps-guix/0.sh
fi

if [ "${INTERACTIVE}" = True ]; then
    env - PATH=${PREFIX}/bin PS1="\w # " setsid openvt -fec1 -- bash -i
fi

if [ "${CHROOT}" = False ]; then
    # ignore errors due to fstab or swapfile not existing
    swapoff -a &> /dev/null || true
    sync
    # sysrq to avoid device busy; then mount to wait for it to finish
    echo u > /proc/sysrq-trigger
    mount -o remount,ro /
    echo o > /proc/sysrq-trigger # power off
    while true; do sleep 1; done
fi
