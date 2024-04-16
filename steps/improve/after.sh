#!/bin/sh
#
# SPDX-FileCopyrightText: 2024 Gábor Stefanik <netrolller.3d@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later
#
# After bootstrap, drop to a shell if needed, then shut down cleanly.

. /steps/bootstrap.cfg
. /steps/env

if [ -d /steps/after ]; then
    find /steps/after -maxdepth 1 -name '*.sh' -exec bash {} \;
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
