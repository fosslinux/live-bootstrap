#!/bin/sh
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

. /steps/bootstrap.cfg
. /steps/env

if [ "${INTERACTIVE}" = True ]; then
    env - PATH=${PREFIX}/bin PS1="\w # " setsid openvt -fec1 -- bash -i
fi

if [ "${CHROOT}" = False ]; then
    # Ignore errors due to missing swap/fstab.
    swapoff -a >/dev/null 2>&1 || true
    sync
    echo u > /proc/sysrq-trigger
    mount -o remount,ro /
    echo o > /proc/sysrq-trigger
    while true; do sleep 1; done
fi
