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

extra_builds="${EXTRA_BUILDS:-}"

if [ -n "${extra_builds}" ]; then
    old_ifs="${IFS}"
    IFS=','
    for extra_build in ${extra_builds}; do
        [ -n "${extra_build}" ] || continue
        extra_manifest="/steps-${extra_build}/manifest"
        if [ ! -f "${extra_manifest}" ]; then
            echo "EXTRA_BUILDS includes '${extra_build}' but ${extra_manifest} is missing." >&2
            IFS="${old_ifs}"
            exit 1
        fi
        /script-generator "${extra_manifest}" /steps
        bash "/steps-${extra_build}/0.sh"
    done
    IFS="${old_ifs}"
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
