#!/bin/sh
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

. /steps/bootstrap.cfg
. /steps/env

guix_localstate_dir="/var/guix"
daemon_socket="${guix_localstate_dir}/daemon-socket/socket"
out_dir="/external/guix-images"
export GUIX_DAEMON_SOCKET="${daemon_socket}"

if [ ! -S "${daemon_socket}" ]; then
    echo "guix-daemon socket is missing: ${daemon_socket}" >&2
    echo "Run improve/guix-daemon-and-pull.sh first." >&2
    exit 1
fi

mkdir -p "${out_dir}"

cp /var/guix/profiles/per-user/root/current-guix/manifest /usr/manifest

iso_store_path="$(guix system image \
    --system=x86_64-linux \
    -t iso9660 \
    -c "${JOBS}" \
    -e '(@@ (gnu system install) installation-os)' \
    --no-substitutes)"

if [ ! -e "${iso_store_path}" ]; then
    echo "guix system image did not return a valid path: ${iso_store_path}" >&2
    exit 1
fi

ln -sfn "${iso_store_path}" "${out_dir}/guix-system-install-x86_64.iso"
