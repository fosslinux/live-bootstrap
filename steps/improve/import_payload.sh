#!/bin/sh
#
# SPDX-FileCopyrightText: 2026 live-bootstrap contributors
# SPDX-License-Identifier: MIT

set -ex

if [ "${PAYLOAD_REQUIRED}" = True ]; then
    mkdir -p /external/distfiles
    mkdir -p /proc
    mount -t proc proc /proc >/dev/null 2>&1 || :

    if [ ! -r /proc/partitions ]; then
        echo "payload-import failed: /proc/partitions is unavailable." >&2
        exit 1
    fi

    found_payload=0
    while read -r major minor blocks name; do
        case "${name}" in
            ""|name|ram*|loop*|fd*|sr*|md*|dm-*|nbd*)
                continue
                ;;
            *[0-9])
                # Skip partitions (sda1, vdb2, ...); payload is attached as a whole disk.
                continue
                ;;
        esac

        dev_path="/dev/${name}"
        mknod -m 600 "${dev_path}" b "${major}" "${minor}" >/dev/null 2>&1 || :

        if payload-import --probe "${dev_path}"; then
            payload-import --device "${dev_path}" /external/distfiles
            found_payload=1
            break
        fi
    done < /proc/partitions

    if [ "${found_payload}" != 1 ]; then
        echo "payload-import failed: no payload image found on probed block devices." >&2
        exit 1
    fi
fi
