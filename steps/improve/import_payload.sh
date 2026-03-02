#!/bin/sh
#
# SPDX-FileCopyrightText: 2026 live-bootstrap contributors
# SPDX-License-Identifier: MIT

set -ex

if [ "${PAYLOAD_REQUIRED}" = True ]; then
    mkdir -p /external/distfiles
    found_payload=0

    if [ -r /proc/partitions ]; then
        while read -r major minor blocks name; do
            case "${name}" in
                ""|name|ram*|loop*|fd*|sr*|md*|dm-*|nbd*)
                    continue
                    ;;
                *[0-9])
                    continue
                    ;;
            esac
            dev_path="/dev/${name}"
            [ -b "${dev_path}" ] || mknod -m 600 "${dev_path}" b "${major}" "${minor}" >/dev/null 2>&1 || :
        done < /proc/partitions
    fi

    for dev_path in /dev/*; do
        [ -b "${dev_path}" ] || continue
        if payload-import --probe "${dev_path}"; then
            payload-import --device "${dev_path}" /external/distfiles
            found_payload=1
            break
        fi
    done

    if [ "${found_payload}" != 1 ]; then
        echo "payload-import failed: no payload image found on block devices under /dev." >&2
        exit 1
    fi
fi
