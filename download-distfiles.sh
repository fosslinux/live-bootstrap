#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

download_source() {
    local distfiles="${1}"
    local url="${2}"
    local checksum="${3}"
    local fname="${4}"
    # Default to basename of url if not given
    fname="${fname:-$(basename "${url}")}"

    local dest_path="${distfiles}/${fname}"
    if ! [ -e "${dest_path}" ]; then
        echo "Downloading ${fname}"
        curl --fail --location "${url}" --output "${dest_path}"
    fi
    echo "${checksum}  ${dest_path}" | sha256sum -c
}

set -e

cd "$(dirname "$(readlink -f "$0")")"
mkdir -p distfiles

for entry in steps/*; do
    [ -e "${entry}/sources" ] || continue

    # shellcheck disable=SC2162
    while read line; do
        # This is intentional - we want to split out ${line} into separate arguments.
        # shellcheck disable=SC2086
        download_source distfiles ${line}
    done < "${entry}/sources"
done
