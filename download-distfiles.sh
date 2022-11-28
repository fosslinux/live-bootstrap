#!/bin/sh

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
        curl -L "${url}" --output "${dest_path}"
    fi
    echo "${checksum}  ${dest_path}" | sha256sum -c
}

download_for_sys() {
    local sysdir="${1}"
    local distfiles="${sysdir}/distfiles"

    mkdir -p "${distfiles}"

    local entry
    for entry in "${sysdir}"/*; do
        [ -e "${entry}/sources" ] || continue

        local line
        while read line; do
            # This is intentional - we want to split out ${line} into separate arguments.
            download_source "${distfiles}" ${line}
        done < "${entry}/sources"
    done
}

set -e
cd "$(dirname "$(readlink -f "$0")")"
download_for_sys sysa
download_for_sys sysc
