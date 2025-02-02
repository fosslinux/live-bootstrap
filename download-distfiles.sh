#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# Optional arguments: Mirrors

download_source() {
    local distfiles=${1}
    local url=${2}
    shift 2
    if [[ ${url} == git://* ]]; then
        url=${1}
        shift
    fi
    local checksum=${1}
    local fname=${2}
    # Default to basename of url if not given
    fname=${fname:-$(basename "${url}")}
    if [ "${fname}" = "_" ]; then
        echo "ERROR: ${url} must have a filename specified"
        exit 1
    fi

    local dest_path=${distfiles}/${fname}
    if ! [ -e "${dest_path}" ]; then
        echo "Downloading ${fname}"
        if [ "${mirrors_len}" -ne 0 ]; then
            local mirror_ix=$((RANDOM % mirrors_len))
            url=${mirrors[${mirror_ix}]}/${fname}
        fi
        curl --fail --location "${url}" --output "${dest_path}" || true
    fi
}

check_source() {
    local distfiles=${1}
    local url=${2}
    shift 2
    if [[ ${url} == git://* ]]; then
        url=${1}
        shift
    fi
    local checksum=${1}
    local fname=${2}
    # Default to basename of url if not given
    fname=${fname:-$(basename "${url}")}

    local dest_path=${distfiles}/${fname}
    echo "${checksum}  ${dest_path}" | sha256sum -c
}

set -e

mirrors=( "$@" )
mirrors_len=$#

cd "$(dirname "$(readlink -f "$0")")"
mkdir -p distfiles

# First, try to download anything missing - ignore failing mirrors
for entry in steps/*; do
    [ -e "${entry}/sources" ] || continue

    # shellcheck disable=SC2162
    while read line; do
        # This is intentional - we want to split out ${line} into separate arguments.
        # shellcheck disable=SC2086
        download_source distfiles ${line}
    done < "${entry}/sources"
done

# Then, check if everything has been obtained at least once
for entry in steps/*; do
    [ -e "${entry}/sources" ] || continue

    # shellcheck disable=SC2162
    while read line; do
        # This is intentional - we want to split out ${line} into separate arguments.
        # shellcheck disable=SC2086
        check_source distfiles ${line}
    done < "${entry}/sources"
done
