#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# Optional arguments: Mirrors

download_source() {
    local distfiles=${1}
    local url=${2}
    shift 2
    case $url in
        'git://'*)
            url=${1}
            shift
        ;;
    esac
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
            local mirror_ix=$(( RANDOM % mirrors_len + 1 ))
            eval "url=\"\${mirror_$mirror_ix}/${fname}\""
        fi
        curl --fail --location "${url}" --output "${dest_path}" || true
    fi
}

check_source() {
    local distfiles=${1}
    local url=${2}
    shift 2
    case $url in
        'git://'*)
            url=${1}
            shift
        ;;
    esac
    local checksum=${1}
    local fname=${2}
    # Default to basename of url if not given
    fname=${fname:-$(basename "${url}")}

    local dest_path=${distfiles}/${fname}
    echo "${checksum}  ${dest_path}" | sha256sum -c
}

walk_manifest_sources () {
    local action=$1
    local manifest_entry=
    local entry=

    while read -r manifest_entry || test -n "$manifest_entry"; do
        case $manifest_entry in
            'build:'*)
                entry="${manifest_entry#'build: '}"
                entry="${entry%% *}" # remove anything after step name
                [ -e "./steps/${entry}/sources" ] || continue
                while read -r line; do
                    # This is intentional - we want to split out ${line} into separate arguments.
                    # shellcheck disable=SC2086
                    $action distfiles ${line}
                done < "./steps/${entry}/sources"
                ;;
        esac
    done < "./steps/manifest"
}

set -e

mirrors_len=$#
while test $# -gt 0; do
    eval "mirror_$#=$1"
    shift
done

cd "$(dirname "$(readlink -f "$0")")"
mkdir -p distfiles

# First, try to download anything missing - ignore failing mirrors
walk_manifest_sources download_source
# Then, check if everything has been obtained at least once
walk_manifest_sources check_source

