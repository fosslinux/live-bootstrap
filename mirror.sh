#!/bin/sh
# This script is intentionally written in POSIX sh to be cross-platform.
#
# SPDX-License-Identifier: GPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 fosslinux <fosslinux@aussies.space>

set -e

# Check arguments, etc
if [ "$#" -lt 1 ] || [ "$#" -gt 2 ]; then
    echo "Usage: $0 <destination> [state]"
    exit 1
fi

dest=$1
if [ ! -d "${dest}" ]; then
    echo "${dest} must be a directory"
    exit 1
fi

if [ ! -w "${dest}" ]; then
    echo "you must be able to write to ${dest}"
    exit 1
fi
dest=$(realpath "${dest}")

state=$2
if [ "${state}" = "" ]; then
    state="${PWD}/mirrorstate"
fi
state="$(realpath "${state}")"

#######

# Download a HTTP file
download_file() {
    url=${1}
    out=${2}
    # Download the file
    continue_arg=""
    if [ -e "${out}" ]; then
        continue_arg="-C -"
    fi
    # shellcheck disable=SC2086
    curl -L ${continue_arg} -o "${out}" "${url}"
}

# Check if a git reference exists in a given repository
git_ref_exists() {
    repo=${1}
    ref=${2}
    # change this to git show-ref once it is sufficiently not-new
    ( cd "${repo}" || exit && git cat-file -t "${ref}" >/dev/null 2>&1 )
    return $?
}

checksum_file() {
    sha256sum "${1}" | cut -d' ' -f1
}

do_file() {
    uri=${1}
    echo "${uri}"

    if echo "${uri}" | grep -qE "^https?://"; then
        # HTTP file
        checksum=${2}
        filename=${3}
        if [ "${filename}" = "" ]; then
            filename=$(basename "${uri}")
        fi

        # Check if the file is already downloaded & the checksum is the same
        dest_file=${dest}/${filename}
        if [ -e "${dest_file}" ]; then
            existing_checksum=$(checksum_file "${dest_file}")
            if [ "${checksum}" = "${existing_checksum}" ]; then
                # There is nothing we need to do here
                return
            fi
        fi

        # Attempt up to 3 times
        retries=3
        matching=no
        while [ "${retries}" -gt 0 ]; do
            download_file "${uri}" "${dest}/${filename}"
            my_checksum=$(checksum_file "${dest_file}")
            if [ "${checksum}" = "${my_checksum}" ]; then
                matching="yes"
                break
            fi
            retries=$((retries - 1))
            if [ "${retries}" -gt 0 ]; then
                echo "${uri}: checksum did not match, trying again"
                rm "${dest}/${filename}"
            fi
            sleep 1
        done

        if [ "${matching}" = "no" ]; then
            echo "${uri}: checksums do not match"
            exit 1
        fi
    elif echo "${uri}" | grep -qE "^git://"; then
        # Creating a tarball from a git repository

        # Very unfortunately, different sites have different rules.
        uri_path=${uri#git://}
        # GitHub does not have git:// protocol support
        if echo "${uri}" | grep -Eq "^git://github.com"; then
            uri=https://${uri_path}
        fi
        repo=${uri%~*}
        outdir=${state}/git/${repo#*://}
        reference=${uri##*~}

        http_src=${2}
        checksum=${3}
        tarball=${4:-$(basename "${http_src}")}
        if [ "${tarball}" = "_" ]; then
            echo "${uri}: ERROR! Must have tarball name if no http source."
            exit 1
        fi
        tarball=${dest}/${tarball}

        # Check if tarball already generated + matches checksum
        checksum=${3}
        if [ -e "${tarball}" ]; then
            existing_checksum=$(checksum_file "${tarball}")
            if [ "${existing_checksum}" = "${checksum}" ]; then
                return
            fi
            rm "${tarball}"
        fi

        # Clone the repository, or update it if needed
        if [ ! -e "${outdir}" ]; then
            mkdir -p "$(dirname "${outdir}")"
            git clone "${repo}" "${outdir}"
        elif ! git_ref_exists "${outdir}" "${reference}" ; then
            (
                cd "${outdir}" || exit
                git pull
            )
        fi

        # Sanity check: the reference we want exists
        if ! git_ref_exists "${outdir}" "${reference}"; then
            echo "${reference} not found in ${repo} (${outdir})"
            exit 1
        fi

        # Generate the prefix for the tarball
        prefix_ref=${reference}
        # All git repositories we already use remove "v"s from the beginning
        # of branch/tag names in the tarball prefix
        if echo "${reference}" | grep -Eq "^v[0-9]"; then
            prefix_ref=$(echo "${reference}" | sed "s/^v//")
        fi
        prefix=$(basename "${repo}" | sed "s/.git$//")-${prefix_ref}

        (
            cd "${outdir}" || exit
            # Some versions of git by default use the internal gzip
            # implementation, others use external gzip; standardize this
            # -n is used for older versions of git that record gzip creation
            # date/time
            git config tar.tar.gz.command "gzip -n"
            # -T1 avoids non-determinism due to threading
            # This may not be correct for forges other than Savannah
            git config tar.tar.xz.command "xz -T1"
            git archive "${reference}" -o "${tarball}" --prefix "${prefix}/"
        )

        my_checksum=$(sha256sum "${tarball}" | cut -d' ' -f1)
        if [ "${my_checksum}" != "${checksum}" ]; then
            echo "${uri}: generated tarball does not match checksum"
            exit 1
        fi
    fi
}

for src in steps/*/sources; do
    while read -r line; do
        # shellcheck disable=SC2086
        do_file ${line}
        uri=$(echo "${line}" | cut -d' ' -f1)
    done < "${src}"
done
