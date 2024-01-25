#!/bin/bash -e

# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
# SPDX-FileCopyrightText: 2021 Melg Eight <public.melg8@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# Get a list of files
get_files() {
    echo "."
    _get_files "${1}"
}

_get_files() {
    local prefix
    prefix="${1}"
    fs=
    if [ -n "$(ls 2>/dev/null)" ]; then
        # This can be removed once Debian 12 is stable
        # shellcheck disable=SC2035
        fs=$(echo *)
    fi
    if [ -n "$(ls .[0-z]* 2>/dev/null)" ]; then
        fs="${fs} $(echo .[0-z]*)"
    fi
    for f in ${fs}; do
        # Archive symlinks to directories as symlinks
        echo "${prefix}/${f}"
        if [ -d "${f}" ] && ! [ -h "${f}" ]; then
            cd "${f}"
            _get_files "${prefix}/${f}"
            cd ..
        fi
    done
}

# Reset all timestamps to unix time 0
reset_timestamp() {
    if command -v find >/dev/null 2>&1; then
        # find does not error out on exec error
        find . -print0 | xargs -0 touch -h -t 197001010000.00
    else
        # A rudimentary find implementation that does the trick
        fs=
        if [ -n "$(ls 2>/dev/null)" ]; then
            fs=$(echo ./*)
        fi
        if [ -n "$(ls .[0-z]* 2>/dev/null)" ]; then
            fs="${fs} $(echo .[0-z]*)"
        fi
        for f in ${fs}; do
            touch -h -t 197001010000.00 "${f}"
            if [ -d "${f}" ]; then
                cd "${f}"
                reset_timestamp
                cd ..
            fi
        done
    fi
}

# Fake grep
_grep() {
    local text="${1}"
    local fname="${2}"
    if command -v grep >/dev/null 2>&1; then
        grep "${text}" "${fname}"
    else
        # shellcheck disable=SC2162
        while read line; do
            case "${line}" in *"${text}"*)
                echo "${line}" ;;
            esac
        done < "${fname}"
    fi
}

get_revision() {
    local pkg=$1
    cd "/external/repo"
    # Get revision (n time this package has been built)
    revision=$( (ls -1 "${pkg}"* 2>/dev/null || true) | wc -l | sed 's/ *//g')
}

# Installs binary packages from an earlier run
# This is useful to speed up development cycle
bin_preseed() {
    if [ -d "/external/repo-preseeded" ]; then
        get_revision "${pkg}"
        cd "/external/repo-preseeded"
        if [ "${UPDATE_CHECKSUMS}" = "True" ] || src_checksum "${pkg}" $((revision)); then
            echo "${pkg}: installing prebuilt package."
            mv "${pkg}_${revision}"* /external/repo || return 1
            if [[ "${pkg}" == bash-* ]]; then
                # tar does not like overwriting running bash
                # shellcheck disable=SC2153
                rm -f "${PREFIX}/bin/bash" "${PREFIX}/bin/sh"
            fi
            cd "/external/repo"
            rm -f /tmp/filelist.txt
            src_apply "${pkg}" $((revision))
            cd "${SRCDIR}"
            return 0
        fi
    fi
    return 1
}

# Common build steps
# Build function provides a few common stages with default implementation
# that can be overridden on per package basis in the build script.
# build takes two arguments:
# 1) name-version of the package
# 2) optionally specify build script. Default is pass$((revision+1)).sh
# 3) optionally specify directory to cd into
build() {
    pkg=$1
    get_revision "${pkg}"
    script_name=${2:-pass$((revision+1)).sh}
    dirname=${3:-${pkg}}

    # shellcheck disable=SC2015
    bin_preseed && return || true # Normal build if preseed fails

    cd "${SRCDIR}/${pkg}" || (echo "Cannot cd into ${pkg}!"; kill $$)
    echo "${pkg}: beginning build using script ${script_name}"
    base_dir="${PWD}"
    if [ -e "${base_dir}/patches-$(basename "${script_name}" .sh)" ]; then
        patch_dir="${base_dir}/patches-$(basename "${script_name}" .sh)"
    else
        patch_dir="${base_dir}/patches"
    fi
    mk_dir="${base_dir}/mk"
    files_dir="${base_dir}/files"

    rm -rf "build"
    mkdir "build"
    cd "build"

    build_script="${base_dir}/${script_name}"
    if test -e "${build_script}"; then
        # shellcheck source=/dev/null
        . "${build_script}"
    fi

    echo "${pkg}: getting sources."
    build_stage=src_get
    call $build_stage

    echo "${pkg}: unpacking source."
    build_stage=src_unpack
    call $build_stage
    unset EXTRA_DISTFILES

    cd "${dirname}" || (echo "Cannot cd into build/${dirname}!"; kill $$)

    echo "${pkg}: preparing source."
    build_stage=src_prepare
    call $build_stage

    echo "${pkg}: configuring source."
    build_stage=src_configure
    call $build_stage

    echo "${pkg}: compiling source."
    build_stage=src_compile
    call $build_stage

    echo "${pkg}: install to fakeroot."
    build_stage=src_install
    call $build_stage

    echo "${pkg}: postprocess binaries."
    build_stage=src_postprocess
    call $build_stage

    echo "${pkg}: creating package."
    cd "${DESTDIR}"
    src_pkg

    src_checksum "${pkg}" "${revision}"

    echo "${pkg}: cleaning up."
    rm -rf "${SRCDIR}/${pkg}/build"
    rm -rf "${DESTDIR}"
    mkdir -p "${DESTDIR}"

    echo "${pkg}: installing package."
    src_apply "${pkg}" "${revision}"

    echo "${pkg}: build successful"

    cd "${SRCDIR}"

    unset -f src_unpack src_prepare src_configure src_compile src_install src_postprocess
    unset extract
}

interpret_source_line() {
    url="${1}"
    checksum="${2}"
    fname="${3}"
    # Default to basename of url if not given
    fname="${fname:-$(basename "${url}")}"
    if ! [ -e "${fname}" ]; then
        curl --fail --retry 5 --location "${url}" --output "${fname}"
    fi
    echo "${checksum}  ${fname}" > "${fname}.sum"
    sha256sum -c "${fname}.sum"
    rm "${fname}.sum"
}

# Default get function that downloads source tarballs.
default_src_get() {
    # shellcheck disable=SC2153
    cd "${DISTFILES}"
    # shellcheck disable=SC2162
    while read line; do
        # This is intentional - we want to split out ${line} into separate arguments.
        # shellcheck disable=SC2086
        interpret_source_line ${line}
    done < "${base_dir}/sources"
    cd -
}

# Intelligently extracts a file based upon its filetype.
extract_file() {
    f="${3:-$(basename "${1}")}"
    # shellcheck disable=SC2154
    case "${noextract}" in
        *${f}*)
            cp "${DISTFILES}/${f}" .
            ;;
        *)
            case "${f}" in
                *.tar* | *.tgz)
                    if test -e "${PREFIX}/libexec/rmt"; then
                        # Again, we want to split out into words.
                        # shellcheck disable=SC2086
                        tar --no-same-owner -xf "${DISTFILES}/${f}" ${extract}
                    else
                        # shellcheck disable=SC2086
                        case "${f}" in
                        *.tar.gz) tar -xzf "${DISTFILES}/${f}" ${extract} ;;
                        *.tar.bz2)
                            # Initial bzip2 built against meslibc has broken pipes
                            bzip2 -dc "${DISTFILES}/${f}" | tar -xf - ${extract} ;;
                        *.tar.xz)
                            tar -xf "${DISTFILES}/${f}" --use-compress-program=xz ${extract} ;;
                        esac
                    fi
                    ;;
                *)
                    cp "${DISTFILES}/${f}" .
                    ;;
            esac
            ;;
    esac
}

# Default unpacking function that unpacks all sources.
default_src_unpack() {
    # Handle the first one differently
    first_line=$(head -n 1 ../sources)
    # Again, we want to split out into words.
    # shellcheck disable=SC2086
    extract_file ${first_line}
    # This assumes there is only one directory in the tarball
    # Get the dirname "smartly"
    if ! [ -e "${dirname}" ]; then
        for i in *; do
            if [ -d "${i}" ]; then
                dirname="${i}"
                break
            fi
        done
    fi
    # shellcheck disable=SC2162
    tail -n +2 ../sources | while read line; do
        # shellcheck disable=SC2086
        extract_file ${line}
    done
}

# Default function to prepare source code.
# It applies all patches from patch_dir (at the moment only -p0 patches are supported).
# Then it copies our custom makefile and any other custom files from files directory.
default_src_prepare() {
    if test -d "${patch_dir}"; then
        if ls "${patch_dir}"/*.patch >/dev/null 2>&1; then
            for p in "${patch_dir}"/*.patch; do
                echo "Applying patch: ${p}"
                patch -Np0 < "${p}"
            done
        fi
    fi

    makefile="${mk_dir}/main.mk"
    if test -e "${makefile}"; then
        cp "${makefile}" Makefile
    fi

    if test -d "${files_dir}"; then
        cp --no-preserve=mode "${files_dir}"/* "${PWD}/"
    fi
}

# Default function for configuring source.
default_src_configure() {
    :
}

# Default function for compiling source. It simply runs make without any parameters.
default_src_compile() {
    make "${MAKEJOBS}" -f Makefile PREFIX="${PREFIX}"
}

# Default installing function. PREFIX should be set by run.sh script.
# Note that upstream makefiles might ignore PREFIX and have to be configured in configure stage.
default_src_install() {
    make -f Makefile install PREFIX="${PREFIX}" DESTDIR="${DESTDIR}"
}

# Default function for postprocessing binaries.
default_src_postprocess() {
    if (command -v find && command -v file && command -v strip) >/dev/null 2>&1; then
        # Logic largely taken from void linux 06-strip-and-debug-pkgs.sh
        # shellcheck disable=SC2162
        find "${DESTDIR}" -type f | while read f; do
            case "$(file -bi "${f}")" in
                application/x-executable*) strip "${f}" ;;
                application/x-sharedlib*|application/x-pie-executable*)
                    machine_set="$(file -b "${f}")"
                    case "${machine_set}" in
                        *no\ machine*) ;; # don't strip ELF container-only
                        *) strip --strip-unneeded "${f}" ;;
                    esac
                    ;;
                application/x-archive*) strip --strip-debug "${f}" ;;
            esac
        done
    fi
}

src_pkg() {
    touch -t 197001010000.00 .
    reset_timestamp

    local tar_basename="${pkg}_${revision}.tar"
    local dest_tar="/external/repo/${tar_basename}"
    local filelist=/tmp/filelist.txt

    cd /external/repo
    # If grep is unavailable, then tar --sort is unavailable.
    # So this does not need a command -v grep.
    if tar --help | grep ' \-\-sort' >/dev/null 2>&1; then
        tar -C "${DESTDIR}" --sort=name --hard-dereference \
            --numeric-owner --owner=0 --group=0 --mode=go=rX,u+rw,a-s -cf "${dest_tar}" .
    else
        local olddir
        olddir=$PWD
        cd "${DESTDIR}"
        local null
        if command -v find >/dev/null 2>&1 && command -v sort >/dev/null 2>&1; then
            find . -print0 | LC_ALL=C sort -z > "${filelist}"
            null="--null"
        elif command -v sort >/dev/null 2>&1; then
            get_files .  | LC_ALL=C sort > "${filelist}"
        else
            get_files . > ${filelist}
        fi
        tar --no-recursion ${null} --files-from "${filelist}" \
                --numeric-owner --owner=0 --group=0 --mode=go=rX,u+rw,a-s -cf "${dest_tar}"
        rm -f "$filelist"
        cd "$olddir"
    fi
    touch -t 197001010000.00 "${tar_basename}"
    bzip2 --best "${tar_basename}"
}

src_checksum() {
    local pkg=$1 revision=$2
    local rval=0
    if ! [ "$UPDATE_CHECKSUMS" = True ] ; then
        # We avoid using pipes as that is not supported by initial sha256sum from mescc-tools-extra
        local checksum_file=/tmp/checksum
        _grep "${pkg}_${revision}" "${SRCDIR}/SHA256SUMS.pkgs" > "${checksum_file}" || true
        # Check there is something in checksum_file
        if ! [ -s "${checksum_file}" ]; then
            echo "${pkg}: no checksum stored!"
            false
        fi
        echo "${pkg}: checksumming created package."
        sha256sum -c "${checksum_file}" || rval=$?
        rm "${checksum_file}"
    fi
    return "${rval}"
}

src_apply() {
    local pkg="${1}" revision="${2}"
    local TAR_PREFIX BZIP2_PREFIX

    # Make sure we have at least one copy of tar
    if [[ "${pkg}" == tar-* ]]; then
        mkdir -p /tmp
        cp "${PREFIX}/bin/tar" "/tmp/tar"
        TAR_PREFIX="/tmp/"
    fi

    # Overwriting files is mega busted, so do it manually
    # shellcheck disable=SC2162
    if [ -e /tmp/filelist.txt ]; then
        while IFS= read -d $'\0' file; do
            rm -f "/${file}" >/dev/null 2>&1 || true
        done < /tmp/filelist.txt
    fi

    # Bzip2 does not like to be overwritten
    if [[ "${pkg}" == bzip2-* ]]; then
        mkdir -p /tmp
        mv "${PREFIX}/bin/bzip2" "/tmp/bzip2"
        BZIP2_PREFIX="/tmp/"
    fi
    "${BZIP2_PREFIX}bzip2" -dc "/external/repo/${pkg}_${revision}.tar.bz2" | \
        "${TAR_PREFIX}tar" -C / -xpf -
    rm -f "/tmp/bzip2" "/tmp/tar"
}

# Check if bash function exists
fn_exists() {
    test "$(type -t "$1")" == 'function'
}

# Call package specific function or default implementation.
call() {
    if fn_exists "$1"; then
        $1
    else
        default_"${1}"
    fi
}

# Call default build stage function
default() {
    "default_${build_stage}"
}
