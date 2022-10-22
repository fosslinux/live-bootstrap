#!/bin/bash -e

# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
# SPDX-FileCopyrightText: 2021 Melg Eight <public.melg8@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# shellcheck source=/dev/null
. "${SOURCES}/bootstrap.cfg"

# Get a list of files
get_files() {
    local prefix
    prefix="${1}"
    fs=
    if [ -n "$(ls 2>/dev/null)" ]; then
        fs=$(echo ./*)
    fi
    if [ -n "$(ls .[0-z]* 2>/dev/null)" ]; then
        fs="${fs} $(echo .[0-z]*)"
    fi
    for f in ${fs}; do
        if [ -d "${f}" ]; then
            cd "${f}"
            get_files "${prefix}/${f}"
            cd ..
        else
            echo -n "${prefix}/${f} "
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
    cd "${SRCDIR}/repo"
    # Get revision (n time this package has been built)
    revision="$(echo "${pkg}"*)"
    # Different versions of bash
    if [ "${revision}" = "${pkg}*" ] || [ -z "${revision}" ]; then
        revision=0
    else
        revision="${revision##*_}"
        revision="${revision%%.*}"
        revision=$((++revision))
    fi
}

# Installs binary packages from an earlier run
# This is useful to speed up development cycle
bin_preseed() {
    if [ -d "${SRCDIR}/repo-preseeded" ]; then
        get_revision "${pkg}"
        cd "${SRCDIR}/repo-preseeded"
        if [ "${UPDATE_CHECKSUMS}" = "True" ] || src_checksum "${pkg}" $((revision)); then
            echo "${pkg}: installing prebuilt package."
            mv "${pkg}_${revision}"* ../repo || return 1
            if [[ "${pkg}" == bash-* ]]; then
                # tar does not like overwriting running bash
                # shellcheck disable=SC2153
                rm -f "${PREFIX}/bin/bash" "${PREFIX}/bin/sh"
            fi
            # shellcheck disable=SC2144
            if [ -f *-repodata ]; then
                mv -- *-repodata ../repo
            fi
            cd "${SRCDIR}/repo"
            rm -f /tmp/filelist.txt
            src_apply "${pkg}" $((revision))
            cd "${SOURCES}"
            return 0
        fi
    fi
    return 1
}

# Common build steps
# Build function provides a few common stages with default implementation
# that can be overridden on per package basis in the build script.
# build takes three arguments:
# 1) name-version of the package
# 2) optionally specify build script. Default is name-version.sh
# 3) directory of patches. Default is patches
# 4) directory to cd into. Default is ${pkg}
build() {
    pkg=$1
    script_name=${2:-${pkg}.sh}
    dirname=${4:-${pkg}}

    bin_preseed && return || true # Normal build if preseed fails

    cd "${SOURCES}/${pkg}" || (echo "Cannot cd into ${pkg}!"; kill $$)
    echo "${pkg}: beginning build using script ${script_name}"
    base_dir="${PWD}"
    patch_dir="${base_dir}/${3:-patches}"
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

    echo "${pkg}: creating package."
    get_revision "${pkg}"
    cd "${DESTDIR}"
    src_pkg

    src_checksum "${pkg}" "${revision}"

    echo "${pkg}: cleaning up."
    rm -rf "${SOURCES}/${pkg}/build"
    rm -rf "${DESTDIR}"
    mkdir -p "${DESTDIR}"

    echo "${pkg}: installing package."
    src_apply "${pkg}" "${revision}"

    echo "${pkg}: build successful"

    cd "${SOURCES}"

    unset -f src_unpack src_prepare src_configure src_compile src_install
}

interpret_source_line() {
    url="${1}"
    checksum="${2}"
    fname="${3}"
    # Default to basename of url if not given
    fname="${fname:-$(basename "${url}")}"
    if ! [ -e "${fname}" ]; then
        curl -L "${url}" --output "${fname}"
    fi
    echo "${checksum}  ${fname}" | sha256sum -c
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
    f="$(basename "${1}")"
    if test $# -gt 3; then
        shift 3
        extract="$*"
    else
        extract=
    fi
    # shellcheck disable=SC2154
    case "${noextract}" in
        *${f}*)
            cp "${DISTFILES}/${f}" .
            ;;
        *)
            case "${f}" in
                *.tar*)
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
        for p in "${patch_dir}"/*.patch; do
            echo "Applying patch: ${p}"
            patch -Np0 < "${p}"
        done
    fi

    makefile="${mk_dir}/main.mk"
    if test -e "${makefile}"; then
        cp "${makefile}" Makefile
    fi

    if test -d "${files_dir}"; then
        cp "${files_dir}"/* "${PWD}/"
    fi
}

# Default function for configuring source.
default_src_configure() {
    :
}

# Default function for compiling source. It simply runs make without any parameters.
default_src_compile() {
    make -f Makefile PREFIX="${PREFIX}"
}

# Default installing function. PREFIX should be set by run.sh script.
# Note that upstream makefiles might ignore PREFIX and have to be configured in configure stage.
default_src_install() {
    make -f Makefile install PREFIX="${PREFIX}" DESTDIR="${DESTDIR}"
}

create_tarball_pkg() {
    # If grep is unavailable, then tar --sort is unavailable.
    # So this does not need a command -v grep.
    tar_basename="${pkg}_${revision}.tar"
    dest_tar="/usr/src/repo/${tar_basename}"
    if command -v find >/dev/null 2>&1 && command -v sort >/dev/null 2>&1; then
        find . -print0 | LC_ALL=C sort -z > /tmp/filelist.txt
    fi
    cd /usr/src/repo

    if tar --help | grep ' \-\-sort' >/dev/null 2>&1; then
        tar -C "${DESTDIR}" --sort=name --hard-dereference \
            --numeric-owner --owner=0 --group=0 --mode=go=rX,u+rw,a-s -cf "${dest_tar}" .
    elif command -v find >/dev/null 2>&1 && command -v sort >/dev/null 2>&1; then
        cd "${DESTDIR}"
        tar --no-recursion --null -T /tmp/filelist.txt \
            --numeric-owner --owner=0 --group=0 --mode=go=rX,u+rw,a-s -cf "${dest_tar}"
        cd -
    else
        echo -n > /dev/null
        tar --numeric-owner --owner=0 --group=0 --mode=go=rX,u+rw,a-s \
            -cf "${dest_tar}" -T /dev/null
        cd "${DESTDIR}"
        for f in $(get_files .); do
            tar --numeric-owner --owner=0 --group=0 --mode=go=rX,u+rw,a-s \
                -rf "${dest_tar}" "${f}"
        done
        cd -
    fi
    touch -t 197001010000.00 "${tar_basename}"
    bzip2 --best "${tar_basename}"
}

src_pkg() {
    touch -t 197001010000.00 .
    reset_timestamp
    if command -v xbps-create >/dev/null 2>&1; then
        cd /usr/src/repo
        xbps-create -A "${ARCH}" -n "${pkg}_${revision}" -s "${pkg}" --compression xz "${DESTDIR}"
        echo "${pkg}: adding package to repository."
        xbps-rindex --compression xz -a "/usr/src/repo/${pkg}_${revision}.${ARCH}.xbps"
    else
        create_tarball_pkg
    fi
}

src_checksum() {
    local pkg=$1 revision=$2
    local rval=0
    if ! [ "$UPDATE_CHECKSUMS" = True ] ; then
        # We avoid using pipes as that is not supported by initial sha256sum from mescc-tools-extra
        local checksum_file=/tmp/checksum
        _grep "${pkg}_${revision}" "${SOURCES}/SHA256SUMS.pkgs" > "${checksum_file}"
        echo "${pkg}: checksumming created package."
        sha256sum -c "${checksum_file}" || rval=$?
        rm "${checksum_file}"
    fi
    return "${rval}"
}

src_apply() {
    local pkg="${1}" revision="${2}"
    if command -v xbps-install >/dev/null 2>&1; then
        xbps-install -y -R /usr/src/repo "${pkg%%-[0-9]*}"
    else
        src_apply_tar "${pkg}" "${revision}"
    fi
}

src_apply_tar() {
    local pkg=$1 revision=$2
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
    "${BZIP2_PREFIX}bzip2" -dc "/usr/src/repo/${pkg}_${revision}.tar.bz2" | \
        tar -C / -xpf -
    unset BZIP2_PREFIX
    rm -f "/tmp/bzip2"
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

# Set modified dates of all files to be 0 unix time.
# Should be called at the end of bootstrapping process.
# This function needs `touch` that supports --no-dereference
# (at least coreutils 8.1).
canonicalise_all_files_timestamp() {
    find / -exec touch --no-dereference -t 197001010000.00 {} +
}

populate_device_nodes() {
    # http://www.linuxfromscratch.org/lfs/view/6.1/chapter06/devices.html
    mkdir -p "/dev"
    test -c "/dev/null" || (rm -f "/dev/null" &&
                                mknod -m 666 "/dev/null" c 1 3)
    test -c "/dev/zero" || mknod -m 666 "/dev/zero" c 1 5
    test -c "/dev/random" || mknod -m 444 "/dev/random" c 1 8
    test -c "/dev/urandom" || mknod -m 444 "/dev/urandom" c 1 9

    if [ "${CHROOT}" = False ]; then
        test -c "/dev/ptmx" || mknod -m 666 "/dev/ptmx" c 5 2
        test -c "/dev/tty" || mknod -m 666 "/dev/tty" c 5 0
        test -c "/dev/console" || mknod -m 666 "/dev/console" c 5 1
    fi
}

sys_transfer() {
    local dest=$1
    local sys_sources=$2

    mkdir -p "${dest}/${PREFIX}/bin" "${dest}/${PREFIX}/src"

    # Bash, Tar and Bzip2 are required to install packages
    cp "${PREFIX}/bin/bash" "${PREFIX}/bin/tar" "${PREFIX}/bin/bzip2" "${dest}${PREFIX}/bin/"

    # Transfer misc files
    cp "${SOURCES}/helpers.sh" "${SOURCES}/SHA256SUMS.pkgs" "${SOURCES}/bootstrap.cfg" "${dest}/${PREFIX}/src"

    cp -r "${sys_sources}/"* "${dest}/${PREFIX}/src"
    cp -f "${sys_sources}/init" "${dest}/"
    cp -r "${PREFIX}/src/repo" "${dest}/${PREFIX}/src"
    if [ -e "${PREFIX}/src/repo-preseeded" ]; then
        cp -r "${PREFIX}/src/repo-preseeded" "${dest}/${PREFIX}/src"
    fi

    shift 2
    # Copy additional binaries
    set -- "${@/#/${PREFIX}/bin/}"
    cp "$@" "${dest}${PREFIX}/bin/"
}
