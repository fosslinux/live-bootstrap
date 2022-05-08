#!/bin/bash -e

# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
# SPDX-FileCopyrightText: 2021 Melg Eight <public.melg8@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# shellcheck source=/dev/null
. bootstrap.cfg

# Find a list of links
get_links() {
    original="${1:-${PWD}/}"
    if [ -n "$(ls)" ]; then
        for f in *; do
            # Bash link detection is broken
            # and we don't always have grep, hence this pretty dodgy solution
            case "$(ls -ld "${f}")" in
            *" -> "*)
                fullpath="${PWD}/${f}"
                # shellcheck disable=SC2001
                echo "$(readlink "${f}") /$(echo "${fullpath}" | sed "s:^${original}::")"
                rm "${f}" # Don't include in tarball
                ;;
            *)
                if [ -d "${f}" ]; then
                    cd "${f}"
                    get_links "${original}"
                    cd ..
                fi
                ;;
            esac
        done
    fi
}

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
    args=
    # touch -h is not avaliable until after grep is built.
    if command -v grep >/dev/null 2>&1; then
        if touch --help | grep ' \-h' >/dev/null; then
            args="-h"
        fi
    fi
    if command -v find >/dev/null 2>&1; then
        # find does not error out on exec error
        find . -print0 | xargs -0 touch ${args} -t 197001010000.00
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
            touch ${args} -t 197001010000.00 "${f}"
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
    text="${1}"
    fname="${2}"
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

    cd /usr/src/repo
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

    echo "${pkg}: creating package."
    cd "${DESTDIR}"
    src_pkg

    src_checksum

    echo "${pkg}: cleaning up."
    rm -rf "${SOURCES}/${pkg}/build"
    rm -rf "${DESTDIR}"
    mkdir -p "${DESTDIR}"

    echo "${pkg}: installing package."
    src_apply

    echo "${pkg}: build successful"

    cd "${SOURCES}"

    unset -f src_unpack src_prepare src_configure src_compile src_install
}

# Default unpacking function that unpacks all source tarballs.
default_src_unpack() {
    distfiles=${EXTRA_DISTFILES}
    # shellcheck disable=SC2153
    for f in "${DISTFILES}/${pkg}."*; do
        distfiles="$(basename "$f") ${distfiles}"
    done

    # Check for new tar
    # shellcheck disable=SC2153
    if test -e "${PREFIX}/libexec/rmt"; then
        for i in ${distfiles}; do
            tar --no-same-owner -xf "${DISTFILES}/${i}"
        done
    else
        for i in ${distfiles}; do
            case "$i" in
            *.tar.gz) tar -xzf "${DISTFILES}/${i}" ;;
            *.tar.bz2)
                # Initial bzip2 built against meslibc has broken pipes
                bzip2 -dc "${DISTFILES}/${i}" | tar -xf - ;;
            *.tar.xz)
                tar -xf "${DISTFILES}/${i}" --use-compress-program=xz ;;
            esac
        done
    fi
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
    if tar --help | grep ' \-\-sort' >/dev/null 2>&1; then
        tar -C "${DESTDIR}" --sort=name --hard-dereference -cf "/usr/src/repo/${pkg}_${revision}.tar" .
    elif command -v find >/dev/null 2>&1 && command -v sort >/dev/null 2>&1; then
        cd "${DESTDIR}"
        tar --no-recursion --null -T /tmp/filelist.txt -cf "/usr/src/repo/${pkg}_${revision}.tar"
        cd -
    else
        echo -n > /dev/null
        tar -cf "/usr/src/repo/${pkg}_${revision}.tar" -T /dev/null
        cd "${DESTDIR}"
        for f in $(get_files .); do
            tar -rf "/usr/src/repo/${pkg}_${revision}.tar" "${f}"
        done
        cd -
    fi
    touch -t 197001010000.00 "${pkg}_${revision}.tar"
    bzip2 --best "${pkg}_${revision}.tar"
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
        cd "${DESTDIR}"
        # All symlinks are dereferenced, which is BAD
        get_links > "/usr/src/repo/${pkg}_${revision}.links"
        if command -v find >/dev/null 2>&1 && command -v sort >/dev/null 2>&1; then
            find . -print0 | LC_ALL=C sort -z > /tmp/filelist.txt
        fi
        cd /usr/src/repo
        create_tarball_pkg
    fi
}

src_checksum() {
    if ! [ "$UPDATE_CHECKSUMS" = True ] ; then
        echo "${pkg}: checksumming created package."
        _grep "${pkg}_${revision}" "${SOURCES}/SHA256SUMS.pkgs" | sha256sum -c
    fi
}

src_apply() {
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
    bzip2 -dc "/usr/src/repo/${pkg}_${revision}.tar.bz2" | \
        tar -C / -xpf -
    # shellcheck disable=SC2162
    # ^ read -r unsupported in old bash
    while read line; do
        # shellcheck disable=SC2001
        # ^ cannot use variable expansion here
        fname="$(echo "${line}" | sed 's/.* //')"
        rm -f "${fname}"
        # shellcheck disable=SC2226,SC2086
        # ^ ${line} expands into two arguments
        ln -s ${line}
        touch -t 197001010000.00 "${fname}"
    done < "/usr/src/repo/${pkg}_${revision}.links"
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
    mkdir -p "${1}/dev"
    rm "${1}/dev/null" -f
    test -c "${1}/dev/null" || mknod -m 666 "${1}/dev/null" c 1 3
    test -c "${1}/dev/zero" || mknod -m 666 "${1}/dev/zero" c 1 5
    test -c "${1}/dev/random" || mknod -m 444 "${1}/dev/random" c 1 8
    test -c "${1}/dev/urandom" || mknod -m 444 "${1}/dev/urandom" c 1 9

    if [ "${CHROOT}" = False ]; then
        test -c "${1}/dev/ptmx" || mknod -m 666 "${1}/dev/ptmx" c 5 2
        test -c "${1}/dev/tty" || mknod -m 666 "${1}/dev/tty" c 5 0
        test -c "${1}/dev/console" || mknod -m 666 "${1}/dev/console" c 5 1
    fi
}

sys_transfer() {
    local dest=$1

    mkdir -p "${dest}/${PREFIX}/bin" "${dest}/${PREFIX}/src"

    # Bash, Tar and Bzip2 are required to install packages
    cp "${PREFIX}/bin/bash" "${PREFIX}/bin/tar" "${PREFIX}/bin/bzip2" "${dest}${PREFIX}/bin/"

    # Transfer misc files
    cp "${SOURCES}/helpers.sh" "${SOURCES}/SHA256SUMS.pkgs" "${SOURCES}/bootstrap.cfg" "${dest}/"

    cp -r "${PREFIX}/src/" "${dest}${PREFIX}/"

    shift
    # Copy additional binaries
    set -- "${@/#/${PREFIX}/bin/}"
    cp "$@" "${dest}${PREFIX}/bin/"
}
