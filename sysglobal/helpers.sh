#!/bin/bash -e

# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
# SPDX-FileCopyrightText: 2021 Melg Eight <public.melg8@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

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

# Reset all timestamps to unix time 0
reset_timestamp() {
    args=
    if touch --help | grep ' \-h' >/dev/null; then
        args="-h"
    fi
    if command -v find 2>&1 >/dev/null; then
        # find does not error out on exec error
        find . -print0 | xargs -0 touch ${args} -t 197001010000.00
    else
        # A rudimentary find implementation that does the trick
        fs=
        if [ -n "$(ls)" ]; then
            fs=$(echo *)
        fi
        if [ -n "$(ls .[0-z]*)" ]; then
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

# Common build steps
# Build function provides a few common stages with default implementation
# that can be overridden on per package basis in the build script.
# build takes three arguments:
# 1) name-version of the package
# 2) optionally specify build script. Default is name-version.sh
# 3) optionally specify name of checksum file. Default is checksums
# 4) directory of patches. Default is patches
# 5) directory to cd into. Default is ${pkg}
build() {
    pkg=$1
    script_name=${2:-${pkg}.sh}
    dirname=${5:-${pkg}}

    cd "${SOURCES}/${pkg}" || (echo "Cannot cd into ${pkg}!"; kill $$)
    echo "${pkg}: beginning build using script ${script_name}"
    base_dir="${PWD}"
    patch_dir="${base_dir}/${4:-patches}"
    mk_dir="${base_dir}/mk"
    files_dir="${base_dir}/files"

    mkdir -p "build"
    cd "build"

    build_script="${base_dir}/${script_name}"
    if test -e "${build_script}"; then
        # shellcheck source=/dev/null
        . "${build_script}"
    fi

    echo "${pkg}: unpacking source."
    build_stage=src_unpack
    call $build_stage

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
    # Various shenanigans must be implemented for repoducibility
    # as a result of timestamps
    cd "${DESTDIR}"
    touch -t 197001010000.00 .
    reset_timestamp
    cd /usr/src/repo
    if command -v xbps-create >/dev/null 2>&1; then
        xbps-create -A "${ARCH}" -n "${pkg}_${revision}" -s "${pkg}" --compression xz "${DESTDIR}"
    else
        # All symlinks are dereferenced, which is BAD
        cd "${DESTDIR}"
        get_links > "/usr/src/repo/${pkg}_${revision}.links"
        if command -v find >/dev/null 2>&1 && command -v sort >/dev/null 2>&1; then
            find -print0 | LC_ALL=C sort -z > /tmp/filelist.txt
        fi
        cd /usr/src/repo
        if tar --help | grep ' \-\-sort' >/dev/null 2>&1; then
            tar -C "${DESTDIR}" --sort=name --hard-dereference -cf "/usr/src/repo/${pkg}_${revision}.tar" .
        elif command -v find >/dev/null 2>&1 && command -v sort >/dev/null 2>&1; then
            cd "${DESTDIR}"
            tar --no-recursion --null -T /tmp/filelist.txt -cf "/usr/src/repo/${pkg}_${revision}.tar"
            cd -
        else
            tar -C "${DESTDIR}" -cf "/usr/src/repo/${pkg}_${revision}.tar" .
        fi
        touch -t 197001010000.00 "${pkg}_${revision}.tar"
        gzip "${pkg}_${revision}.tar"
    fi

    echo "${pkg}: checksumming created package."
    # shellcheck disable=SC2154
    test -z "${checksum}" || checksum="$(echo "${checksum} " "${pkg}_${revision}."*)"
    if echo "${checksum}" | grep -q ".links"; then
        checksum="$(echo "${checksum}" | cut -f'1 2 4' -d' ')"
    fi
    test -z "${checksum}" || echo "${checksum}" | sha256sum -c || \
        (echo "Expected: ${checksum}"; echo "Got: $(sha256sum $(echo "${checksum}" | cut -d' ' -f3))"; false)

    if command -v xbps-rindex >/dev/null 2>&1; then
        echo "${pkg}: adding package to repository."
        xbps-rindex --compression xz -a "/usr/src/repo/${pkg}_${revision}.${ARCH}.xbps"
    fi

    echo "${pkg}: cleaning up."
    rm -rf "${SOURCES}/${pkg}/build"
    rm -rf /tmp/destdir/
    mkdir -p /tmp/destdir/

    echo "${pkg}: installing package."
    if command -v xbps-install >/dev/null 2>&1; then
        xbps-install -y -R /usr/src/repo "${pkg%%-[0-9]*}"
    else
        # Overwriting files is mega busted, so do it manually
        while IFS= read -d $'\0' file; do
            rm -f "/${file}" >/dev/null 2>&1 || true
        done < /tmp/filelist.txt
        tar -C / -xzpf "/usr/src/repo/${pkg}_${revision}.tar.gz"
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
    fi

    echo "${pkg}: build successful"

    cd "${SOURCES}"

    unset -f src_unpack src_prepare src_configure src_compile src_install
    unset checksum
}

# Default unpacking function that unpacks all source tarballs.
default_src_unpack() {
    src_dir="${base_dir}/src"

    # Check for new tar
    if test -e "${PREFIX}/libexec/rmt"; then
        for i in "${src_dir}"/*; do
            tar --no-same-owner -xf "${i}"
        done
    else
        for i in "${src_dir}"/*.tar.gz; do
            [ -e "${i}" ] || continue
            tar -xzf "${i}"
        done
        for i in "${src_dir}"/*.tar.bz2; do
            [ -e "${i}" ] || continue
            tar -xf "${i}" --use-compress-program=bzip2
        done
        for i in "${src_dir}"/*.tar.xz; do
            [ -e "${i}" ] || continue
            tar -xf "${i}" --use-compress-program=xz
        done
        for i in "${src_dir}"/*.tar; do
            [ -e "${i}" ] || continue
            tar -xf "${i}"
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
