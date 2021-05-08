#!/bin/bash -e

# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

export PATH=/after/bin

# Common build steps
# Build function provides a few common stages with default implementation
# that can be overridden on per package basis in the build script.
# build takes three arguments:
# 1) name-version of the package
# 2) optionally specify build script. Default is name-version.sh
# 3) optionally specify name of checksum file. Default is checksums
# 4) directory of patches. Default is patches
build () {
    pkg=$1
    script_name=${2:-${pkg}.sh}
    checksum_f=${3:-checksums}

    cd "$pkg" || (echo "Cannot cd into ${pkg}!"; kill $$)
    echo "${pkg}: beginning build using script ${script_name}"
    base_dir="${PWD}"
    patch_dir="${base_dir}/${4:-patches}"
    mk_dir="${base_dir}/mk"
    files_dir="${base_dir}/files"

    rm -rf "build"
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

    cd "${pkg}" || (echo "Cannot cd into build/${pkg}!"; kill $$)

    echo "${pkg}: preparing source."
    build_stage=src_prepare
    call $build_stage

    echo "${pkg}: configuring source."
    build_stage=src_configure
    call $build_stage

    echo "${pkg}: compiling source."
    build_stage=src_compile
    call $build_stage

    echo "${pkg}: installing."
    build_stage=src_install
    call $build_stage

    cd ../..

    echo "${pkg}: checksumming installed files."
    test -e "${checksum_f}" && sha256sum -c "${checksum_f}"

    echo "${pkg}: build successful"
    cd ..

    unset -f src_unpack src_prepare src_configure src_compile src_install
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
