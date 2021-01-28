#!/bin/bash -e

# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-License-Identifier: GPL-3.0-or-later

export PATH=/after/bin

# Common build steps
# Build function provides a few common stages with default implementation
# that can be overridden on per package basis in the build script.
# build takes two arguments:
# 1) name-version of the package
# 2) optionally specify build script. Default is name-version.sh
build () {
    pkg=$1
    script_name=${2:-${pkg}.sh}

    cd "$pkg" || (echo "Cannot cd into ${pkg}!"; kill $$)
    echo "${pkg}: beginning build using script ${script_name}"
    base_dir="${PWD}"
    patch_dir="${base_dir}/patches"
    mk_dir="${base_dir}/mk"
    files_dir="${base_dir}/files"

    rm -rf "build"
    mkdir -p "build"

    cd "build"

    echo "${pkg}: unpacking source."
    call src_unpack

    build_script="${base_dir}/${script_name}"
    if test -e "${build_script}"; then
        # shellcheck source=/dev/null
        . "${build_script}"
    fi
    cd "${pkg}" || (echo "Cannot cd into build/${pkg}!"; kill $$)

    echo "${pkg}: preparing source."
    call src_prepare

    echo "${pkg}: configuring source."
    call src_configure

    echo "${pkg}: compiling source."
    call src_compile

    echo "${pkg}: installing."
    call src_install

    cd ../..

    echo "${pkg}: build successful"
    cd ..
}

# Default unpacking function that unpacks a single source tarball.
default_src_unpack() {
    src_dir="${base_dir}/src"

    for suf in gz bz2 xz; do
        source="${src_dir}/${pkg}.tar.${suf}"
        if test -e "${source}"; then
            case "${suf}" in
                gz) xtr="z" ;;
                bz2) xtr="j" ;;
                xz) xtr="J" ;;
            esac
            tar "-${xtr}" -xf "${source}"
            break
        fi
    done
}

# Default function to prepare source code.
# It applies all patches from patch_dir (at the moment only -p0 patches are supported).
# Then it copies our custom makefile and any other custom files from files directory.
default_src_prepare() {
    if test -d "${patch_dir}"; then
        for p in "${patch_dir}"/*.patch; do
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
    make
}

# Default installing function. PREFIX should be set by run.sh script.
# Note that upstream makefiles might ignore PREFIX and have to be configured in configure stage.
default_src_install() {
    make install PREFIX="${PREFIX}"
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
