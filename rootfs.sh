#!/bin/bash

# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 Bastian Bittorf <bb@npl.de>
# SPDX-FileCopyrightText: 2020-2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -ex

QEMU_CMD="${1:-qemu-system-x86_64}"    # or 'chroot' or 'minikernel'
QEMU_RAM="${2:-8G}"

GITDIR="$PWD/$(dirname "$0")"
if [ ! -f 'rootfs.sh' ]; then
    echo 'must be run from base of repo'
    exit 1
fi

pushd sysa

# SYSTEM A

# Setup tmp
mkdir -p tmp/
sudo mount -t tmpfs -o size=8G tmpfs tmp

LOGFILE="$PWD/tmp/bootstrap.log"

_wget() {
    local url="$1"
    local dir="${CACHEDIR:-$GITDIR/sources}"
    local file="${2:-$(basename "${url}")}"
    mkdir -p "$dir"
    test -s "$dir/$file" || command wget -O "$dir/$file" "$url"
    cp -v "$dir/$file" .
    checksum_do "$dir" "$file"
}

checksum_do() {
    local dir="$1"
    local file="$2"
    local line
    local store="$GITDIR/SHA256SUMS.sources"

    if line=$(grep "[[:space:]][[:space:]]$file"$ "$store"); then
        (cd "$dir" && echo "$line" | sha256sum -c)
    else
        echo 'Checksum mismatch or not found!'
        exit 1
    fi
}

# base: mescc-tools-seed
# copy in all the mescc-tools-seed stuff
cp -r mescc-tools-seed/src/mescc-tools-seed/x86/* tmp
cp -r mescc-tools-seed/src/mescc-tools-seed/{M2-Planet,mes-m2} tmp/
cp -r mescc-tools-seed/src/mescc-tools-patched tmp/mescc-tools
# and the kaem seed
cp bootstrap-seeds/POSIX/x86/kaem-optional-seed tmp/init
cp bootstrap-seeds/POSIX/x86/kaem-optional-seed tmp/
cp -r bootstrap-seeds tmp/
# replace the init kaem with our own custom one
mv tmp/kaem.run tmp/mescc-tools-seed.kaem.run
cp base.kaem.run tmp/kaem.run
# create directories needed
mkdir -p tmp/bin

# after mescc-tools-seed we get into our own directory because
# the mescc-tools-seed one is hella messy
mkdir -p tmp/after/bin
mkdir -p tmp/after/{lib,include}
mkdir -p tmp/after/lib/{tcc,linux}
ln -s . tmp/after/lib/x86-mes
ln -s . tmp/after/lib/linux/x86-mes
mkdir -p tmp/after/include/{mes,gnu,linux,sys,mach}
mkdir -p tmp/after/include/linux/{x86,x86_64}
mkdir -p tmp/tmp
cp after.kaem tmp/
cp after.kaem.run tmp/after/kaem.run
cp mescc-tools-seed/checksums tmp/after/mescc-tools-seed-checksums
cp helpers.sh run.sh run2.sh pre-sha.sha256sums tmp/after/

# mescc-tools-extra
cp -r mescc-tools-extra tmp/after/

# mes
cp -r mes tmp/after/
#ln -s lib/x86-mes tmp/after/mes/src/mes/x86-mes
mkdir -p tmp/after/mes/src/mes/{bin,m2}

# tcc 0.9.26
cp -r tcc-0.9.26 tmp/after/
pushd tmp/after/tcc-0.9.26/src/tcc-0.9.26
ln -s ../mes/module .
ln -s ../mes/mes .
ln -s /after/lib x86-mes
ln -s /after/lib/linux .
popd

# tcc 0.9.27
cp -r tcc-0.9.27 tmp/after/

# tar 1.12
url=https://ftp.gnu.org/gnu/tar/tar-1.12.tar.gz
cp -r tar-1.12 tmp/after
mkdir tmp/after/tar-1.12/{src,build}
pushd tmp/after/tar-1.12/src
if [ ! -f "$(basename $url)" ]; then
    _wget "$url"
fi
popd
tar -C tmp/after/tar-1.12/src -xf "tmp/after/tar-1.12/src/$(basename $url)" --strip-components=1

get_file() {
    url=$1
    make_build=${2:-0}
    output_filename=$3
    ext="${url##*.}"
    if [ "$ext" = "tar" ]; then
        bname=$(basename "${output_filename:-${url}}" ".tar")
    else
        bname=$(basename "${output_filename:-${url}}" ".tar.${ext}")
    fi
    cp -r "${bname}" tmp/after/
    target="tmp/after/${bname}"
    mkdir -p "${target}/src"
    if [ "${make_build}" -ne 0 ]; then
        mkdir "${target}/build"
    fi
    pushd "tmp/after/${bname}/src"
    if [ ! -f "$(basename "$url")" ]; then
        _wget "$url" "${output_filename:-${url##*/}}"
    fi
    popd
}

# gzip 1.2.4
get_file https://ftp.gnu.org/gnu/gzip/gzip-1.2.4.tar 1

# sed 4.0.9
get_file https://ftp.gnu.org/gnu/sed/sed-4.0.9.tar.gz 1

# patch 2.5.9
get_file https://ftp.gnu.org/pub/gnu/patch/patch-2.5.9.tar.gz 1

# sha-2 61555d
get_file https://github.com/amosnier/sha-2/archive/61555d.tar.gz 1 sha-2-61555d.tar.gz

# make 3.80
get_file https://ftp.gnu.org/gnu/make/make-3.80.tar.gz 1

# bzip2 1.0.8
get_file ftp://sourceware.org/pub/bzip2/bzip2-1.0.8.tar.gz 1

# coreutils 5.0
get_file https://ftp.gnu.org/gnu/coreutils/coreutils-5.0.tar.bz2 1

# heirloom-devtools
get_file http://downloads.sourceforge.net/project/heirloom/heirloom-devtools/070527/heirloom-devtools-070527.tar.bz2

# bash 2.05b
get_file https://ftp.gnu.org/pub/gnu/bash/bash-2.05b.tar.gz

# flex 2.5.11
get_file http://download.nust.na/pub2/openpkg1/sources/DST/flex/flex-2.5.11.tar.gz

# musl 1.1.24
get_file https://musl.libc.org/releases/musl-1.1.24.tar.gz

# m4 1.4.7
get_file https://ftp.gnu.org/gnu/m4/m4-1.4.7.tar.gz

# flex 2.6.4
get_file https://github.com/westes/flex/releases/download/v2.6.4/flex-2.6.4.tar.gz

# bison 3.4.1
get_file https://ftp.gnu.org/gnu/bison/bison-3.4.1.tar.gz

# grep 2.4
get_file https://ftp.gnu.org/gnu/grep/grep-2.4.tar.gz

# diffutils 2.7
get_file https://ftp.gnu.org/gnu/diffutils/diffutils-2.7.tar.gz

# coreutils 6.10
get_file https://ftp.gnu.org/gnu/coreutils/coreutils-6.10.tar.gz

# gawk 3.0.4
get_file https://ftp.gnu.org/gnu/gawk/gawk-3.0.4.tar.gz

# perl 5.000
get_file https://github.com/Perl/perl5/archive/perl-5.000.tar.gz

# perl 5.003
get_file https://github.com/Perl/perl5/archive/perl-5.003.tar.gz

# perl 5.004_05
get_file https://www.cpan.org/src/5.0/perl5.004_05.tar.gz

# perl 5.005_03
get_file https://www.cpan.org/src/5.0/perl5.005_03.tar.gz

# perl 5.6.2
get_file https://www.cpan.org/src/5.0/perl-5.6.2.tar.gz

# autoconf 2.52
get_file https://ftp.gnu.org/gnu/autoconf/autoconf-2.52.tar.bz2

# automake 1.6.3
get_file https://ftp.gnu.org/gnu/automake/automake-1.6.3.tar.bz2

# automake 1.4-p6
get_file https://ftp.gnu.org/gnu/automake/automake-1.4-p6.tar.gz

# autoconf 2.13
get_file https://ftp.gnu.org/gnu/autoconf/autoconf-2.13.tar.gz

# autoconf 2.12
get_file https://ftp.gnu.org/gnu/autoconf/autoconf-2.12.tar.gz

# libtool 1.4
get_file https://ftp.gnu.org/gnu/libtool/libtool-1.4.tar.gz

# binutils 2.14
get_file https://ftp.gnu.org/gnu/binutils/binutils-2.14.tar.bz2

# autoconf 2.53
get_file https://ftp.gnu.org/gnu/autoconf/autoconf-2.53.tar.bz2

# automake 1.7
get_file https://ftp.gnu.org/gnu/automake/automake-1.7.tar.bz2

# autoconf 2.54
get_file https://ftp.gnu.org/gnu/autoconf/autoconf-2.54.tar.bz2

# autoconf 2.55
get_file https://ftp.gnu.org/gnu/autoconf/autoconf-2.55.tar.bz2

# automake 1.7.8
get_file https://ftp.gnu.org/gnu/automake/automake-1.7.8.tar.bz2

# autoconf 2.57
get_file https://ftp.gnu.org/gnu/autoconf/autoconf-2.57.tar.bz2

# autoconf 2.59
get_file https://ftp.gnu.org/gnu/autoconf/autoconf-2.59.tar.bz2

# automake 1.8.5
get_file https://ftp.gnu.org/gnu/automake/automake-1.8.5.tar.bz2

# help2man 1.36.4
get_file https://ftp.gnu.org/gnu/help2man/help2man-1.36.4.tar.gz

# autoconf 2.61
get_file https://ftp.gnu.org/gnu/autoconf/autoconf-2.61.tar.bz2

# automake 1.9.6
get_file https://ftp.gnu.org/gnu/automake/automake-1.9.6.tar.bz2

# findutils 4.2.33
get_file https://ftp.gnu.org/gnu/findutils/findutils-4.2.33.tar.gz

# libtool 2.2.4
get_file https://ftp.gnu.org/gnu/libtool/libtool-2.2.4.tar.bz2

# automake 1.10.3
get_file https://ftp.gnu.org/gnu/automake/automake-1.10.3.tar.bz2

# autoconf 2.65
get_file https://ftp.gnu.org/gnu/autoconf/autoconf-2.65.tar.bz2

# gcc 4.0.4
get_file https://ftp.gnu.org/gnu/gcc/gcc-4.0.4/gcc-core-4.0.4.tar.bz2 0 gcc-4.0.4.tar.bz2

# musl 1.2.2
get_file https://musl.libc.org/releases/musl-1.2.2.tar.gz

# bash 5.1
get_file https://ftp.gnu.org/gnu/bash/bash-5.1.tar.gz

# xz 5.0.5
get_file https://tukaani.org/xz/xz-5.0.5.tar.bz2

# automake 1.11.2
get_file https://ftp.gnu.org/gnu/automake/automake-1.11.2.tar.bz2

# autoconf 2.69
get_file https://ftp.gnu.org/gnu/autoconf/autoconf-2.69.tar.xz

# General cleanup
find tmp -name .git -exec rm -rf \;

# initramfs
cd tmp
find . | cpio -H newc -o | gzip > initramfs.igz

# Run
case "${QEMU_CMD}" in
    chroot)
        sudo PATH="/after/bin:${PATH}" chroot . /init 2>&1 | tee "$LOGFILE"
    ;;
    minikernel)
        git clone --depth 1 --branch v0.4 https://github.com/bittorf/kritis-linux.git

        kritis-linux/ci_helper.sh \
            --arch x86_64 \
            --ramsize 4G \
            --kernel 5.10.8 \
            --initrd initramfs.igz \
            --log "$LOGFILE"
    ;;
    *)
        ${QEMU_CMD} -enable-kvm \
            -m "${QEMU_RAM:-8G}" \
            -nographic \
            -no-reboot \
            -kernel ../../kernel -initrd initramfs.igz -append console=ttyS0 | tee "$LOGFILE"
    ;;
esac

cd ../..

# Cleanup
sudo umount sysa/tmp
