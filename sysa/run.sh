#!/bin/bash

# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -e
# shellcheck source=sysa/helpers.sh
. helpers.sh

populate_device_nodes() {
    # http://www.linuxfromscratch.org/lfs/view/6.1/chapter06/devices.html
    test -c /dev/null || mknod -m 666 /dev/null c 1 3
    test -c /dev/zero || mknod -m 666 /dev/zero c 1 5
    test -c /dev/ptmx || mknod -m 666 /dev/ptmx c 5 2
    test -c /dev/tty || mknod -m 666 /dev/tty c 5 0
    test -c /dev/random || mknod -m 444 /dev/random c 1 8
    test -c /dev/urandom || mknod -m 444 /dev/urandom c 1 9
}

export PREFIX=/image
export PATH="${PREFIX}/bin"
export SOURCES=/after

build flex-2.5.11

# Patch meslibc to support > 255 command line arguments
build mes-0.23 mes-libc-0.23.sh

build tcc-0.9.27 tcc-meslibc-rebuild.sh checksums/tcc-meslibc-rebuild

build musl-1.1.24 '' checksums/pass1

# Rebuild tcc using musl
build tcc-0.9.27 tcc-musl-pass1.sh checksums/tcc-musl-pass1

# Rebuild musl using tcc-musl
build musl-1.1.24 '' checksums/pass2

# Rebuild tcc-musl using new musl
build tcc-0.9.27 tcc-musl-pass2.sh checksums/tcc-musl-pass2

# Rebuild sed using musl
build sed-4.0.9 sed-4.0.9.sh checksums/pass2

# Rebuild bzip2 using musl
build bzip2-1.0.8 bzip2-1.0.8.sh checksums/bzip2-pass2

build m4-1.4.7

build flex-2.6.4

build bison-3.4.1 stage1.sh checksums/stage1
build bison-3.4.1 stage2.sh checksums/stage2
build bison-3.4.1 stage3.sh checksums/stage3

build grep-2.4

build diffutils-2.7

# Rebuild coreutils using musl
build coreutils-5.0 coreutils-5.0.sh checksums/pass2

# Build only date, mktemp and sha256sum
build coreutils-6.10

build gawk-3.0.4

build perl-5.000

build perl-5.003

build perl5.004_05

build perl5.005_03

build perl-5.6.2

populate_device_nodes

build autoconf-2.52 stage1.sh

build automake-1.6.3 stage1.sh
build automake-1.6.3 stage2.sh
build automake-1.6.3 stage3.sh

build automake-1.4-p6

build autoconf-2.52 stage2.sh

build autoconf-2.13

build autoconf-2.12

build libtool-1.4

build binutils-2.14

# Build musl with fewer patches
build musl-1.1.24 binutils-rebuild.sh checksums/pass3 patches-pass3
populate_device_nodes

# Rebuild tcc-musl using new musl
build tcc-0.9.27 tcc-musl-pass3.sh checksums/tcc-musl-pass3 patches-musl-pass3

build autoconf-2.53 stage1.sh
build autoconf-2.53 stage2.sh

build automake-1.7 stage1.sh

build autoconf-2.54 stage1.sh
build autoconf-2.54 stage2.sh

build automake-1.7 stage2.sh

build autoconf-2.55

build automake-1.7.8

build autoconf-2.57

build autoconf-2.59

build automake-1.8.5

build help2man-1.36.4

build autoconf-2.61 stage1.sh
build autoconf-2.61 stage2.sh

build automake-1.9.6 stage1.sh
build automake-1.9.6 stage2.sh

build findutils-4.2.33

build libtool-2.2.4

build automake-1.10.3

build autoconf-2.65

build gcc-4.0.4 pass1.sh checksums/pass1

build musl-1.2.2

build gcc-4.0.4 pass2.sh checksums/pass2

build bash-5.1

exec env -i PATH=${PREFIX}/bin PREFIX=${PREFIX} SOURCES=${SOURCES} bash run2.sh
