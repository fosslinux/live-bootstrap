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
    test -c /dev/console || mknod -m 622 /dev/console c 5 1
    test -c /dev/null || mknod -m 666 /dev/null c 1 3
    test -c /dev/zero || mknod -m 666 /dev/zero c 1 5
    test -c /dev/ptmx || mknod -m 666 /dev/ptmx c 5 2
    test -c /dev/tty || mknod -m 666 /dev/tty c 5 0
    test -c /dev/random || mknod -m 444 /dev/random c 1 8
    test -c /dev/urandom || mknod -m 444 /dev/urandom c 1 9
}

export PREFIX=/after

build flex-2.5.11

build musl-1.1.24 musl-1.1.24.sh checksums/pass1

# Rebuild tcc using musl
build tcc-0.9.27 tcc-musl-pass1.sh checksums/tcc-musl-pass1

# Rebuild musl using tcc-musl
build musl-1.1.24 musl-1.1.24.sh checksums/pass2

# Rebuild tcc-musl using new musl
build tcc-0.9.27 tcc-musl-pass2.sh checksums/tcc-musl-pass2

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

build gawk-3.0.4

build perl-5.000

build perl-5.003

build perl5.004_05

build perl5.005_03

build perl-5.6.2

populate_device_nodes

build autoconf-2.52

build automake-1.4-p6 stage1.sh
build automake-1.4-p6 stage2.sh

echo "Bootstrapping completed."
