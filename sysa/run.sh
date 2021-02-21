#!/bin/bash

# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -e
# shellcheck source=sysa/helpers.sh
. helpers.sh

export PREFIX=/after

# Part 20
build flex-2.5.11

# Part 21
build musl-1.1.24 musl-1.1.24.sh checksums/pass1

# Part 22
build tcc-0.9.27 tcc-musl-pass1.sh checksums/tcc-musl-pass1

# Part 23
build musl-1.1.24 musl-1.1.24.sh checksums/pass2

# Part 24
build tcc-0.9.27 tcc-musl-pass2.sh checksums/tcc-musl-pass2

# Part 25
build m4-1.4.7

# Part 26
build flex-2.6.4

# Part 27
build bison-3.4.1 stage1.sh checksums/stage1
build bison-3.4.1 stage2.sh checksums/stage2
build bison-3.4.1 stage3.sh checksums/stage3

# Part 28
build grep-2.4

# Part 29
build diffutils-2.7

# Part 30
build coreutils-5.0 coreutils-5.0.sh checksums/pass2

# Part 31
build gawk-3.0.4

# Part 32
build perl-5.000

# Part 33
build perl-5.003

# Part 34
build perl5.004_05

# Part 35
build perl5.005_03

echo "Bootstrapping completed."
