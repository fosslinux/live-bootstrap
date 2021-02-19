#!/bin/bash

# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-License-Identifier: GPL-3.0-or-later

set -e
# shellcheck source=sysa/helpers.sh
. helpers.sh

export PREFIX=/after

# Part 19
build flex-2.5.11

# Part 20
build musl-1.1.24

# Part 21
build tcc-0.9.27 tcc-musl.sh
build musl-1.1.24

# Part 22
build m4-1.4.7

# Part 23
build flex-2.6.4

# Part 24
build bison-3.4.1 stage1.sh
build bison-3.4.1 stage2.sh
build bison-3.4.1 stage3.sh

# Part 25
build grep-2.4

# Part 26
build diffutils-2.7

# Part 27
build coreutils-5.0

# Part 28
build gawk-3.0.4

# Part 29
build perl-5.000

# Part 30
build perl-5.003

# Part 31
build perl5.004_05

echo "Bootstrapping completed."
