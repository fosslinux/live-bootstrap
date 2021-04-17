#!/bin/bash

# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

. helpers.sh

build xz-5.0.5

build automake-1.11.2

build autoconf-2.69

build automake-1.15.1

build tar-1.34

build gmp-6.2.1

build autoconf-archive-2021.02.19

build mpfr-4.1.0

echo "Bootstrapping completed."

exec env - PATH=/after/bin PS1="\w # " bash -i
