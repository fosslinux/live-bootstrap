#!/bin/bash

# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
# SPDX-FileCopyrightText: 2021 Melg Eight <public.melg8@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

# shellcheck source=sysa/helpers.sh
. helpers.sh

. bootstrap.cfg

build xz-5.0.5

build automake-1.11.2

build autoconf-2.69

build automake-1.15.1

build tar-1.34

build coreutils-8.32

build make-4.2.1

build gmp-6.2.1

build autoconf-archive-2021.02.19

build mpfr-4.1.0

build mpc-1.2.1

build flex-2.5.33

build bison-2.3

build bison-3.4.2

build perl-5.10.1

build dist-3.5-236 '' '' '' dist-d1de81f

build perl-5.32.1

build automake-1.16.3

if [ "$FORCE_TIMESTAMPS" = True ] ; then
    echo 'Forcing all files timestamps to be 0 unix time.'
    canonicalise_all_files_timestamp
fi

echo "Bootstrapping completed."

exec env - PATH=/after/bin PS1="\w # " bash -i
