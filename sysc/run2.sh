#!/bin/bash

# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
# SPDX-FileCopyrightText: 2021 Melg Eight <public.melg8@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

# shellcheck disable=SC2153
trap 'env - PATH=${PREFIX}/bin PS1="\w # " bash -i' EXIT

# shellcheck source=sysa/helpers.sh
. helpers.sh

trap bash EXIT

umask 0022

build xz-5.0.5

build automake-1.11.2

build autoconf-2.69

build automake-1.15.1

build tar-1.34

build coreutils-8.32

build pkg-config-0.29.2

build make-4.2.1

build gmp-6.2.1

build autoconf-archive-2021.02.19

build mpfr-4.1.0

build mpc-1.2.1

build flex-2.5.33

build bison-2.3

build bison-3.4.2

build perl-5.10.1

build dist-3.5-236 '' '' dist-d1de81f

build perl-5.32.1

build libarchive-3.5.2

build openssl-1.1.1l

build zlib-1.2.12

build xbps-0.59.1
export XBPS_TARGET_ARCH="${ARCH}"

build automake-1.16.3

build autoconf-2.71

build patch-2.7.6

build gettext-0.21

build texinfo-6.7

build gcc-4.7.4

build gperf-3.1

build libunistring-0.9.10

build libffi-3.3

build libatomic_ops-7.6.10

build gc-8.0.4

build guile-3.0.7

if [ "$FORCE_TIMESTAMPS" = True ] ; then
    echo 'Forcing all files timestamps to be 0 unix time.'
    canonicalise_all_files_timestamp
fi

if [ "$UPDATE_CHECKSUMS" = True ] ; then
    pushd /usr/src/repo
    sha256sum -- * | tee "${SOURCES}/SHA256SUMS.pkgs"
    popd
fi

echo "Bootstrapping completed."

cd "${PREFIX}"
exec env - PATH="${PREFIX}/bin" PS1="\w # " bash -i
