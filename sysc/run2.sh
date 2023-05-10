#!/usr/bin/bash

# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
# SPDX-FileCopyrightText: 2021 Melg Eight <public.melg8@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

# shellcheck source=/dev/null
. .env

# shellcheck disable=SC2153
trap 'env - PATH=${PREFIX}/bin PS1="\w # " bash -i' EXIT

# shellcheck source=sysa/helpers.sh
. helpers.sh

build xz-5.4.1

build file-5.44

build libtool-2.4.7

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

build dist-3.5-236

build perl-5.32.1

build libarchive-3.5.2

build openssl-1.1.1l

build ca-certificates-3.88.1

build curl-7.88.1 pass2.sh

build zlib-1.2.13

build automake-1.16.3

build autoconf-2.71

build patch-2.7.6

build gettext-0.21

build texinfo-6.7

build gcc-4.7.4

build binutils-2.38 pass1.sh

build gperf-3.1

build libunistring-0.9.10

build libffi-3.3

build libatomic_ops-7.6.10

build gc-8.0.4

build guile-3.0.9

build which-2.21

build grep-3.7

build sed-4.8

build autogen-5.18.16 autogen-5.18.16.sh

build musl-1.2.3

build python-2.0.1 stage1.sh

build python-2.0.1 stage2.sh

build python-2.3.7 stage1.sh

build python-2.3.7 stage2.sh

build python-2.5.6

build python-3.1.5 stage1.sh

build python-3.1.5 stage2.sh

build python-3.3.7

build python-3.4.10

build python-3.8.16

build python-3.11.1

[ "${INTERNAL_CI}" = "pass2" ] && exit 0

build gcc-10.4.0

build binutils-2.38 pass2.sh

build gcc-13.1.0

if [ "$FORCE_TIMESTAMPS" = True ] ; then
    echo 'Forcing all files timestamps to be 0 unix time.'
    canonicalise_all_files_timestamp
fi

shopt -s extglob
if [ "$UPDATE_CHECKSUMS" = True ] ; then
    pushd /usr/src/repo
    sha256sum -- !(*-repodata) | tee "${SOURCES}/SHA256SUMS.pkgs"
    popd
fi

echo "Bootstrapping completed."

cd "/"
exec env -i bash "${SOURCES}/after.sh"

