# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_unpack() {
    src_dir="${base_dir}/src"
    tar -xf "${src_dir}/${pkg}.tar"
}

src_prepare() {
    cp lib/fnmatch_.h lib/fnmatch.h
    cp lib/ftw_.h lib/ftw.h
    cp lib/search_.h lib/search.h
    touch config.h

    # Bison pre-generated file
    rm lib/getdate.c

    cp "${mk_dir}/pass2.mk" Makefile
}

src_compile() {
    make -f Makefile
}

src_install() {
    make -f Makefile install PREFIX="${PREFIX}"
}
