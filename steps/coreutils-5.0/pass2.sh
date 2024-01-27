# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    cp lib/fnmatch_.h lib/fnmatch.h
    cp lib/ftw_.h lib/ftw.h
    cp lib/search_.h lib/search.h
    touch config.h

    # Rebuild pregenerated file
    rm -f src/false.c

    rm -f src/dircolors.h

    # Bison pre-generated file
    rm lib/getdate.c

    cp "${mk_dir}/pass2.mk" Makefile
}
