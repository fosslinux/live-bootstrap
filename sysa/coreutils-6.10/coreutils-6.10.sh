# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=7ee4b097f5a896af925b53eee7055ba87dad89949f44d14695566249f45a5eaf

src_prepare() {
    default
    mv lib/fnmatch.in.h lib/fnmatch.h

    # gperf pregenerated files
    rm lib/iconv_open-hpux.h lib/iconv_open-aix.h lib/iconv_open-irix.h lib/iconv_open-osf.h

    # Rebuild bison pre-generated file
    rm lib/getdate.c
    cd lib
    bison --update getdate.y
    bison getdate.y
    mv getdate.tab.c getdate.c
    cd ..

    touch config.h
    touch lib/configmake.h
}

