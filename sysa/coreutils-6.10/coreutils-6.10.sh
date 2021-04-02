# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default_src_prepare
    mv lib/fnmatch.in.h lib/fnmatch.h

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

