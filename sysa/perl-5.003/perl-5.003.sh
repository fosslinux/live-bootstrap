# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_unpack() {
    default

    mv perl5-perl-5.003 perl-5.003
}

src_prepare() {
    default

    # Regenerate bison files
    rm perly.c perly.h
    bison -d perly.y
    mv perly.tab.c perly.c
    mv perly.tab.h perly.h

    # Regenerate other prebuilt header files
    for file in embed keywords opcode; do
        rm ${file}.h
        perl ${file}.pl
    done
}
