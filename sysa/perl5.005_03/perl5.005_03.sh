# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default_src_prepare

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
    rm regnodes.h
    perl regcomp.pl
    rm byterun.h byterun.c
    perl bytecode.pl
}

src_install() {
    default_src_install

    # Remove old perl
    rm -rf "${PREFIX}"/lib/perl5/5.004_05
}
