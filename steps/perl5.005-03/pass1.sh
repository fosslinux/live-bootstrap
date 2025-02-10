# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # Regenerate bison files
    rm -f perly.c perly.h
    bison -d perly.y
    mv perly.tab.c perly.c
    mv perly.tab.h perly.h

    # Regenerate other prebuilt header files
    rm -f embedvar.h
    for file in embed keywords opcode; do
        rm -f ${file}.h
        perl ${file}.pl
    done
    rm -f regnodes.h
    perl regcomp.pl
    rm -f byterun.h byterun.c ext/B/B/Asmdata.pm
    perl bytecode.pl
}
