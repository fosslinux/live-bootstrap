# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default_src_prepare

    # Regenerate bison files
    sed -i '/yydestruct/d' perly.y
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
    rm ext/ByteLoader/byterun.h ext/ByteLoader/byterun.c
    perl bytecode.pl
    rm warnings.h lib/warnings.pm
    perl warnings.pl
}

src_install() {
    # Remove old perl
    rm -rf "${PREFIX}"/lib/perl5/

    default_src_install
}
