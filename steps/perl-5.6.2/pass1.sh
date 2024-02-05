# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # Regenerate bison files
    sed -i '/yydestruct/d' perly.y
    rm -f perly.c perly.h
    bison -d perly.y
    mv perly.tab.c perly.c
    mv perly.tab.h perly.h

    # Regenerate other prebuilt header files
    for file in embed keywords opcode; do
        rm -f ${file}.h
        perl ${file}.pl
    done
    rm -f regnodes.h
    perl regcomp.pl
    rm -f ext/ByteLoader/byterun.h ext/ByteLoader/byterun.c
    perl bytecode.pl
    rm -f warnings.h lib/warnings.pm
    perl warnings.pl

    # Workaround for some linking problems, remove if possible
    sed -i 's/perl_call_method/Perl_call_method/' ext/Data/Dumper/Dumper.xs
    sed -i 's/perl_call_sv/Perl_call_sv/' ext/Data/Dumper/Dumper.xs
    sed -i 's/sv_setptrobj/Perl_sv_setref_iv/' ext/POSIX/POSIX.xs

    # We are using non-standard locations
    sed -i "s#/usr/include/errno.h#${PREFIX}/include/bits/errno.h#" ext/Errno/Errno_pm.PL
}

src_compile() {
    make -j1 PREFIX="${PREFIX}"
}
