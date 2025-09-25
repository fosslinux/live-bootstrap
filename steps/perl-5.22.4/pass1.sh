# SPDX-FileCopyrightText: 2025 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    # change unicode files to 5.24 version, in particular mktables
    # This is required to have the necessary tables for the 5.24 build
    mv ../perl-5.24.4/lib/unicore/{version,mktables,*.txt} lib/unicore/
    mv ../perl-5.24.4/lib/unicore/auxiliary/*.txt lib/unicore/auxiliary/
    mv ../perl-5.24.4/lib/unicore/extracted/*.txt lib/unicore/extracted/

    default

    chmod 644 cpan/Compress-Raw-Zlib/config.in
    sed "s:%LIBDIR%:${LIBDIR}:" Compress-Raw-Zlib_config.in > cpan/Compress-Raw-Zlib/config.in

    # Remove miscellaneous pregenerated files
    rm Porting/Glossary \
        cpan/Devel-PPPort/parts/apidoc.fnc Configure config_h.SH \
        cpan/Win32API-File/cFile.pc cpan/Sys-Syslog/win32/Win32.pm \
        dist/ExtUtils-CBuilder/Makefile.PL
    rm win32/perlexe.ico
    rm -r cpan/Compress-Raw-Zlib/zlib-src

    # Generated tests
    rm cpan/Unicode-Collate/Collate/keys.txt \
        cpan/Devel-PPPort/t/*.t

    # Regenerate other prebuilt header files
    # Taken from headers of regen scripts
    rm lib/warnings.pm warnings.h regnodes.h reentr.h reentr.c overload.h \
        opcode.h opnames.h pp_proto.h keywords.h embed.h embedvar.h \
        perlapi.{c,h} proto.h lib/overload/numbers.pm regcharclass.h \
        perly.{tab,h,act} mg_{raw.h,vtable.h} keywords.c l1_char_class_tab.h \
        lib/feature.pm lib/B/Op_private.pm miniperlmain.c unicode_constants.h \
        charclass_invlists.h ebcdic_tables.h packsizetables.c overload.c \
        mg_names.c
    # If an input file does not exist, the "digest" of the input file (used as
    # a manifest of inputs) in the generated file is a random number, which is
    # not reproducible
    touch lib/unicore/mktables.lst

    perl regen.pl
    perl regen_perly.pl -b bison-2.3
    perl regen/keywords.pl
    perl regen/mk_PL_charclass.pl
    perl regen/regcharclass.pl
    perl regen/genpacksizetables.pl
    perl regen/ebcdic.pl
    perl regen/mk_invlists.pl
    perl regen/miniperlmain.pl
    perl regen/unicode_constants.pl

    # regenerate configure
    ln -s ../metaconfig*/.package .
    ln -s ../metaconfig*/U .
    metaconfig -m

    # Glossary
    sed -i -e "s:/pro/3gl/CPAN/lib/dist:${PREFIX}/lib/perl5/5.6.2:" \
        -e "s:/pro/3gl/CPAN/perl:$PWD:" \
        ../metaconfig*/U/mkglossary
    chmod +x ../metaconfig*/U/mkglossary
    ../metaconfig*/U/mkglossary > Porting/Glossary

    bash cpan/Devel-PPPort/devel/mkapidoc.sh . \
        cpan/Devel-PPPort/parts/apidoc.fnc \
        cpan/Devel-PPPort/parts/embed.fnc

    # Remove lines from MANIFEST that we have deleted
    while read -r line; do
        f="$(echo "${line}" | cut -d' ' -f1)"
        if [ -e "${f}" ]; then
            echo "${line}"
        fi
    done < MANIFEST > MANIFEST.new
    mv MANIFEST.new MANIFEST
}

src_configure() {
    ./Configure -des \
        -Dprefix="${PREFIX}" \
        -Dcc=gcc \
        -Dusedl=false \
        -Ddate=':' \
        -Dccflags="-U__DATE__ -U__TIME__" \
        -Darchname="i686-linux" \
        -Dmyhostname="(none)" \
        -Dmaildomain="(none)"
}

src_install() {
    default

    # Remove messed up manpages
    rm "${DESTDIR}/"*.0
}
