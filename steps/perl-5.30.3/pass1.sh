# SPDX-FileCopyrightText: 2025 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    chmod 644 cpan/Compress-Raw-Zlib/config.in
    sed "s:%LIBDIR%:${LIBDIR}:" Compress-Raw-Zlib_config.in > cpan/Compress-Raw-Zlib/config.in

    # Remove miscellaneous pregenerated files
    rm Porting/Glossary \
        dist/Devel-PPPort/parts/apidoc.fnc Configure config_h.SH \
        cpan/Win32API-File/cFile.pc cpan/Sys-Syslog/win32/Win32.pm \
        dist/ExtUtils-CBuilder/Makefile.PL \
        cpan/Test-Simple/lib/Test2/Util/HashBase.pm
    rm win32/perlexe.ico
    rm -r cpan/Compress-Raw-Zlib/zlib-src

    # Generated tests
    rm cpan/Unicode-Collate/Collate/keys.txt \
        dist/Devel-PPPort/t/*.t

    # Regenerate other prebuilt header files
    # Taken from headers of regen scripts
    rm lib/warnings.pm warnings.h regnodes.h reentr.h reentr.c overload.h \
        opcode.h opnames.h pp_proto.h keywords.h embed.h embedvar.h \
        perlapi.{c,h} proto.h lib/overload/numbers.pm regcharclass.h \
        perly.{tab,h,act} mg_{raw.h,vtable.h} keywords.c l1_char_class_tab.h \
        lib/feature.pm lib/B/Op_private.pm miniperlmain.c unicode_constants.h \
        charclass_invlists.h ebcdic_tables.h mg_names.inc overload.inc \
        packsizetables.inc uni_keywords.h
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
    perl regen/miniperlmain.pl
    perl regen/unicode_constants.pl
    perl lib/unicore/mktables -C lib/unicore -P pod -maketest -makelist -p
    perl regen/mk_invlists.pl

    mconf_dir=$(echo ../metaconfig*)
    ln -s "$mconf_dir"/.package .
    ln -s "$mconf_dir"/U .
    metaconfig -m

    # Glossary
    ln -s ../perl-* "$mconf_dir"/perl
    "$mconf_dir"/U/mkglossary > Porting/Glossary

    bash dist/Devel-PPPort/devel/mkapidoc.sh . \
        dist/Devel-PPPort/parts/apidoc.fnc \
        dist/Devel-PPPort/parts/embed.fnc

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
        -Darchname="i686-linux" \
        -Dmyhostname="(none)" \
        -Dmaildomain="(none)" \
        -Dccflags='-DPERL_BUILD_DATE="null"'
}

src_install() {
    default

    # Remove messed up manpages
    rm "${DESTDIR}/"*.0
}
