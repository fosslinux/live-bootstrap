# SPDX-FileCopyrightText: 2025 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    chmod 644 cpan/Compress-Raw-Zlib/config.in
    sed "s:%LIBDIR%:${LIBDIR}:" Compress-Raw-Zlib_config.in > cpan/Compress-Raw-Zlib/config.in

    # Remove miscellaneous pregenerated files
    rm Porting/Glossary \
        cpan/Devel-PPPort/parts/apidoc.fnc Configure config_h.SH \
        x2p/a2p.c cpan/Win32API-File/cFile.pc cpan/Sys-Syslog/win32/Win32.pm \
        utils/Makefile
    rm win32/perlexe.ico
    rm -r cpan/Compress-Raw-Zlib/zlib-src

    # Generated tests
    rm cpan/Devel-PPPort/t/*.t cpan/Unicode-Collate/Collate/keys.txt

    # Regenerate other prebuilt header files
    # Taken from headers of regen scripts
    rm lib/warnings.pm warnings.h regnodes.h reentr.h reentr.c \
          overload.h overload.c opcode.h opnames.h pp_proto.h \
          keywords.h embed.h embedvar.h perlapi.c perlapi.h \
          proto.h lib/overload/numbers.pm regcharclass.h perly.{tab,h,act} \
          mg_{raw.h,vtable.h,names.c} keywords.c l1_char_class_tab.h \
          lib/feature.pm unicode_constants.h charclass_invlists.h
    perl regen.pl
    perl regen_perly.pl -b bison-2.3
    perl regen/keywords.pl
    perl regen/mk_PL_charclass.pl
    perl regen/mk_invlists.pl
    perl regen/unicode_constants.pl
    perl regen/regcharclass.pl

    # Change the name, patching the generator script is not easy
    # Because of 0005-Move-an-inversion-list-generation-to-mktables.patch
    sed -i "s/_Perl_Multi_Char_Folds_invlist/_Perl_Folds_To_Multi_Char_invlist/" charclass_invlists.h

    # regenerate configure
    ln -s ../metaconfig*/.package .
    ln -s ../metaconfig*/U .
    metaconfig -m

    # Glossary
    pushd Porting
    ln -s /usr/lib/perl5/5.6.2/U .
    makegloss
    popd

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

    # Remains unclear why this is necessary
    pushd x2p
    ./Makefile.SH
    make depend
    popd

    pushd utils
    bash Makefile.SH
    popd
}

src_compile() {
    pushd x2p
    make BYACC=yacc run_byacc
    popd

    # The Revert-regen-mk_PL_charclass-pl.patch breaks building some
    # modules. The error messages are fairly cryptic. It seems probable that
    # the symbols have been used elsewhere in the codebase or there is a
    # reliance on different behaviour.
    # 
    # We will build miniperl, revert the patch, regenerate l1_char_class_tab.h,
    # (using miniperl), and then go again
 
    make "${MAKEJOBS}" miniperl lib/unicore/Name.pm
    patch -Np1 -R -i ../../patches/Revert-regen-mk_PL_charclass-pl.patch
    rm l1_char_class_tab.h
    ./miniperl -Ilib regen/mk_PL_charclass.pl

    # there are concurrency issues
    make -j1 PREFIX="${PREFIX}"
}

src_install() {
    default

    # Remove messed up manpages
    rm "${DESTDIR}/"*.0
}
