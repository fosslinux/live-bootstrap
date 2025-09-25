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
        x2p/a2p.c cpan/Win32API-File/cFile.pc cpan/Sys-Syslog/win32/Win32.pm
    rm win32/perlexe.ico
    rm -r cpan/Compress-Raw-Zlib/zlib-src

    # Generated tests
    rm cpan/Devel-PPPort/t/*.t cpan/Unicode-Collate/Collate/keys.txt

    # toke.c Perl_keyword
    # bit before the generated bit
    sed '/The following code was generated/,$d' toke.c | head -n -1 > toke.c.new
    perl perl_keyword.pl >> toke.c.new
    # bit after the generated bit
    # sed with two -e does not appear to be working with our sed
    sed '1,/The following code was generated/d' toke.c | sed '1,/^}$/d' >> toke.c.new
    mv toke.c.new toke.c

    # Regenerate other prebuilt header files
    # Taken from headers of regen scripts
    rm lib/warnings.pm warnings.h regnodes.h reentr.h reentr.c pp.sym \
          overload.h overload.c opcode.h opnames.h pp_proto.h \
          keywords.h embed.h embedvar.h global.sym perlapi.c perlapi.h \
          proto.h lib/overload/numbers.pm regcharclass.h perly.{tab,h,act}
    perl regen.pl
    perl regen_perly.pl -b bison-2.3
    touch regcharclass.h
    perl Porting/regcharclass.pl

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

    # For some reason Configure fails to generate x2p/Makefile
    # wrong metaconfig version? not bothered to figure out
    pushd x2p
    ./Makefile.SH
    make depend
    popd
}

src_compile() {
    pushd x2p
    make BYACC=yacc run_byacc
    popd

    # there are concurrency issues
    make -j1 PREFIX="${PREFIX}"
}

src_install() {
    default

    # Remove messed up manpages
    rm "${DESTDIR}/"*.0
}
