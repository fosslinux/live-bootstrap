# SPDX-FileCopyrightText: 2025 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    mv ../Digest-SHA-6.04 ext/Digest/SHA/

    # Remove miscellaneous pregenerated files
    rm Porting/Glossary lib/unicore/mktables.lst \
        ext/Sys/Syslog/win32/Win32.pm ext/Win32API/File/cFile.pc \
        ext/Devel/PPPort/parts/apidoc.fnc Configure config_h.SH \
        x2p/a2p.c
    # Generated tests
    rm ext/Devel/PPPort/t/*.t

    # Remove jpl
    rm -r jpl

    # Regenerate bison files
    sed -i '/yydestruct/d' perly.y
    rm -f perly.c perly.h
    bison -d perly.y
    ln -s perly.tab.h perly.h
    ln -s perly.tab.c perly.c

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
    rm lib/warnings.pm warnings.h regnodes.h reentr.h reentr.c reentr.inc \
          overload.h overload.c opcode.h opnames.h pp_proto.h \
          pp.sym keywords.h embed.h embedvar.h global.sym perlapi.c perlapi.h \
          proto.h pod/perlintern.pod pod/perlapi.pod \
          pod/perlmodlib.pod ext/ByteLoader/byterun.{h,c} \
          ext/B/B/Asmdata.pm
    perl regen.pl

    # regenerate configure
    ln -s ../metaconfig*/.package .
    ln -s ../metaconfig*/U .
    metaconfig -m

    # Glossary
    pushd Porting
    ln -s /usr/lib/perl5/5.6.2/U .
    makegloss
    popd

    bash ext/Devel/PPPort/devel/mkapidoc.sh . \
        ext/Devel/PPPort/parts/apidoc.fnc \
        ext/Devel/PPPort/parts/embed.fnc
}

src_configure() {
    rm MANIFEST # make Configure script happy
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

src_compile() {
    pushd x2p
    make BYACC=yacc run_byacc
    popd

    make -j1 PREFIX="${PREFIX}"
}

src_install() {
    default

    # Remove messed up manpages
    rm "${DESTDIR}/"*.0
}
