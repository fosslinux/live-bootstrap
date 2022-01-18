# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=34688d9b7ea888ea78e4bf38ad0de6a24e496fb0c1590f7c3c682b45e285d310

src_prepare() {
    default

    # Regenerate bison files
    # perly.c looks suspiciously like it is from bison, but is not; from the
    # below script:
    # Note that perly.c is *not* regenerated - this is now a static file which
    # is not dependent on perly.y any more.
    perl regen_perly.pl

    # Regenerate other prebuilt header files
    # Taken from headers of regen scripts
    rm -f embed.h embedvar.h perlapi.c perlapi.h proto.h mg_names.inc mg_raw.h \
          mg_vtable.h opcode.h opnames.h pp_proto.h \
          lib/B/Op_private.pm overload.h overload.inc lib/overload/numbers.pm \
          reentr.h reentr.c regnodes.h lib/warnings.pm \
          warnings.h lib/feature.pm feature.h
    perl regen.pl

    # Regenerate configure + config_h.SH
    rm -f Configure config_h.SH
    ln -s ../perl-5f2dc80/regen-configure/.package .
    ln -s ../perl-5f2dc80/regen-configure/U .
    metaconfig -m
}

src_configure() {
    ./Configure -des \
        -Dprefix="${PREFIX}" \
        -Dcc=gcc \
        -Dusedl=false \
        -Ddate=':' \
        -Dccflags="-U__DATE__ -U__TIME__" \
        -Darchname="i386-linux"
}

src_install() {
    # Remove old perl
    rm -rf "${PREFIX}"/lib/perl5/

    default
}
