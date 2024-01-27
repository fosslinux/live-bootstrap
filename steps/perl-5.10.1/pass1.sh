# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default_src_prepare

    # Regenerate bison files
    # perly.c looks suspiciously like it is from bison, but is not; from the
    # below script:
    # Note that perly.c is *not* regenerated - this is now a static file which
    # is not dependent on perly.y any more.
    perl regen_perly.pl -b bison-2.3
    # Remove the source file so make works.
    rm -f perly.y

    # Regenerate other prebuilt header files
    # Taken from headers of regen scripts
    rm -f lib/warnings.pm warnings.h regnodes.h reentr.h reentr.c overload.h \
          overload.c lib/overload/numbers.pm opcode.h opnames.h pp_proto.h \
          pp.sym keywords.h embed.h embedvar.h global.sym perlapi.c perlapi.h \
          proto.h
    perl regen.pl

    mkdir -p ext/File ext/Digest ext/Data
    mv ext/File-Glob ext/File/Glob
    mv ext/Digest-SHA ext/Digest/SHA
    mv ext/Data-Dumper ext/Data/Dumper
}
