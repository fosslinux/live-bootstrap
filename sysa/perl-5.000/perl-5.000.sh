# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_unpack() {
    default

    mv perl5-perl-5.000 perl-5.000
}

src_prepare() {
    default

    # Remove and regenerate bison files
    rm perly.c perly.h
    bison -d perly.y
    mv perly.tab.c perly.c
    mv perly.tab.h perly.h

    # Regenerate embed.h
    rm embed.h
    ./embed_h.SH

    # Regenerate keywords.h
    rm keywords.h
    chmod +x keywords.sh
    ./keywords.sh

    # Regenerate opcode.h
    rm opcode.h
    chmod +x opcode.sh
    ./opcode.sh
}
