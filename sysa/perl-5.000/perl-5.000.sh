# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # Remove and regenerate bison files
    rm -f perly.c perly.h
    bison -d perly.y
    mv perly.tab.c perly.c
    mv perly.tab.h perly.h

    # Regenerate embed.h
    rm -f embed.h
    ./embed_h.SH

    # Regenerate keywords.h
    rm -f keywords.h
    chmod +x keywords.sh
    ./keywords.sh

    # Regenerate opcode.h
    rm -f opcode.h
    chmod +x opcode.sh
    ./opcode.sh
}
