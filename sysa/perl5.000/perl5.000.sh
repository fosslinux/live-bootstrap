# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default_src_prepare

    rm perly.c perly.h
    bison -d perly.y
    mv perly.tab.c perly.c
    mv perly.tab.h perly.h

    rm embed.h
    ./embed_h.SH

    rm keywords.h
    chmod +x keywords.sh
    ./keywords.sh
 
    rm opcode.h
    chmod +x opcode.sh
    ./opcode.sh
}
