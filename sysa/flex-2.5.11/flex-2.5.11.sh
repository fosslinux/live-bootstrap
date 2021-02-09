# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# When we rebuild flex it no longer needs patching
# and can use simplified makefile
src_prepare() {
    default_src_prepare

    touch config.h
    rm parse.c parse.h scan.c skel.c
}

src_install() {
    if test -d /lex; then
        # Remove lex, later  make install will symlink lex to flex
        rm -rf /lex
        rm -f "${PREFIX}/bin/lex"
        rm -f "${PREFIX}/lib/libl.a"
    fi
    
    default_src_install
}
