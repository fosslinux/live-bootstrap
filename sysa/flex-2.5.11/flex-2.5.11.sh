# When we rebuild flex it no longer needs patching
# and can use simplified makefile
src_prepare() {
    if test -d /lex; then
        default_src_prepare
    else
        cp ../../mk/main2.mk Makefile
    fi
    touch config.h
    rm parse.c parse.h scan.c
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
