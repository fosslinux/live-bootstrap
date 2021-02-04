src_prepare() {
    default_src_prepare

    mv Makefile src/
    touch src/config.h
    rm src/parse.c src/parse.h src/scan.c src/skel.c
}

src_compile() {
    cd src
    default_src_compile
    cd ..
}

src_install() {
    cd src
    default_src_install
    cd ..
}
