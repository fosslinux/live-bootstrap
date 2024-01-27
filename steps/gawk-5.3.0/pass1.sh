src_prepare() {
    default

    # texinfo
    rm doc/*.info

    # bison
    rm awkgram.c command.c
}

src_configure() {
    ./configure --prefix="${PREFIX}"
}
