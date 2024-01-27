src_prepare() {
    default
    autoreconf-2.71 -fi

    . ../../import-gnulib.sh
}

src_configure() {
    ./configure --prefix="${PREFIX}"
}
