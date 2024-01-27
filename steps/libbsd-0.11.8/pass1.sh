src_prepare() {
    default
    autoreconf-2.71 -fi
}

src_configure() {
    ./configure --prefix="${PREFIX}" --libdir="${LIBDIR}"
}
