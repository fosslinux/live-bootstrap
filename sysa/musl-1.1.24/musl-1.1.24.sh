src_prepare() {
    default_src_prepare

    # tcc does not support complex types
    rm -rf src/complex

    # Configure fails without this
    mkdir -p /dev
}

src_configure() {
    CC=tcc ./configure \
      --host=i386 \
      --disable-shared \
      --prefix=/after \
      --libdir=/after/lib/musl/ \
      --includedir=/after/include/musl
}

src_compile() {
    make CROSS_COMPILE= AR="tcc -ar" RANLIB=true CFLAGS="-DSYSCALL_NO_TLS"
}
