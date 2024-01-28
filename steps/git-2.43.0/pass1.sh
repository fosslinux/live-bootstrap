src_configure() {
    LDFLAGS="-static" \
        LIBS="-lssl -lcrypto" \
        CURL_CONFIG_OPTS="--static-libs" \
        ./configure \
        --prefix="${PREFIX}" \
        --with-gitconfig=/etc/gitconfig \
        --with-python=python3 \
        --with-curl
}

src_compile() {
    make "${MAKEJOBS}"
}
