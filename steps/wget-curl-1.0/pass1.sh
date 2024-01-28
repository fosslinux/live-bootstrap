src_compile() {
    :
}

src_install() {
    install -m 755 -D wget "${DESTDIR}/usr/bin/wget"
}
