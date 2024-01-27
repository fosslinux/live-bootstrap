src_configure() {
    ./configure --prefix="${PREFIX}" \
        --without-pam
}

src_compile() {
    make -f GNUmakefile "${MAKEJOBS}" PREFIX="${PREFIX}"
}

src_install() {
    make -f GNUmakefile install PREFIX="${PREFIX}" DESTDIR="${DESTDIR}"

    ln -s doas "${DESTDIR}${PREFIX}/bin/sudo"
}
