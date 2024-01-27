src_prepare() {
    default
    find man -name "*.[1-9]" -delete

    # groups is provided by coreutils
    sed -i 's/groups$(EXEEXT) //' src/Makefile.in

    autoreconf-2.71 -fi
}

src_configure() {
    PKG_CONFIG_PATH="${LIBDIR}/pkgconfig/" \
    ./configure \
        --prefix="${PREFIX}" \
        --sbindir="${PREFIX}/bin" \
        --libdir="${LIBDIR}" \
        --sysconfdir=/etc \
        --localstatedir=/var
}

src_compile() {
    make "${MAKEJOBS}" PREFIX="${PREFIX}"
}

src_install() {
    make install PREFIX="${PREFIX}" DESTDIR="${DESTDIR}" exec_prefix="${PREFIX}"

    # fix sbin
    mv "${DESTDIR}"/usr/sbin/* "${DESTDIR}"/usr/bin/
    rmdir "${DESTDIR}/usr/sbin"
}
