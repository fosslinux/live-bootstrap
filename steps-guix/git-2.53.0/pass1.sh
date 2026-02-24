# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default
}

src_configure() {
    local host_triplet curl_static_libs
    host_triplet="$(gcc -dumpmachine)"
    curl_static_libs="-lcurl -lssl -lcrypto -lz -pthread"

    PATH="${PREFIX}/bin:/usr/bin:/bin" \
    LIBS="${curl_static_libs}" \
    ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --build="${host_triplet}" \
        --host="${host_triplet}"
}

src_compile() {
    local curl_static_libs
    curl_static_libs="-lcurl -lssl -lcrypto -lz -pthread"

    make "${MAKEJOBS}" \
        NO_GETTEXT=YesPlease \
        NO_TCLTK=YesPlease \
        NO_PERL=YesPlease \
        NO_PYTHON=YesPlease \
        CURL_LDFLAGS="${curl_static_libs}"
}

src_install() {
    local curl_static_libs
    curl_static_libs="-lcurl -lssl -lcrypto -lz -pthread"

    make install \
        NO_GETTEXT=YesPlease \
        NO_TCLTK=YesPlease \
        NO_PERL=YesPlease \
        NO_PYTHON=YesPlease \
        CURL_LDFLAGS="${curl_static_libs}" \
        DESTDIR="${DESTDIR}" \
        prefix="${PREFIX}" \
        libdir="${LIBDIR}"
}
