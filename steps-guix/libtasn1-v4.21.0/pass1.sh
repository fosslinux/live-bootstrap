# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    GNULIB_SRCDIR="$(realpath ../gnulib-e93789d)" \
    ./bootstrap --no-git
}

src_configure() {
    local host_triplet
    host_triplet="$(gcc -dumpmachine)"

    PATH="${PREFIX}/bin:/usr/bin:/bin" \
    ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --host="${host_triplet}" \
        --build="${host_triplet}"
}

src_compile() {
    default_src_compile
}

src_install() {
    default_src_install
}
