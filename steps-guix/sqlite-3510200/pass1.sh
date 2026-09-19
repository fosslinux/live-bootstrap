# SPDX-License-Identifier: GPL-3.0-or-later

src_unpack() {
    local url fname

    url="$(awk 'NR==1 { print $2 }' ../sources)"
    fname="$(awk 'NR==1 { print $4 }' ../sources)"

    if [ -z "${fname}" ]; then
        fname="$(basename "${url}")"
    fi

    cp "${DISTFILES}/${fname}" .
    unzip -q "${fname}"

    dirname="${fname%.zip}"
}

src_prepare() {
    default
}

src_configure() {
    local host_triplet
    host_triplet="$(gcc -dumpmachine)"

    PATH="${PREFIX}/bin:/usr/bin:/bin" \
    ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --host="${host_triplet}" \
        --build="${host_triplet}" \
        --disable-tcl
}

src_compile() {
    default_src_compile
}

src_install() {
    default_src_install
}
