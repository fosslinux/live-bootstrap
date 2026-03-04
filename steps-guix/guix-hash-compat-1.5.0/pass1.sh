# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default
}

src_configure() {
    :
}

src_compile() {
    :
}

src_install() {
    local compat_src
    local compat_bin
    compat_src="${DESTDIR}/usr/libexec/guix-hash-compat/guix-1.5.0"
    compat_bin="${DESTDIR}/usr/bin/guix-hash-compat"

    mkdir -p "${compat_src}"
    cp -a . "${compat_src}/"
    mkdir -p "$(dirname "${compat_bin}")"

    cat > "${compat_bin}" <<'EOS'
#!/bin/sh
set -e

src_dir="/usr/libexec/guix-hash-compat/guix-1.5.0"
if [ -d /tmp/guix-1.5.0 ]; then
    src_dir="/tmp/guix-1.5.0"
fi

cd "${src_dir}"
exec ./pre-inst-env guix hash "$@"
EOS
    chmod 0755 "${compat_bin}"
}
