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
    compat_src="${DESTDIR}/usr/libexec/guix-hash-compat/guix-1.5.0"

    mkdir -p "${compat_src}"
    cp -a . "${compat_src}/"

    install -D -m 0755 /dev/stdin "${DESTDIR}/usr/bin/guix-hash-compat" <<'EOS'
#!/bin/sh
set -e

src_dir="/usr/libexec/guix-hash-compat/guix-1.5.0"
if [ -d /tmp/guix-1.5.0 ]; then
    src_dir="/tmp/guix-1.5.0"
fi

cd "${src_dir}"
exec ./pre-inst-env guix hash "$@"
EOS
}
