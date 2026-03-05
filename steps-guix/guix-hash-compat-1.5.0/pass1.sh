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
    local compat_config
    local src_system
    compat_src="${DESTDIR}/usr/libexec/guix-hash-compat/guix-1.5.0"
    compat_bin="${DESTDIR}/usr/bin/guix-hash-compat"
    compat_config="${compat_src}/guix/config.scm"
    src_system="$(uname -m)-linux"
    case "${src_system}" in
        x86_64-linux|i686-linux)
            ;;
        *)
            src_system="x86_64-linux"
            ;;
    esac

    mkdir -p "${compat_src}"
    cp -a . "${compat_src}/"
    mkdir -p "$(dirname "${compat_bin}")"
    sed \
        -e 's|@PACKAGE_NAME@|guix|g' \
        -e 's|@PACKAGE_VERSION@|1.5.0|g' \
        -e 's|@PACKAGE_BUGREPORT@|bug-guix@gnu.org|g' \
        -e 's|@PACKAGE_URL@|https://guix.gnu.org|g' \
        -e 's|@GUIX_CHANNEL_URL@|#f|g' \
        -e 's|@GUIX_CHANNEL_COMMIT@|#f|g' \
        -e 's|@GUIX_CHANNEL_INTRODUCTION@|#f|g' \
        -e 's|@storedir@|/gnu/store|g' \
        -e 's|@guix_localstatedir@|/var|g' \
        -e 's|@guix_sysconfdir@|/etc|g' \
        -e "s|@guix_system@|${src_system}|g" \
        -e 's|@GIT@|/usr/bin/git|g' \
        -e 's|@GZIP@|/usr/bin/gzip|g' \
        -e 's|@BZIP2@|/usr/bin/bzip2|g' \
        -e 's|@XZ@|/usr/bin/xz|g' \
        "${compat_src}/guix/config.scm.in" > "${compat_config}"

    cat > "${compat_bin}" <<'EOS'
#!/bin/sh
set -e

src_dir="/usr/libexec/guix-hash-compat/guix-1.5.0"

export GUIX_UNINSTALLED=1
export GUILE_LOAD_PATH="${src_dir}${GUILE_LOAD_PATH:+:}${GUILE_LOAD_PATH}"
export GUILE_LOAD_COMPILED_PATH="${src_dir}${GUILE_LOAD_COMPILED_PATH:+:}${GUILE_LOAD_COMPILED_PATH}"
exec /usr/bin/guile --no-auto-compile \
    -L "${src_dir}" \
    -C "${src_dir}" \
    -c '(use-modules (guix scripts hash)) (apply guix-hash (cdr (command-line)))' \
    guix-hash-compat "$@"
EOS
    chmod 0755 "${compat_bin}"
}
