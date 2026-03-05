# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    local bootstrap_scm patch_template rendered_patch
    local mes_patch_template rendered_mes_patch

    default

    # Merge improve:guix-1.5.0 into build:guix-1.5.0 so manifest order cannot
    # accidentally influence pass number selection.
    "${SRCDIR}/improve/guix-1.5.0.sh"

    if [ ! -f /tmp/guix-bootstrap-seeds.env ]; then
        echo "Missing /tmp/guix-bootstrap-seeds.env" >&2
        false
    fi
    . /tmp/guix-bootstrap-seeds.env

    bootstrap_scm="gnu/packages/bootstrap.scm"
    patch_template="${base_dir}/patches/bootstrap-local-seeds.patch.in"
    rendered_patch="/tmp/guix-bootstrap-local-seeds.patch"
    mes_patch_template="${base_dir}/patches/bootstrap-local-mes-extra.patch.in"
    rendered_mes_patch="/tmp/guix-bootstrap-local-mes-extra.patch"

    if [ ! -f "${patch_template}" ]; then
        echo "Missing patch template: ${patch_template}" >&2
        false
    fi
    if [ ! -f "${mes_patch_template}" ]; then
        echo "Missing patch template: ${mes_patch_template}" >&2
        false
    fi

    sed \
        -e "s|@EXEC_BASH_HASH@|${EXEC_BASH_HASH}|g" \
        -e "s|@EXEC_MKDIR_HASH@|${EXEC_MKDIR_HASH}|g" \
        -e "s|@EXEC_TAR_HASH@|${EXEC_TAR_HASH}|g" \
        -e "s|@EXEC_XZ_HASH@|${EXEC_XZ_HASH}|g" \
        -e "s|@STATIC_BINARIES_SEED_HASH@|${STATIC_BINARIES_SEED_HASH}|g" \
        -e "s|@GUILE_SEED_HASH@|${GUILE_SEED_HASH}|g" \
        -e "s|@MES_MINIMAL_SEED_HASH@|${MES_MINIMAL_SEED_HASH}|g" \
        -e "s|@MESCC_TOOLS_SEED_HASH@|${MESCC_TOOLS_SEED_HASH}|g" \
        "${patch_template}" > "${rendered_patch}"
    sed \
        -e "s|@MES_MINIMAL_SEED_HASH@|${MES_MINIMAL_SEED_HASH}|g" \
        -e "s|@MESCC_TOOLS_SEED_HASH@|${MESCC_TOOLS_SEED_HASH}|g" \
        "${mes_patch_template}" > "${rendered_mes_patch}"

    if grep -Eq '@[A-Z0-9_]+@' "${rendered_patch}"; then
        echo "Unexpanded placeholder found in ${rendered_patch}" >&2
        false
    fi
    if grep -Eq '@[A-Z0-9_]+@' "${rendered_mes_patch}"; then
        echo "Unexpanded placeholder found in ${rendered_mes_patch}" >&2
        false
    fi
    patch --dry-run -p1 < "${rendered_patch}"
    patch -p1 < "${rendered_patch}"
    patch --dry-run -p1 < "${rendered_mes_patch}"
    patch -p1 < "${rendered_mes_patch}"

    grep -q 'file:///external/distfiles/' "${bootstrap_scm}"
    grep -q "${EXEC_BASH_HASH}" "${bootstrap_scm}"
    grep -q "${STATIC_BINARIES_SEED_HASH}" "${bootstrap_scm}"
    grep -q "${GUILE_SEED_HASH}" "${bootstrap_scm}"
    grep -q "${MES_MINIMAL_SEED_HASH}" "${bootstrap_scm}"
    grep -q "${MESCC_TOOLS_SEED_HASH}" "${bootstrap_scm}"
    grep -q "All bootstrap binaries must come from local, reproducible distfiles." "${bootstrap_scm}"
    grep -q "%bootstrap-linux-headers-base-urls" "${bootstrap_scm}"
    grep -q '^(define (bootstrap-mes-minimal-origin system)' "${bootstrap_scm}"
    grep -q '^(define %bootstrap-mescc-tools' "${bootstrap_scm}"
    grep -q "mes-minimal-stripped-0.19-i686-linux.tar.xz" "${bootstrap_scm}"
    grep -q "mescc-tools-static-stripped-0.5.2-i686-linux.tar.xz" "${bootstrap_scm}"
    grep -q '("mes-minimal" ,%bootstrap-mes-minimal)' "${bootstrap_scm}"
    grep -q '("mescc-tools" ,%bootstrap-mescc-tools)' "${bootstrap_scm}"
    grep -q "Offline bootstrap environment: require explicit channels." guix/channels.scm
}

src_configure() {
    local host_triplet pkg_config_path guile_cflags guile_libs
    host_triplet="$(gcc -dumpmachine)"
    pkg_config_path="${LIBDIR}/pkgconfig:${PREFIX}/lib/pkgconfig:${PREFIX}/share/pkgconfig"
    guile_cflags="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --cflags guile-3.0)"
    guile_libs="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --libs guile-3.0)"

    PKG_CONFIG="/usr/bin/pkg-config" \
    PKG_CONFIG_LIBDIR="${pkg_config_path}" \
    PKG_CONFIG_PATH="${pkg_config_path}" \
    GUILE_CFLAGS="${guile_cflags}" \
    GUILE_LIBS="${guile_libs}" \
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
