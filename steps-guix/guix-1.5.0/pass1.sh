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

probe_guile_module() {
    local module_name debug_log
    local guile_site_path guile_site_ccache guile_core_ccache guile_ext_path
    local pkg_config_path probe_label probe_pkg_config probe_expression
    local find_name find_module

    module_name="$1"
    debug_log="/tmp/${module_name}-guile-probe.log"
    pkg_config_path="${LIBDIR}/pkgconfig:${PREFIX}/lib/pkgconfig:${PREFIX}/share/pkgconfig"
    guile_site_path="${PREFIX}/share/guile/site/3.0:${PREFIX}/share/guile/3.0"
    guile_site_ccache="${LIBDIR}/guile/3.0/site-ccache"
    guile_core_ccache="${LIBDIR}/guile/3.0/ccache"
    guile_ext_path="${LIBDIR}/guile/3.0/extensions"

    case "${module_name}" in
        git)
            probe_label="git-related"
            probe_pkg_config="libgit2"
            find_name='libguile-git*'
            find_module='git'
            probe_expression="(use-modules (git)) (display \"git-module-ok\\n\")"
            ;;
        gcrypt)
            probe_label="gcrypt-related"
            probe_pkg_config="libgcrypt"
            find_name='*gcrypt*'
            find_module='gcrypt'
            probe_expression="(use-modules (gcrypt hash)) (unless (equal? (hash-algorithm sha256) (lookup-hash-algorithm 'sha256)) (error \"guile-gcrypt sha256 lookup mismatch\")) (display \"gcrypt-module-ok\\n\")"
            ;;
        gnutls)
            probe_label="gnutls-related"
            probe_pkg_config="gnutls"
            find_name='libguile-gnutls*'
            find_module='gnutls'
            probe_expression="(use-modules (gnutls)) (display \"gnutls-module-ok\\n\")"
            ;;
        *)
            probe_label="${module_name}-related"
            probe_pkg_config=""
            find_name="*${module_name}*"
            find_module="${module_name}"
            probe_expression="(use-modules (${module_name})) (display \"${module_name}-module-ok\\n\")"
            ;;
    esac

    rm -f "${debug_log}"

    echo "guix: probing (${module_name}) before configure" >&2
    echo "guix: GUILE_LOAD_PATH=${guile_site_path}" >&2
    echo "guix: GUILE_LOAD_COMPILED_PATH=${guile_site_ccache}:${guile_core_ccache}" >&2
    echo "guix: GUILE_EXTENSIONS_PATH=${guile_ext_path}" >&2
    if [ -n "${probe_pkg_config}" ]; then
        echo "guix: ${probe_pkg_config} pkg-config libs=$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" /usr/bin/pkg-config --libs "${probe_pkg_config}")" >&2
        echo "guix: ${probe_pkg_config} pkg-config static-libs=$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" /usr/bin/pkg-config --static --libs "${probe_pkg_config}")" >&2
    fi

    if command -v find >/dev/null 2>&1; then
        echo "guix: installed Guile ${probe_label} files:" >&2
        find "${PREFIX}/share/guile" "${LIBDIR}/guile" -type f \
            \( -path "*/${find_module}/*.scm" -o -path "*/${find_module}.scm" -o -path "*/${find_module}/*.go" -o -path "*/${find_module}.go" -o -name "${find_name}" \) \
            2>/dev/null | LC_ALL=C sort >&2 || true
    fi

    if ! PATH="${PREFIX}/bin:/usr/bin:/bin" \
        PKG_CONFIG="/usr/bin/pkg-config" \
        PKG_CONFIG_LIBDIR="${pkg_config_path}" \
        PKG_CONFIG_PATH="${pkg_config_path}" \
        LD_LIBRARY_PATH="${LIBDIR}:${PREFIX}/lib:${LD_LIBRARY_PATH}" \
        GUILE_LOAD_PATH="${guile_site_path}" \
        GUILE_LOAD_COMPILED_PATH="${guile_site_ccache}:${guile_core_ccache}" \
        GUILE_SYSTEM_PATH="${guile_site_path}" \
        GUILE_SYSTEM_COMPILED_PATH="${guile_site_ccache}:${guile_core_ccache}" \
        GUILE_EXTENSIONS_PATH="${guile_ext_path}" \
        GNUTLS_GUILE_EXTENSION_DIR="${guile_ext_path}" \
        "${PREFIX}/bin/guile" -c "${probe_expression}" \
        >"${debug_log}" 2>&1; then
        echo "guix: explicit (${module_name}) probe failed; raw Guile output follows:" >&2
        cat "${debug_log}" >&2 || true
        false
    fi
}

src_configure() {
    local host_triplet pkg_config_path guile_cflags guile_libs
    local guile_site_path guile_site_ccache guile_core_ccache guile_ext_path
    host_triplet="$(gcc -dumpmachine)"
    pkg_config_path="${LIBDIR}/pkgconfig:${PREFIX}/lib/pkgconfig:${PREFIX}/share/pkgconfig"
    guile_site_path="${PREFIX}/share/guile/site/3.0:${PREFIX}/share/guile/3.0"
    guile_site_ccache="${LIBDIR}/guile/3.0/site-ccache"
    guile_core_ccache="${LIBDIR}/guile/3.0/ccache"
    guile_ext_path="${LIBDIR}/guile/3.0/extensions"
    guile_cflags="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --cflags guile-3.0)"
    guile_libs="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --libs guile-3.0)"

    probe_guile_module gcrypt
    probe_guile_module gnutls
    probe_guile_module git

    PATH="${PREFIX}/bin:/usr/bin:/bin" \
    PKG_CONFIG="/usr/bin/pkg-config" \
    PKG_CONFIG_LIBDIR="${pkg_config_path}" \
    PKG_CONFIG_PATH="${pkg_config_path}" \
    LD_LIBRARY_PATH="${LIBDIR}:${PREFIX}/lib:${LD_LIBRARY_PATH}" \
    GUILE_LOAD_PATH="${guile_site_path}" \
    GUILE_LOAD_COMPILED_PATH="${guile_site_ccache}:${guile_core_ccache}" \
    GUILE_SYSTEM_PATH="${guile_site_path}" \
    GUILE_SYSTEM_COMPILED_PATH="${guile_site_ccache}:${guile_core_ccache}" \
    GUILE_EXTENSIONS_PATH="${guile_ext_path}" \
    GNUTLS_GUILE_EXTENSION_DIR="${guile_ext_path}" \
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
