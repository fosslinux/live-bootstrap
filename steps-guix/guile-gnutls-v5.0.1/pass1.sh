# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default
    autoreconf -fi
}

src_configure() {
    local host_triplet pkg_config_path guile_cflags guile_static_libs gnutls_static_libs
    host_triplet="$(gcc -dumpmachine)"
    pkg_config_path="${LIBDIR}/pkgconfig:${PREFIX}/lib/pkgconfig:${PREFIX}/share/pkgconfig"
    guile_cflags="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --cflags guile-3.0)"
    guile_static_libs="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --static --libs guile-3.0)"
    gnutls_static_libs="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --static --libs gnutls)"

    PATH="${PREFIX}/bin:/usr/bin:/bin" \
    PKG_CONFIG="/usr/bin/pkg-config" \
    PKG_CONFIG_LIBDIR="${pkg_config_path}" \
    PKG_CONFIG_PATH="${pkg_config_path}" \
    CFLAGS="-fPIC" \
    CXXFLAGS="-fPIC" \
    LD_LIBRARY_PATH="${LIBDIR}:${PREFIX}/lib:${LD_LIBRARY_PATH}" \
    GUILE_CFLAGS="${guile_cflags}" \
    GUILE_LIBS="${guile_static_libs}" \
    GNUTLS_LIBS="${gnutls_static_libs}" \
    ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --host="${host_triplet}" \
        --build="${host_triplet}" \
        --enable-static \
        --disable-shared \
        '--with-guile-site-dir=$(datarootdir)/guile/site/$(GUILE_EFFECTIVE_VERSION)' \
        '--with-guile-site-ccache-dir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache' \
        '--with-guile-extension-dir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/extensions'
}

src_compile() {
    local pkg_config_path guile_cflags guile_static_libs gnutls_cflags gnutls_static_libs
    local main_c

    pkg_config_path="${LIBDIR}/pkgconfig:${PREFIX}/lib/pkgconfig:${PREFIX}/share/pkgconfig"
    guile_cflags="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --cflags guile-3.0)"
    guile_static_libs="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --static --libs guile-3.0)"
    gnutls_cflags="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --cflags gnutls)"
    gnutls_static_libs="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --static --libs gnutls)"

    if [ -z "${guile_cflags}" ]; then
        echo "guile-gnutls: pkg-config returned empty cflags for guile-3.0" >&2
        false
    fi
    make "${MAKEJOBS}" -C guile/src \
        CPPFLAGS="${guile_cflags} ${gnutls_cflags} ${CPPFLAGS:-}" \
        GUILE_CFLAGS="${guile_cflags}" \
        GUILE_LDFLAGS="${guile_static_libs}" \
        GNUTLS_CFLAGS="${gnutls_cflags}" \
        GNUTLS_LIBS="${gnutls_static_libs}" \
        guile-gnutls-v-2.la
    make "${MAKEJOBS}" -C guile modules/gnutls.scm GNUTLS_GUILE_CROSS_COMPILING=yes

    mkdir -p static
    ar rcs static/libguile-gnutls-static.a \
        guile/src/.libs/core.o \
        guile/src/.libs/errors.o \
        guile/src/.libs/utils.o

    main_c="static/guile-static-main.c"
    cat > "${main_c}" <<'EOF_C'
#include <libguile.h>

void scm_init_gnutls(void);

static void
inner_main(void *closure, int argc, char **argv)
{
  (void) closure;
  scm_init_gnutls();
  scm_shell(argc, argv);
}

int
main(int argc, char **argv)
{
  scm_boot_guile(argc, argv, inner_main, NULL);
  return 0;
}
EOF_C

    gcc -O2 -static ${guile_cflags} \
        -o static/guile \
        "${main_c}" \
        static/libguile-gnutls-static.a \
        ${guile_static_libs} \
        ${gnutls_static_libs}
}

src_install() {
    install -Dm755 static/guile "${DESTDIR}${PREFIX}/bin/guile"
    install -Dm644 guile/modules/gnutls.scm \
        "${DESTDIR}${PREFIX}/share/guile/site/3.0/gnutls.scm"
    install -Dm644 guile/modules/gnutls/extra.scm \
        "${DESTDIR}${PREFIX}/share/guile/site/3.0/gnutls/extra.scm"
    install -Dm644 static/libguile-gnutls-static.a \
        "${DESTDIR}${LIBDIR}/libguile-gnutls-static.a"
}

src_postprocess() {
    local guile_site_path guile_site_ccache guile_core_ccache

    default_src_postprocess

    if find "${DESTDIR}" -type f \( -name '*.so' -o -name '*.so.*' \) | grep -q .; then
        echo "guile-gnutls: shared objects are forbidden in static profile." >&2
        false
    fi

    guile_site_path="${DESTDIR}${PREFIX}/share/guile/site/3.0:${PREFIX}/share/guile/site/3.0:${PREFIX}/share/guile/3.0"
    guile_site_ccache="${DESTDIR}${LIBDIR}/guile/3.0/site-ccache:${LIBDIR}/guile/3.0/site-ccache"
    guile_core_ccache="${LIBDIR}/guile/3.0/ccache"

    PATH="${PREFIX}/bin:/usr/bin:/bin" \
    GUILE_LOAD_PATH="${guile_site_path}" \
    GUILE_LOAD_COMPILED_PATH="${guile_site_ccache}:${guile_core_ccache}" \
    GUILE_SYSTEM_PATH="${guile_site_path}" \
    GUILE_SYSTEM_COMPILED_PATH="${guile_site_ccache}:${guile_core_ccache}" \
    "${DESTDIR}${PREFIX}/bin/guile" -c '(use-modules (gnutls)) (display "gnutls-module-ok\n")'
}
