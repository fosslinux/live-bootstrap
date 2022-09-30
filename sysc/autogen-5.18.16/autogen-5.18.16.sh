# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

urls="https://mirrors.kernel.org/gnu/autogen/rel5.18.16/autogen-5.18.16.tar.xz
 https://git.savannah.gnu.org/cgit/autogen.git/snapshot/autogen-5.18.16.tar.gz
 https://github.com/schierlm/gnu-autogen-bootstrapping/archive/refs/tags/autogen-5.18.16-v1.0.tar.gz
 http://git.savannah.gnu.org/cgit/gnulib.git/snapshot/gnulib-8f4538a5.tar.gz"

src_unpack() {
    tar --no-same-owner -xf "${DISTFILES}/autogen-5.18.16-v1.0.tar.gz"
    tar --no-same-owner -xf "${DISTFILES}/gnulib-8f4538a5.tar.gz"
    local build_dir=gnu-autogen-bootstrapping-autogen-5.18.16-v1.0/build
    mkdir -p "${build_dir}"
    cp "${DISTFILES}/autogen-5.18.16.tar.xz" "${build_dir}"
    cp "${DISTFILES}/autogen-5.18.16.tar.gz" "${build_dir}"
    pushd "${build_dir}"
    tar --no-same-owner -xf autogen-5.18.16.tar.gz
    mv autogen-5.18.16 src
    rm -f src/add-on/char-mapper/cm.tar
    popd
}

src_prepare() {
    :
}

src_compile() {
(
    set -e
    export PKG_CONFIG_PATH="${PREFIX}/lib/musl/pkgconfig"
    sed -i "s/make install/make install DESTDIR=\${DESTDIR}/" bootstrap_tarball.sh
    sed -i "/make check/d" bootstrap_tarball.sh
    export FINALPREFIX="${PREFIX}"
    export GUILE_STATIC="--static"
    export GNULIBDIR="${PWD}"/../gnulib-8f4538a5
    export MAN_PAGE_DATE=1970-01-01

    SKIP_MAIN=1 . ./bootstrap_tarball.sh
    prepare_tarball
    bootstrap_columns
    bootstrap_getdefs
    bootstrap_autogen
    bootstrap_tpl_config

    # Build stage2 autogen (stage1 autogen is not easy to install into /usr)
    rm -R build/tarball
    cp -ar build/autogen-5.18.16 build/tarball
    cd build/tarball

    # These files does not respect MAN_PAGE_DATE
    sed -i "s/+%Y/+1970/; s/%m/01/; s/%d'/01'/; s/%Y/2018/" autoopts/aoconf.tpl
    sed -i 's/%Y/2018/' autoopts/options_h.tpl

    SOURCE_DIR="$PWD" ./config/bootstrap
    # Specify timeout to avoid non-reproducibility
    ./configure \
	--prefix="${FINALPREFIX}" \
	--libdir="${FINALPREFIX}/lib/musl" \
	--enable-timeout=15
    touch doc/agdoc.texi # build later
    make CFLAGS=-Wno-error

    # Fix non-reproducible man-page
    awk '{gsub("\\(/tmp/.*", "", $7); print}' agen5/autogen.1 > autogen.1
    mv autogen.1 agen5/autogen.1

    cd ../..
)
}

src_install() {
    cd build/tarball
    make DESTDIR="${DESTDIR}" install MAN_PAGE_DATE=1970-01-01

    # Some reproducibility issues and generally not useful for bootstrapping
    rm "${DESTDIR}${PREFIX}/share/autogen/libopts-"*.tar.gz

    cd ../..
}
