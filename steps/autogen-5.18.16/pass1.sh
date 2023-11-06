# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

noextract="autogen-5.18.16.tar.xz"

src_prepare() {
    mkdir build
    mv ../autogen-5.18.16 build/src
    mv ../autogen-5.18.16.tar.xz build/
    rm -f build/src/add-on/char-mapper/cm.tar
}

src_compile() {
(
    set -e
    declare -x PKG_CONFIG_PATH="${LIBDIR}/pkgconfig"
    sed -i "s/make install/make install DESTDIR=\${DESTDIR}/" bootstrap_tarball.sh
    sed -i "/make check/d" bootstrap_tarball.sh
    declare -x FINALPREFIX="${PREFIX}"
    declare -x GUILE_STATIC="--static"
    declare -x GNULIBDIR="${PWD}"/../gnulib-8f4538a5
    declare -x MAN_PAGE_DATE=1970-01-01

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
    CPPFLAGS=-D_LARGEFILE64_SOURCE=1 ./configure \
	--prefix="${FINALPREFIX}" \
	--libdir="${FINALPREFIX}/lib/i386-unknown-linux-musl" \
	--disable-shared \
	--enable-timeout=15
    touch doc/agdoc.texi # build later
    make -j1 CFLAGS=-Wno-error

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
