#!/bin/sh
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

. /steps/bootstrap.cfg
. /steps/env
. /steps-guix/improve/local-distfiles-http.sh

guix_localstate_dir="/var/guix"
daemon_socket="${guix_localstate_dir}/daemon-socket/socket"
channel_root="/var/lib/guix/local-channels"
channel_repo="${channel_root}/guix"
base_snapshot_dir="${channel_root}/guix-base-snapshot"
channel_work="/tmp/guix-local-channel-work"
local_guix_source_tarball="guix-1.5.0-local-source.tar.xz"
channels_file="/root/.config/guix/channels.scm"
guix_seed_helper="/steps-guix/improve/guix-1.5.0.sh"
guix_patch_dir="/steps-guix/guix-1.5.0/patches"
PATH="/usr/sbin:/sbin:${PATH}"
export GUIX_DAEMON_SOCKET="${daemon_socket}"
guile_site_path="${PREFIX}/share/guile/site/3.0:${PREFIX}/share/guile/3.0"
guile_site_ccache="${LIBDIR}/guile/3.0/site-ccache"
guile_core_ccache="${LIBDIR}/guile/3.0/ccache"
guile_ext_path="${LIBDIR}/guile/3.0/extensions"

# Keep Guile runtime lookup deterministic for both CLI and daemon-side Guile
# helpers.
export LD_LIBRARY_PATH="${LIBDIR}:${PREFIX}/lib:${LD_LIBRARY_PATH}"
export GUILE_LOAD_PATH="${guile_site_path}"
export GUILE_LOAD_COMPILED_PATH="${guile_site_ccache}:${guile_core_ccache}"
export GUILE_SYSTEM_PATH="${guile_site_path}"
export GUILE_SYSTEM_COMPILED_PATH="${guile_site_ccache}:${guile_core_ccache}"
export GUILE_EXTENSIONS_PATH="${guile_ext_path}"
export GNUTLS_GUILE_EXTENSION_DIR="${guile_ext_path}"

trap stop_distfiles_http_server EXIT INT TERM HUP

have_group() {
    if command -v getent >/dev/null 2>&1; then
        getent group "$1" >/dev/null 2>&1
    else
        grep -q "^$1:" /etc/group
    fi
}

have_user() {
    if command -v getent >/dev/null 2>&1; then
        getent passwd "$1" >/dev/null 2>&1
    else
        grep -q "^$1:" /etc/passwd
    fi
}

have_tty_device() {
    for dev in /dev/tty /dev/tty[0-9]* /dev/ttyS*; do
        if [ -c "${dev}" ]; then
            return 0
        fi
    done
    return 1
}

verify_terminal_devices() {
    mount | grep ' on /dev ' >/dev/null 2>&1 &&
    mount | grep ' on /dev/pts ' >/dev/null 2>&1 &&
    test -c /dev/ptmx &&
    test -c /dev/pts/ptmx &&
    have_tty_device
}

reset_tree_timestamps() {
    find "$1" -print0 | xargs -0 touch -h -t 197001010000.00
}

make_repro_source_tar_xz() {
    src_dir="$1"
    out_file="$2"
    tmp_tar="$(mktemp /tmp/guix-source-tarball.XXXXXX.tar)"

    mkdir -p "$(dirname "${out_file}")"

    reset_tree_timestamps "${src_dir}"
    (
        cd "$(dirname "${src_dir}")"
        tar --sort=name --hard-dereference \
            --numeric-owner --owner=0 --group=0 --mode=go=rX,u+rw \
            -cf "${tmp_tar}" "$(basename "${src_dir}")"
    )
    touch -t 197001010000.00 "${tmp_tar}"
    xz -T1 -c "${tmp_tar}" > "${out_file}"
    touch -t 197001010000.00 "${out_file}"
    rm -f "${tmp_tar}"
}

prepare_local_channel_checkout() {
    rendered_patch="/tmp/guix-bootstrap-local-seeds.patch"
    rendered_mes_patch="/tmp/guix-bootstrap-local-mes-extra.patch"
    self_source_patch_template="${guix_patch_dir}/use-local-self-source.patch.in"
    rendered_self_source_patch="/tmp/guix-use-local-self-source.patch"
    local_guix_source_path="${distfiles}/${local_guix_source_tarball}"
    local_guix_source_url="${distfiles_http_base_url}/${local_guix_source_tarball}"
    local_guix_source_hash=""
    static_patch=""
    channel_patch_link=""

    if [ ! -x "${guix_seed_helper}" ]; then
        echo "Missing Guix seed helper: ${guix_seed_helper}" >&2
        exit 1
    fi
    if [ ! -d "${guix_patch_dir}" ]; then
        echo "Missing Guix patch directory: ${guix_patch_dir}" >&2
        exit 1
    fi
    if [ ! -f "${self_source_patch_template}" ]; then
        echo "Missing Guix self-source patch template: ${self_source_patch_template}" >&2
        exit 1
    fi

    "${guix_seed_helper}"
    if [ ! -f /tmp/guix-bootstrap-seeds.env ]; then
        echo "Missing /tmp/guix-bootstrap-seeds.env" >&2
        exit 1
    fi
    . /tmp/guix-bootstrap-seeds.env

    sed \
        -e "s|@EXEC_BASH_HASH@|${EXEC_BASH_HASH}|g" \
        -e "s|@EXEC_MKDIR_HASH@|${EXEC_MKDIR_HASH}|g" \
        -e "s|@EXEC_TAR_HASH@|${EXEC_TAR_HASH}|g" \
        -e "s|@EXEC_XZ_HASH@|${EXEC_XZ_HASH}|g" \
        -e "s|@STATIC_BINARIES_SEED_HASH@|${STATIC_BINARIES_SEED_HASH}|g" \
        -e "s|@GUILE_I686_SEED_HASH@|${GUILE_I686_SEED_HASH}|g" \
        -e "s|@GUILE_X86_64_SEED_HASH@|${GUILE_X86_64_SEED_HASH}|g" \
        -e "s|@MES_MINIMAL_SEED_HASH@|${MES_MINIMAL_SEED_HASH}|g" \
        -e "s|@MESCC_TOOLS_SEED_HASH@|${MESCC_TOOLS_SEED_HASH}|g" \
        "${guix_patch_dir}/bootstrap-local-seeds.patch.in" > "${rendered_patch}"
    sed \
        -e "s|@MES_MINIMAL_SEED_HASH@|${MES_MINIMAL_SEED_HASH}|g" \
        -e "s|@MESCC_TOOLS_SEED_HASH@|${MESCC_TOOLS_SEED_HASH}|g" \
        "${guix_patch_dir}/bootstrap-local-mes-extra.patch.in" > "${rendered_mes_patch}"

    if grep -Eq '@[A-Z0-9_]+@' "${rendered_patch}" "${rendered_mes_patch}"; then
        echo "Unexpanded placeholder found while rendering Guix channel patches." >&2
        exit 1
    fi

    (
        channel_patch_link="$(dirname "${channel_repo}")/guix-1.5.0"
        trap 'rm -f "${channel_patch_link}"' EXIT HUP INT TERM

        ln -s "$(basename "${channel_repo}")" "${channel_patch_link}"
        cd "${channel_repo}"
        for static_patch in "${guix_patch_dir}"/*.patch; do
            patch -d.. -Np0 < "${static_patch}"
        done
        patch -p1 < "${rendered_patch}"
        patch -p1 < "${rendered_mes_patch}"
        rm -f "${channel_patch_link}"
        # TODO: Expand this cleanup to cover the full pregenerated-file set
        # before snapshotting the local channel checkout.
        rm -f doc/os-config*
        rm -f doc/*.??*.*
        rm -f doc/version*.texi
        rm -f doc/stamp*
        rm -f doc/*.1
        rm -f doc/*.info
        rm -f doc/guix.info*
        rm -f \
            doc/images/*.eps \
            doc/images/*.pdf \
            doc/images/bootstrap-*.png \
            doc/images/coreutils-bag-graph.png \
            doc/images/coreutils-graph.png \
            doc/images/gcc-core-mesboot0-graph.png \
            doc/images/service-graph.png \
            doc/images/shepherd-graph.png
        git init -q
        git add -A
        # Commit the bootstrap-patched tree first, then package that exact tree.
        # The follow-up self-source patch points the final channel snapshot at
        # this source tarball to avoid a self-reference cycle.
        git -c user.name='guix-local' -c user.email='guix-local@example.invalid' commit -q -m 'local guix channel base snapshot'

        rm -rf "${base_snapshot_dir}"
        mkdir -p "${base_snapshot_dir}"
        cp -a "${channel_repo}/." "${base_snapshot_dir}/"
        rm -rf "${base_snapshot_dir}/.git"
        make_repro_source_tar_xz "${base_snapshot_dir}" "${local_guix_source_path}"
        local_guix_source_hash="$(/usr/bin/guix-hash-compat "${local_guix_source_path}")"

        sed \
            -e "s|@LOCAL_GUIX_SOURCE_URL@|${local_guix_source_url}|g" \
            -e "s|@LOCAL_GUIX_SOURCE_HASH@|${local_guix_source_hash}|g" \
            "${self_source_patch_template}" > "${rendered_self_source_patch}"

        if grep -Eq '@[A-Z0-9_]+@' "${rendered_self_source_patch}"; then
            echo "Unexpanded placeholder found while rendering Guix self-source patch." >&2
            exit 1
        fi
        if ! grep -F "${local_guix_source_url}" "${rendered_self_source_patch}" >/dev/null 2>&1; then
            echo "Rendered Guix self-source patch does not point at the expected local source URL." >&2
            exit 1
        fi
        if ! grep -F "base32" "${rendered_self_source_patch}" >/dev/null 2>&1 ||
           ! grep -F "${local_guix_source_hash}" "${rendered_self_source_patch}" >/dev/null 2>&1; then
            echo "Rendered Guix self-source patch does not include the expected local source hash." >&2
            exit 1
        fi

        patch -p1 < "${rendered_self_source_patch}"
        if ! grep -F "uri \"${local_guix_source_url}\"" gnu/packages/package-management.scm >/dev/null 2>&1 ||
           ! grep -F "${local_guix_source_hash}" gnu/packages/package-management.scm >/dev/null 2>&1; then
            echo "Applied Guix self-source patch did not update package-management.scm to the expected local source." >&2
            exit 1
        fi
        git add -A
        git -c user.name='guix-local' -c user.email='guix-local@example.invalid' commit -q -m 'local guix channel snapshot'
    )
}

mkdir -p /proc /sys /dev "${guix_localstate_dir}/daemon-socket" /var/lib/guix /root/.config/guix
mount | grep ' on /proc ' >/dev/null 2>&1 || mount -t proc proc /proc
mount | grep ' on /sys ' >/dev/null 2>&1 || mount -t sysfs sysfs /sys
mount | grep ' on /dev ' >/dev/null 2>&1 || mount -t devtmpfs devtmpfs /dev
if ! mount | grep ' on /dev/pts ' >/dev/null 2>&1; then
    mkdir -p /dev/pts
    mount -t devpts devpts /dev/pts
fi
test -c /dev/tty || mknod -m 666 /dev/tty c 5 0
test -c /dev/ptmx || mknod -m 666 /dev/ptmx c 5 2
test -c /dev/tty0 || mknod -m 666 /dev/tty0 c 4 0
test -c /dev/tty1 || mknod -m 666 /dev/tty1 c 4 1
test -c /dev/tty2 || mknod -m 666 /dev/tty2 c 4 2
test -c /dev/ttyS0 || mknod -m 666 /dev/ttyS0 c 4 64

if ! verify_terminal_devices; then
    echo "Missing terminal devices required for Guix: /dev/ptmx, /dev/pts/ptmx, or /dev/tty*" >&2
    exit 1
fi

if ! have_group guixbuild; then
    groupadd --system guixbuild
fi

nologin_bin="$(command -v nologin)"

i=1
while [ "${i}" -le 10 ]; do
    idp="$(printf '%02d' "${i}")"
    user="guixbuilder${idp}"
    if ! have_user "${user}"; then
        useradd -g guixbuild -G guixbuild \
            -d /var/empty -s "${nologin_bin}" \
            -c "Guix build user ${idp}" --system \
            "${user}"
    fi
    i=$((i + 1))
done

if [ ! -S "${daemon_socket}" ]; then
    guix-daemon \
        --build-users-group=guixbuild \
        --no-substitutes \
        --listen="${daemon_socket}" \
        >/tmp/guix-daemon.log 2>&1 &
fi

retry=0
while [ "${retry}" -lt 60 ]; do
    if [ -S "${daemon_socket}" ]; then
        break
    fi
    retry=$((retry + 1))
    sleep 1
done

if [ ! -S "${daemon_socket}" ]; then
    echo "guix-daemon did not become ready: ${daemon_socket}" >&2
    exit 1
fi

src_tar=""
for f in "${distfiles}"/guix-1.5.0*.tar.* "${distfiles}"/guix-v1.5.0*.tar.*; do
    if [ -f "${f}" ]; then
        src_tar="${f}"
        break
    fi
done

if [ -z "${src_tar}" ]; then
    echo "Could not find Guix 1.5.0 source tarball in ${distfiles}" >&2
    exit 1
fi

rm -rf "${channel_work}" "${channel_repo}" "${base_snapshot_dir}"
mkdir -p "${channel_work}" "${channel_root}"

case "${src_tar}" in
    *.tar.gz|*.tgz) tar -C "${channel_work}" -xzf "${src_tar}" ;;
    *.tar.xz) tar -C "${channel_work}" -xJf "${src_tar}" ;;
    *.tar.bz2) tar -C "${channel_work}" -xjf "${src_tar}" ;;
    *.tar) tar -C "${channel_work}" -xf "${src_tar}" ;;
    *)
        echo "Unsupported tarball format: ${src_tar}" >&2
        exit 1
        ;;
esac

src_dir="$(find "${channel_work}" -mindepth 1 -maxdepth 1 -type d | head -n 1)"
if [ -z "${src_dir}" ]; then
    echo "Failed to unpack Guix source from ${src_tar}" >&2
    exit 1
fi

mv "${src_dir}" "${channel_repo}"

prepare_local_channel_checkout

channel_commit="$(git -C "${channel_repo}" rev-parse HEAD)"
channel_branch="$(git -C "${channel_repo}" symbolic-ref --quiet --short HEAD)"
if [ -z "${channel_branch}" ]; then
    echo "Failed to determine local Guix channel branch." >&2
    exit 1
fi

cat > "${channels_file}" <<EOF
(use-modules (guix channels))
(list
 (channel
  (name 'guix)
  (url "file://${channel_repo}")
  (branch "${channel_branch}")
  (commit "${channel_commit}")))
EOF

chmod 0644 "${channels_file}"

if ! guile -c '(use-modules (gnutls)) (if (module-variable (resolve-module (quote (gnutls))) (quote make-session)) (exit 0) (exit 1))'; then
    echo "Guile GnuTLS bindings are incomplete: (gnutls) is missing make-session." >&2
    exit 1
fi

start_distfiles_http_server
guix pull --system=x86_64-linux --bootstrap -c "${JOBS}" --no-substitutes --channels="${channels_file}" --disable-authentication
