#!/bin/sh
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

. /steps/bootstrap.cfg
. /steps/env

guix_localstate_dir="/var/guix"
daemon_socket="${guix_localstate_dir}/daemon-socket/socket"
channel_root="/var/lib/guix/local-channels"
channel_repo="${channel_root}/guix"
channel_work="/tmp/guix-local-channel-work"
channels_file="/root/.config/guix/channels.scm"
distfiles="${DISTFILES:-/external/distfiles}"
guix_seed_helper="/steps-guix/improve/guix-1.5.0.sh"
guix_patch_dir="/steps-guix/guix-1.5.0/patches"
PATH="/usr/sbin:/sbin:${PATH}"
export GUIX_DAEMON_SOCKET="${daemon_socket}"

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

prepare_local_channel_checkout() {
    rendered_patch="/tmp/guix-bootstrap-local-seeds.patch"
    rendered_mes_patch="/tmp/guix-bootstrap-local-mes-extra.patch"

    if [ ! -x "${guix_seed_helper}" ]; then
        echo "Missing Guix seed helper: ${guix_seed_helper}" >&2
        exit 1
    fi
    if [ ! -d "${guix_patch_dir}" ]; then
        echo "Missing Guix patch directory: ${guix_patch_dir}" >&2
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
        -e "s|@GUILE_SEED_HASH@|${GUILE_SEED_HASH}|g" \
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
        cd "${channel_repo}"
        patch --dry-run -p1 < "${guix_patch_dir}/enforce-local-bootstrap-binaries-except-linux-headers.patch"
        patch -p1 < "${guix_patch_dir}/enforce-local-bootstrap-binaries-except-linux-headers.patch"
        patch --dry-run -p1 < "${rendered_patch}"
        patch -p1 < "${rendered_patch}"
        patch --dry-run -p1 < "${rendered_mes_patch}"
        patch -p1 < "${rendered_mes_patch}"
        git init -q
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

nologin_bin="$(command -v nologin || true)"
if [ -z "${nologin_bin}" ]; then
    if [ -x /usr/sbin/nologin ]; then
        nologin_bin=/usr/sbin/nologin
    elif [ -x /sbin/nologin ]; then
        nologin_bin=/sbin/nologin
    else
        echo "Could not find nologin binary." >&2
        exit 1
    fi
fi

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

rm -rf "${channel_work}" "${channel_repo}"
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

guix pull --bootstrap --no-substitutes --channels="${channels_file}" --disable-authentication
