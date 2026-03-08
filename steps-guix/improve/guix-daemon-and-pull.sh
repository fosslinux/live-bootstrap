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

mkdir -p /proc /sys /dev "${guix_localstate_dir}/daemon-socket" /var/lib/guix /root/.config/guix
mount | grep ' on /proc ' >/dev/null 2>&1 || mount -t proc proc /proc
mount | grep ' on /sys ' >/dev/null 2>&1 || mount -t sysfs sysfs /sys
mount | grep ' on /dev ' >/dev/null 2>&1 || mount -t devtmpfs devtmpfs /dev

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

(
    cd "${channel_repo}"
    git init -q
    git add -A
    git -c user.name='guix-local' -c user.email='guix-local@example.invalid' commit -q -m 'local guix channel snapshot'
)

channel_commit="$(git -C "${channel_repo}" rev-parse HEAD)"

cat > "${channels_file}" <<EOF
(use-modules (guix channels))
(list
 (channel
  (name 'guix)
  (url "file://${channel_repo}")
  (branch "master")
  (commit "${channel_commit}")))
EOF

chmod 0644 "${channels_file}"

guix pull --bootstrap --no-substitutes
