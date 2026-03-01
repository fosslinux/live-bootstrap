#!/bin/sh
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

dist="${DISTFILES:-/external/distfiles}"
env_out="/tmp/guix-bootstrap-seeds.env"

required_files="
${dist}/static-binaries-0-i686-linux.tar.xz
${dist}/guile-static-stripped-2.2.4-i686-linux.tar.xz
${dist}/mes-minimal-stripped-0.19-i686-linux.tar.xz
${dist}/mescc-tools-static-stripped-0.5.2-i686-linux.tar.xz
${dist}/bootstrap-exec-bash-i686-linux
${dist}/bootstrap-exec-mkdir-i686-linux
${dist}/bootstrap-exec-tar-i686-linux
${dist}/bootstrap-exec-xz-i686-linux
"

for f in ${required_files}; do
    if [ ! -e "${f}" ]; then
        echo "Missing required seed artifact: ${f}" >&2
        exit 1
    fi
done

if [ ! -x /usr/bin/guix-hash-compat ]; then
    echo "Missing /usr/bin/guix-hash-compat" >&2
    exit 1
fi

# Prepare file layout expected by bootstrap.scm for i686/x86_64.
mkdir -p "${dist}/i686-linux" "${dist}/x86_64-linux"
cp -f "${dist}/static-binaries-0-i686-linux.tar.xz" \
      "${dist}/i686-linux/static-binaries-0-i686-linux.tar.xz"
cp -f "${dist}/static-binaries-0-i686-linux.tar.xz" \
      "${dist}/x86_64-linux/static-binaries-0-i686-linux.tar.xz"
cp -f "${dist}/guile-static-stripped-2.2.4-i686-linux.tar.xz" \
      "${dist}/i686-linux/guile-static-stripped-2.2.4-i686-linux.tar.xz"
cp -f "${dist}/guile-static-stripped-2.2.4-i686-linux.tar.xz" \
      "${dist}/x86_64-linux/guile-static-stripped-2.2.4-i686-linux.tar.xz"
cp -f "${dist}/bootstrap-exec-bash-i686-linux" \
      "${dist}/i686-linux/bootstrap-exec-bash-i686-linux"
cp -f "${dist}/bootstrap-exec-mkdir-i686-linux" \
      "${dist}/i686-linux/bootstrap-exec-mkdir-i686-linux"
cp -f "${dist}/bootstrap-exec-tar-i686-linux" \
      "${dist}/i686-linux/bootstrap-exec-tar-i686-linux"
cp -f "${dist}/bootstrap-exec-xz-i686-linux" \
      "${dist}/i686-linux/bootstrap-exec-xz-i686-linux"

static_binaries_hash="$(/usr/bin/guix-hash-compat "${dist}/static-binaries-0-i686-linux.tar.xz")"
guile_seed_hash="$(/usr/bin/guix-hash-compat "${dist}/guile-static-stripped-2.2.4-i686-linux.tar.xz")"
mes_minimal_hash="$(/usr/bin/guix-hash-compat "${dist}/mes-minimal-stripped-0.19-i686-linux.tar.xz")"
mescc_tools_hash="$(/usr/bin/guix-hash-compat "${dist}/mescc-tools-static-stripped-0.5.2-i686-linux.tar.xz")"
exec_bash_hash="$(/usr/bin/guix-hash-compat -r "${dist}/bootstrap-exec-bash-i686-linux")"
exec_mkdir_hash="$(/usr/bin/guix-hash-compat -r "${dist}/bootstrap-exec-mkdir-i686-linux")"
exec_tar_hash="$(/usr/bin/guix-hash-compat -r "${dist}/bootstrap-exec-tar-i686-linux")"
exec_xz_hash="$(/usr/bin/guix-hash-compat -r "${dist}/bootstrap-exec-xz-i686-linux")"

cat > "${env_out}" <<ENVEOF
DISTFILES=${dist}
STATIC_BINARIES_SEED_FILE=static-binaries-0-i686-linux.tar.xz
STATIC_BINARIES_SEED_HASH=${static_binaries_hash}
GUILE_SEED_FILE=guile-static-stripped-2.2.4-i686-linux.tar.xz
GUILE_SEED_HASH=${guile_seed_hash}
MES_MINIMAL_SEED_FILE=mes-minimal-stripped-0.19-i686-linux.tar.xz
MES_MINIMAL_SEED_HASH=${mes_minimal_hash}
MESCC_TOOLS_SEED_FILE=mescc-tools-static-stripped-0.5.2-i686-linux.tar.xz
MESCC_TOOLS_SEED_HASH=${mescc_tools_hash}
EXEC_BASH_HASH=${exec_bash_hash}
EXEC_MKDIR_HASH=${exec_mkdir_hash}
EXEC_TAR_HASH=${exec_tar_hash}
EXEC_XZ_HASH=${exec_xz_hash}
ENVEOF

chmod 0644 "${env_out}"
