#!/bin/sh
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

dist="${DISTFILES:-/external/distfiles}"
env_out="/tmp/guix-bootstrap-seeds.env"
work="/tmp/guix-bootstrap-seed-variants"

required_files="
${dist}/static-binaries-0-i686-linux.tar.xz
${dist}/guile-static-stripped-2.0.9-i686-linux.tar.xz
${dist}/mes-minimal-stripped-0.19-i686-linux.tar.xz
${dist}/mescc-tools-static-stripped-0.5.2-i686-linux.tar.xz
${dist}/bootstrap-exec-bash-i686-linux
${dist}/bootstrap-exec-mkdir-i686-linux
${dist}/bootstrap-exec-tar-i686-linux
${dist}/bootstrap-exec-xz-i686-linux
"

reset_tree_timestamps() {
    find "$1" -print0 | xargs -0 touch -h -t 197001010000.00
}

make_repro_tar_xz() {
    src_dir="$1"
    out_file="$2"
    tmp_tar="$(mktemp /tmp/guix-seed-tarball.XXXXXX.tar)"

    mkdir -p "$(dirname "${out_file}")"

    reset_tree_timestamps "${src_dir}"
    (
        cd "${src_dir}"
        tar --sort=name --hard-dereference \
            --numeric-owner --owner=0 --group=0 --mode=go=rX,u+rw \
            -cf "${tmp_tar}" .
    )
    touch -t 197001010000.00 "${tmp_tar}"
    xz -T1 -c "${tmp_tar}" > "${out_file}"
    touch -t 197001010000.00 "${out_file}"
    rm -f "${tmp_tar}"
}

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
rm -rf "${work}"
mkdir -p "${dist}/i686-linux" "${dist}/x86_64-linux"
cp -f "${dist}/static-binaries-0-i686-linux.tar.xz" \
      "${dist}/i686-linux/static-binaries-0-i686-linux.tar.xz"
cp -f "${dist}/static-binaries-0-i686-linux.tar.xz" \
      "${dist}/x86_64-linux/static-binaries-0-i686-linux.tar.xz"
# Guix interns both x86_64 and i686 bootstrap Guile seeds during its self-build.
# Keep the payload effectively identical but make the archives distinct so they
# do not collapse to the same store item and GC root name.
rm -rf "${work}/i686-linux"
mkdir -p "${work}/i686-linux"
xz -dc "${dist}/guile-static-stripped-2.0.9-i686-linux.tar.xz" | tar -xf - -C "${work}/i686-linux"
printf '%s\n' "i686-linux" > "${work}/i686-linux/i686.txt.planceholder"
make_repro_tar_xz \
      "${work}/i686-linux" \
      "${dist}/i686-linux/guile-static-stripped-2.0.9-i686-linux.tar.xz"
rm -rf "${work}/x86_64-linux"
mkdir -p "${work}/x86_64-linux"
xz -dc "${dist}/guile-static-stripped-2.0.9-i686-linux.tar.xz" | tar -xf - -C "${work}/x86_64-linux"
printf '%s\n' "x86_64-linux" > "${work}/x86_64-linux/x86_64.txt.planceholder"
make_repro_tar_xz \
      "${work}/x86_64-linux" \
      "${dist}/x86_64-linux/guile-static-stripped-2.0.9-x86_64-linux.tar.xz"
cp -f "${dist}/bootstrap-exec-bash-i686-linux" \
      "${dist}/i686-linux/bootstrap-exec-bash-i686-linux"
cp -f "${dist}/bootstrap-exec-mkdir-i686-linux" \
      "${dist}/i686-linux/bootstrap-exec-mkdir-i686-linux"
cp -f "${dist}/bootstrap-exec-tar-i686-linux" \
      "${dist}/i686-linux/bootstrap-exec-tar-i686-linux"
cp -f "${dist}/bootstrap-exec-xz-i686-linux" \
      "${dist}/i686-linux/bootstrap-exec-xz-i686-linux"

static_binaries_hash="$(/usr/bin/guix-hash-compat "${dist}/static-binaries-0-i686-linux.tar.xz")"
guile_seed_i686_hash="$(/usr/bin/guix-hash-compat "${dist}/i686-linux/guile-static-stripped-2.0.9-i686-linux.tar.xz")"
guile_seed_x86_64_hash="$(/usr/bin/guix-hash-compat "${dist}/x86_64-linux/guile-static-stripped-2.0.9-x86_64-linux.tar.xz")"
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
GUILE_I686_SEED_FILE=i686-linux/guile-static-stripped-2.0.9-i686-linux.tar.xz
GUILE_I686_SEED_HASH=${guile_seed_i686_hash}
GUILE_X86_64_SEED_FILE=x86_64-linux/guile-static-stripped-2.0.9-x86_64-linux.tar.xz
GUILE_X86_64_SEED_HASH=${guile_seed_x86_64_hash}
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
