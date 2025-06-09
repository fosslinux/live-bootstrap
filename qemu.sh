#!/usr/bin/env sh

# SPDX-FileCopyrightText: 2025 Alexandre Gomes Gaigalas <alganet@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -euf

# Runtime dependencies
SH="${SH:-$(command -v sh)}"

# Userland dependencies
CAT="${CAT:-$(command -v cat)}"
DD="${DD:-$(command -v dd)}"
FIND="${FIND:-$(command -v find)}"
MKDIR="${MKDIR:-$(command -v mkdir)}"
WC="${WC:-$(command -v wc)}"

# Emulation dependencies
QEMU="${QEMU:-$(command -v qemu-system-i386)}"
MKFIFO="${MKFIFO:-$(command -v mkfifo)}"
NC="${NC:-$(command -v nc)}"

# Don't look PATH anymore
PATH=

# Remember home folder
SH_FILE="$PWD/$0"
LB_ROOT="${SH_FILE%\/*}"

# Reads from a pipe from `find` and writes a builder-hex0 compatible src,
# optionally adding a $1 prefix to the destination filename
to_builder_hex0_src () {
    local prefix="${1:-}"
    local fs_entry=
    local wc_out=

    while read -r fs_entry; do
        case $fs_entry in
            *'/.git'*) continue;; # Ignore lengthy .git filetrees
        esac

        if [ -d "$fs_entry" ]; then
            printf %s\\n "src 0 ${fs_entry#\.}"
        else
            wc_out="$($WC -c "$fs_entry")"
            printf %s\\n "src ${wc_out%% *} ${prefix}${fs_entry#\.}"
            $CAT "$fs_entry"
        fi

    done
}

# Walk the manifest looking for sources and does $1 on them
walk_manifest_distfiles () {
    local action=$1
    local manifest_entry=
    local entry=
    local file=
    local walked=":"

    while read -r manifest_entry || [ -n "$manifest_entry" ]; do
        case $manifest_entry in
            'improve: get_network'*)
                break
                ;;
            'build:'*)
                entry="${manifest_entry#'build: '}"
                entry="${entry%% *}"
                [ -e "./steps/${entry}/sources" ] || continue
                while read -r line; do
                    file="${line##* }"
                    if [ ! -e "./mirror/$file" ]; then
                        file="${line##*\/}"
                        file="${file%% *}"
                    fi
                    case $walked in *":$file:"*) continue;;esac
                    walked="$walked$file:"
                    $action "./distfiles/$file"
                done < "./steps/${entry}/sources"
                ;;
        esac
    done < "./steps/manifest"
}

cd "$LB_ROOT"

$MKDIR "$LB_ROOT/target"
$MKDIR "$LB_ROOT/target/steps"

$CAT <<@ > "$LB_ROOT/steps/bootstrap.cfg"
ARCH=x86
ARCH_DIR=x86
FORCE_TIMESTAMPS=False
CHROOT=False
UPDATE_CHECKSUMS=False
JOBS=6
SWAP_SIZE=0
FINAL_JOBS=6
INTERNAL_CI=False
INTERACTIVE=False
BARE_METAL=False
QEMU=True
DISK=sda
KERNEL_BOOTSTRAP=True
BUILD_KERNELS=True
CONFIGURATOR=False
MIRRORS="http://10.0.2.2:42645"
MIRRORS_LEN=1
@

# Creates ./target/init.builder-hex0-src
{
    printf %s\\n "Copying stage0-posix..." 1>&2
    cd "$LB_ROOT/seed/stage0-posix";
    $FIND ./ | to_builder_hex0_src

    printf %s\\n "Copying seed folder..." 1>&2
    cd "$LB_ROOT/seed";
    $FIND ./ -maxdepth 1 | to_builder_hex0_src

    # Write steps
    printf %s\\n "Copying steps..." 1>&2
    cd "$LB_ROOT"
    printf %s\\n "src 0 /steps"
    $FIND ./steps | to_builder_hex0_src

    # Write distfiles as /external/distfiles
    cd "$LB_ROOT";
    printf %s\\n "src 0 /external"
    printf %s\\n "src 0 /external/distfiles"
    printf %s\\n "Copying distfiles..." 1>&2
    walk_manifest_distfiles echo | to_builder_hex0_src "/external"

    # Initialization sequence
    printf %s\\n "src 0 /dev"
    printf %s\\n "src 0 /bootstrap-seeds"
    printf %s\\n "src 0 /bootstrap-seeds/POSIX"
    printf %s\\n "src 0 /bootstrap-seeds/POSIX/x86"
    printf %s\\n "hex0 /x86/hex0_x86.hex0 /bootstrap-seeds/POSIX/x86/hex0-seed"
    printf %s\\n "hex0 /x86/kaem-minimal.hex0 /bootstrap-seeds/POSIX/x86/kaem-optional-seed"
    printf %s\\n "hex0 /x86/kaem-minimal.hex0 /init"
    printf %s\\n "/bootstrap-seeds/POSIX/x86/kaem-optional-seed /kaem.x86"
} >> ./target/init.builder-hex0-src

# Creates the builder_hex0 image
printf %s\\n "Creating image (this might take a while)..." 1>&2
$DD if=/dev/zero of="target/init.img" bs=512 count=33554432
$DD if=seed/stage0-posix/bootstrap-seeds/NATIVE/x86/builder-hex0-x86-stage1.img of="target/init.img" conv=notrunc
$DD if="builder-hex0/builder-hex0-x86-stage2.hex0" of="target/init.img" seek=1 bs=512 conv=notrunc

STAGE2_LEN="$($WC -c builder-hex0/builder-hex0-x86-stage2.hex0)"
STAGE2_LEN="${STAGE2_LEN% *}"
if [ $((STAGE2_LEN % 512)) = 0 ]; then
    STAGE2_SECTORS=$((STAGE2_LEN / 512))
else
    STAGE2_SECTORS=$((STAGE2_LEN / 512 + 1))
fi
SRC_LBA_SECTOR=$((STAGE2_SECTORS + 1))

# Appends the generated source to the image
printf %s\\n "Adding sources to image..." 1>&2
$DD if="target/init.builder-hex0-src" of="target/init.img" seek=$SRC_LBA_SECTOR bs=512 conv=notrunc

# Starts a simple HTTP file server for ./distfiles in the background
{
    $MKFIFO $LB_ROOT/target/httpmirror.fifo
    while true; do
        $NC -l 127.0.0.1 42645 < $LB_ROOT/target/httpmirror.fifo | {
            ret="$(printf \\r)"
            while read -r line; do
                case $line in
                    'GET /'*)
                        distfile="${line#* \/}"
                        distfile="${distfile% *}"
                        ;;
                    "$ret")
                        break
                        ;;
                    *)
                        continue
                        ;;
                esac
            done
            if ! [ -f "$LB_ROOT/mirror/$distfile" ]; then 
                echo "HTTP/1.1 404 Not Found"
                echo "Content-Length: 0"
                echo "Connection: close"
                printf \\r\\n\\r\\n
            else
                echo "HTTP/1.1 200 OK"
                echo "Content-Length: $($WC -c "$LB_ROOT/mirror/$distfile")"
                echo "Connection: close"
                printf \\r\\n\\r\\n
                $CAT "$LB_ROOT/mirror/$distfile"
            fi
        } > $LB_ROOT/target/httpmirror.fifo
    done
} &
HTTPMIRROR_PID=$!

# Launch image
$QEMU \
    --enable-kvm \
    -m 4G \
    -smp 6 \
    -nographic \
    -no-reboot \
    -nic user,ipv6=off,model=e1000 \
    -drive file="target/init.img",format=raw \
    -machine kernel-irqchip=split || :

kill -9 $HTTPMIRROR_PID

