#!/usr/bin/env python3
"""
A helper application used to start bootstrapping process.
It has a few modes of operation, you can create initramfs with
binary seeds and sources that you can boot into or alternatively
you can run bootstap inside chroot.
"""

# SPDX-License-Identifier: GPL-3.0-or-later
# SPDX-FileCopyrightText: 2022 Dor Askayo <dor.askayo@gmail.com>
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 Bastian Bittorf <bb@npl.de>
# SPDX-FileCopyrightText: 2021 Melg Eight <public.melg8@gmail.com>
# SPDX-FileCopyrightText: 2021-23 Samuel Tyler <samuel@samuelt.me>
# SPDX-FileCopyrightText: 2023-24 Gábor Stefanik <netrolller.3d@gmail.com>

import argparse
import os
import signal
import shutil
import tempfile
import threading

from lib.generator import Generator, stage0_arch_map
from lib.simple_mirror import SimpleMirror
from lib.target import Target
from lib.utils import run, run_as_root

def parse_internal_ci_break_after(value):
    """
    Parse INTERNAL_CI break directive in the form '<scope>:<step>'.
    """
    if not value:
        return None, None
    scope, separator, step_name = value.partition(":")
    scope = scope.strip()
    step_name = step_name.strip()
    if separator != ":" or scope not in ("steps", "steps-guix") or not step_name:
        raise ValueError(
            "--internal-ci-break-after must be in the form "
            "'steps:<name>' or 'steps-guix:<name>'."
        )
    return scope, step_name

def apply_internal_ci_break_to_tree(tree_root, break_scope, break_step, internal_ci):
    """
    Inject INTERNAL_CI break jump after a build step in a prepared steps tree.
    """
    if not break_scope or not break_step:
        return
    if internal_ci in ("", "False", None):
        raise ValueError("INTERNAL_CI must be set when INTERNAL_CI_BREAK_AFTER is used.")

    manifest_path = os.path.join(tree_root, break_scope, "manifest")
    if not os.path.isfile(manifest_path):
        raise ValueError(f"Missing manifest for INTERNAL_CI_BREAK_AFTER: {manifest_path}")

    with open(manifest_path, "r", encoding="utf-8") as manifest_file:
        manifest_lines = manifest_file.readlines()

    inserted = False
    break_line = f"jump: break ( INTERNAL_CI == {internal_ci} )\n"
    output_lines = []
    for line in manifest_lines:
        output_lines.append(line)
        stripped = line.strip()
        if inserted or not stripped.startswith("build: "):
            continue

        step_name = stripped[len("build: "):].split("#", 1)[0].strip()
        if step_name == break_step:
            output_lines.append(break_line)
            inserted = True

    if not inserted:
        raise ValueError(
            "INTERNAL_CI_BREAK_AFTER target not found in "
            + break_scope
            + "/manifest: "
            + break_step
        )

    with open(manifest_path, "w", encoding="utf-8") as manifest_file:
        manifest_file.writelines(output_lines)

def update_stage0_image(image_path,
                        build_guix_also=False,
                        mirrors=None,
                        internal_ci=False,
                        internal_ci_break_after=None):
    """
    Update an existing stage0 image by refreshing step sources from the working
    tree and patching bootstrap config / optional guix handoff bits.
    """
    if mirrors is None:
        mirrors = []

    steps_dir = os.path.abspath("steps")
    steps_manifest = os.path.join(steps_dir, "manifest")
    if not os.path.isdir(steps_dir) or not os.path.isfile(steps_manifest):
        raise ValueError("steps/manifest does not exist.")

    steps_guix_dir = ""
    if build_guix_also:
        steps_guix_dir = os.path.abspath("steps-guix")
        manifest = os.path.join(steps_guix_dir, "manifest")
        if not os.path.isdir(steps_guix_dir) or not os.path.isfile(manifest):
            raise ValueError("steps-guix/manifest does not exist while --build-guix-also is set.")
    break_scope, break_step = parse_internal_ci_break_after(internal_ci_break_after)

    mountpoint = tempfile.mkdtemp(prefix="lb-stage0-", dir="/tmp")
    mounted = False
    try:
        run_as_root(
            "mount",
            "-t", "ext4",
            "-o", "loop,offset=1073741824",
            image_path,
            mountpoint,
        )
        mounted = True
        script = '''
import os
import shutil
import sys

mountpoint = sys.argv[1]
steps_dir = sys.argv[2]
steps_guix_dir = sys.argv[3]
build_guix_also = (sys.argv[4] == "True")
internal_ci = sys.argv[5]
break_scope = sys.argv[6]
break_step = sys.argv[7]
mirrors = sys.argv[8:]

old_config_path = os.path.join(mountpoint, "steps", "bootstrap.cfg")
if not os.path.isfile(old_config_path):
    raise SystemExit(f"Missing config in stage0 image: {old_config_path}")
old_env_path = os.path.join(mountpoint, "steps", "env")
old_env_content = None
if os.path.isfile(old_env_path):
    with open(old_env_path, "rb") as env_file:
        old_env_content = env_file.read()

with open(old_config_path, "r", encoding="utf-8") as cfg:
    lines = [
        line for line in cfg
        if not line.startswith("BUILD_GUIX_ALSO=")
        and not line.startswith("INTERNAL_CI=")
        and not line.startswith("MIRRORS=")
        and not line.startswith("MIRRORS_LEN=")
        and not line.startswith("PAYLOAD_REQUIRED=")
    ]

dest_steps = os.path.join(mountpoint, "steps")
if os.path.isdir(dest_steps):
    shutil.copytree(steps_dir, dest_steps, dirs_exist_ok=True)
else:
    shutil.copytree(steps_dir, dest_steps)
if old_env_content is not None:
    with open(os.path.join(dest_steps, "env"), "wb") as env_file:
        env_file.write(old_env_content)
env_path = os.path.join(dest_steps, "env")
if os.path.isfile(env_path):
    with open(env_path, "r", encoding="utf-8", errors="ignore") as env_file:
        env_content = env_file.read()
else:
    env_content = ""
if "NETWORK_READY=" not in env_content:
    with open(env_path, "a", encoding="utf-8") as env_file:
        if env_content and not env_content.endswith("\\n"):
            env_file.write("\\n")
        env_file.write("NETWORK_READY=False\\n")

config_path = os.path.join(dest_steps, "bootstrap.cfg")
if build_guix_also:
    lines.append("BUILD_GUIX_ALSO=True\\n")
if mirrors:
    lines.append(f'MIRRORS="{" ".join(mirrors)}"\\n')
    lines.append(f"MIRRORS_LEN={len(mirrors)}\\n")
lines.append(f"INTERNAL_CI={internal_ci}\\n")
lines.append("PAYLOAD_REQUIRED=False\\n")
with open(config_path, "w", encoding="utf-8") as cfg:
    cfg.writelines(lines)

# Ensure resumed stage0-image init scripts have minimal early mounts and
# only restore networking after get_network has run.
mount_marker = "# LB_STAGE0_EARLY_MOUNTS"
regen_marker = "# LB_STAGE0_REGENERATE_SCRIPTS"
legacy_network_block = (
    'if [ "${CHROOT}" = False ] && command -v dhcpcd >/dev/null 2>&1; then\\n'
    'dhcpcd --waitip=4 || true\\n'
    'fi\\n'
)
regen_block = (
    regen_marker + "\\n"
    + 'if [ -x /script-generator ] && [ -f /steps/manifest ]; then\\n'
    + '/script-generator /steps/manifest\\n'
    + 'fi\\n'
    + 'if [ -x /script-generator ] && [ -f /steps-guix/manifest ]; then\\n'
    + '/script-generator /steps-guix/manifest /steps\\n'
    + 'fi\\n'
    + 'if [ -f /steps-guix/0.sh ] && grep -Eq "/steps-guix/[1-9][0-9]*\\\\.sh" "$0"; then\\n'
    + 'if ! bash /steps-guix/0.sh; then\\n'
    + 'status=$?\\n'
    + 'echo "bootstrap script failed with status ${status}; dropping to rescue shell" >&2\\n'
    + 'PATH=/bin:/usr/bin:/sbin:/usr/sbin:${PREFIX}/bin:${PATH}\\n'
    + 'while true; do\\n'
    + 'if command -v bash >/dev/null 2>&1; then\\n'
    + 'env PS1="[FAIL ${status}] \\\\w # " bash -i\\n'
    + 'elif command -v sh >/dev/null 2>&1; then\\n'
    + 'env PS1="[FAIL ${status}] \\\\w # " sh -i\\n'
    + 'else\\n'
    + 'sleep 60\\n'
    + 'fi\\n'
    + 'done\\n'
    + 'fi\\n'
    + 'exit 0\\n'
    + 'fi\\n'
)
gated_network_block = (
    'if [ -f /steps/env ]; then\\n'
    '. /steps/env\\n'
    'fi\\n'
    'if [ "${CHROOT}" = False ] && [ "${NETWORK_READY}" = True ] && command -v dhcpcd >/dev/null 2>&1; then\\n'
    'dhcpcd --waitip=4 || true\\n'
    'fi\\n'
)
for init_name in os.listdir(mountpoint):
    if init_name != "init" and not init_name.startswith("init."):
        continue
    init_path = os.path.join(mountpoint, init_name)
    if not os.path.isfile(init_path):
        continue

    with open(init_path, "r", encoding="utf-8", errors="ignore") as init_file:
        init_content = init_file.read()

    if mount_marker not in init_content:
        first_newline = init_content.find("\\n")
        if first_newline != -1:
            mount_block = (
                mount_marker + "\\n"
                + "mount | grep ' on /dev ' >/dev/null 2>&1 || (mkdir -p /dev; mount -t devtmpfs devtmpfs /dev)\\n"
                + "mount | grep ' on /proc ' >/dev/null 2>&1 || (mkdir -p /proc; mount -t proc proc /proc)\\n"
            )
            init_content = (
                init_content[: first_newline + 1]
                + mount_block
                + init_content[first_newline + 1 :]
            )

    if legacy_network_block in init_content and gated_network_block not in init_content:
        init_content = init_content.replace(legacy_network_block, gated_network_block)

    if regen_marker not in init_content:
        first_newline = init_content.find("\\n")
        if first_newline != -1:
            init_content = (
                init_content[: first_newline + 1]
                + regen_block
                + init_content[first_newline + 1 :]
            )

    with open(init_path, "w", encoding="utf-8") as init_file:
        init_file.write(init_content)

if build_guix_also:
    dest_steps_guix = os.path.join(mountpoint, "steps-guix")
    if os.path.isdir(dest_steps_guix):
        shutil.copytree(steps_guix_dir, dest_steps_guix, dirs_exist_ok=True)
    else:
        shutil.copytree(steps_guix_dir, dest_steps_guix)

if break_scope and break_step:
    if internal_ci in ("", "False"):
        raise SystemExit("INTERNAL_CI must be set when INTERNAL_CI_BREAK_AFTER is used.")

    manifest_path = os.path.join(mountpoint, break_scope, "manifest")
    if not os.path.isfile(manifest_path):
        raise SystemExit(f"Missing manifest for INTERNAL_CI_BREAK_AFTER: {manifest_path}")

    with open(manifest_path, "r", encoding="utf-8") as manifest_file:
        manifest_lines = manifest_file.readlines()

    inserted = False
    break_line = f"jump: break ( INTERNAL_CI == {internal_ci} )\\n"
    output_lines = []
    for line in manifest_lines:
        output_lines.append(line)
        stripped = line.strip()
        if inserted or not stripped.startswith("build: "):
            continue

        step_name = stripped[len("build: "):].split("#", 1)[0].strip()
        if step_name == break_step:
            output_lines.append(break_line)
            inserted = True

    if not inserted:
        raise SystemExit(
            "INTERNAL_CI_BREAK_AFTER target not found in "
            + break_scope
            + "/manifest: "
            + break_step
        )

    with open(manifest_path, "w", encoding="utf-8") as manifest_file:
        manifest_file.writelines(output_lines)
'''
        run_as_root(
            "python3",
            "-c",
            script,
            mountpoint,
            steps_dir,
            steps_guix_dir,
            "True" if build_guix_also else "False",
            str(internal_ci) if internal_ci else "False",
            break_scope or "",
            break_step or "",
            *mirrors,
        )
    finally:
        if mounted:
            run_as_root("umount", mountpoint)
        os.rmdir(mountpoint)

def prepare_stage0_work_image(base_image,
                              output_dir,
                              build_guix_also,
                              mirrors=None,
                              internal_ci=False,
                              internal_ci_break_after=None):
    """
    Copy stage0 base image to a disposable work image and refresh steps/config.
    """
    work_image = os.path.join(output_dir, "stage0-work.img")
    shutil.copy2(base_image, work_image)
    update_stage0_image(work_image,
                        build_guix_also=build_guix_also,
                        mirrors=mirrors,
                        internal_ci=internal_ci,
                        internal_ci_break_after=internal_ci_break_after)
    return work_image

def create_configuration_file(args):
    """
    Creates bootstrap.cfg file which would contain options used to
    customize bootstrap.
    """
    config_path = os.path.join('steps', 'bootstrap.cfg')
    with open(config_path, "w", encoding="utf_8") as config:
        kernel_bootstrap = ((args.bare_metal or args.qemu) and not args.kernel)
        payload_required = kernel_bootstrap and args.external_sources and not args.repo
        config.write(f"ARCH={args.arch}\n")
        config.write(f"ARCH_DIR={stage0_arch_map.get(args.arch, args.arch)}\n")
        config.write(f"FORCE_TIMESTAMPS={args.force_timestamps}\n")
        config.write(f"CHROOT={args.chroot or args.bwrap}\n")
        config.write(f"UPDATE_CHECKSUMS={args.update_checksums}\n")
        config.write(f"JOBS={args.cores}\n")
        config.write(f"SWAP_SIZE={args.swap}\n")
        config.write(f"FINAL_JOBS={args.cores}\n")
        config.write(f"INTERNAL_CI={args.internal_ci or False}\n")
        config.write(f"INTERACTIVE={args.interactive}\n")
        config.write(f"PAYLOAD_REQUIRED={payload_required}\n")
        config.write(f"QEMU={args.qemu}\n")
        config.write(f"BARE_METAL={args.bare_metal or (args.qemu and args.interactive)}\n")
        config.write(f"BUILD_GUIX_ALSO={args.build_guix_also}\n")
        if kernel_bootstrap:
            config.write("DISK=sdb1\n" if args.repo else "DISK=sda\n")
            config.write("KERNEL_BOOTSTRAP=True\n")
        else:
            config.write("DISK=sda1\n")
            config.write("KERNEL_BOOTSTRAP=False\n")
        config.write(f"BUILD_KERNELS={args.update_checksums or args.build_kernels}\n")
        config.write(f"CONFIGURATOR={args.configurator}\n")
        if not args.external_sources:
            if args.mirrors:
                config.write(f'MIRRORS="{" ".join(args.mirrors)}"\n')
                config.write(f"MIRRORS_LEN={len(args.mirrors)}\n")
            else:
                config.write("MIRRORS_LEN=0\n")

# pylint: disable=too-many-statements,too-many-branches
def main():
    """
    A few command line arguments to customize bootstrap.
    This function also creates object which prepares directory
    structure with bootstrap seeds and all sources.
    """
    parser = argparse.ArgumentParser()
    parser.add_argument("-a", "--arch", help="Bootstrap architecture",
                        default="x86")
    parser.add_argument("-c", "--chroot", help="Run inside chroot",
                        action="store_true")
    parser.add_argument("-bw", "--bwrap", help="Run inside a bwrap sandbox",
                        action="store_true")
    parser.add_argument("-t", "--target", help="Target directory",
                        default="target")
    parser.add_argument("--tmpfs", help="Use a tmpfs on target",
                        action="store_true")
    parser.add_argument("--tmpfs-size", help="Size of the tmpfs",
                        default="8G")
    parser.add_argument("--cores", help="Cores to use for building",
                         default=2)
    parser.add_argument("--force-timestamps",
                        help="Force all files timestamps to be 0 unix time",
                        action="store_true")
    parser.add_argument("--update-checksums",
                        help="Update checksum files",
                        action="store_true")
    parser.add_argument("--external-sources",
                        help="Download sources externally from live-bootstrap",
                        action="store_true")
    parser.add_argument("--build-kernels",
                        help="Also build kernels in chroot and bwrap builds",
                        action="store_true")
    parser.add_argument("--build-guix-also",
                        help="After main steps finish, switch to steps-guix and run its manifest",
                        action="store_true")
    parser.add_argument("--no-create-config",
                        help="Do not automatically create config file",
                        action="store_true")
    parser.add_argument("-i", "--interactive",
                        help="Use interactive prompts to resolve issues during bootstrap",
                        action="store_true")
    parser.add_argument("--configurator",
                        help="Run the interactive configurator",
                        action="store_true")
    parser.add_argument("-m", "--mirrors",
                        help="Mirrors to download distfiles from",
                        nargs='+')
    parser.add_argument("-r", "--repo",
                        help="Path to prebuilt binary packages", nargs=None)
    parser.add_argument("--early-preseed",
                        help="Skip early stages of live-bootstrap", nargs=None)
    parser.add_argument("--internal-ci", help="INTERNAL for github CI")
    parser.add_argument("--internal-ci-break-after",
                        help="Insert a temporary jump: break after a build step using "
                             "'steps:<name>' or 'steps-guix:<name>' "
                             "for --stage0-image resume runs and fresh --qemu kernel-bootstrap runs.")
    parser.add_argument("-s", "--swap", help="Swap space to allocate in Linux",
                        default=0)

    # QEMU arguments
    parser.add_argument("-q", "--qemu", help="Use QEMU",
                        action="store_true")
    parser.add_argument("-qc", "--qemu-cmd", help="QEMU command to run",
                        default="qemu-system-x86_64")
    parser.add_argument("-qr", "--qemu-ram", help="Memory (in megabytes) allocated to QEMU VM",
                        default=4096)
    parser.add_argument("-qs", "--target-size", help="Size of the target image (for QEMU only)",
                        default="16G")
    parser.add_argument("-qk", "--kernel", help="Custom early kernel to use")
    parser.add_argument("--stage0-image",
                        help="Boot an existing stage0 image (target/init.img) directly in QEMU")

    parser.add_argument("-b", "--bare-metal", help="Build images for bare metal",
                        action="store_true")

    args = parser.parse_args()

    # Mode validation
    def check_types():
        count = 0
        if args.qemu:
            count += 1
        if args.chroot:
            count += 1
        if args.bwrap:
            count += 1
        if args.bare_metal:
            count += 1
        return count

    if check_types() > 1:
        raise ValueError("No more than one of qemu, chroot, bwrap, bare metal"
                         "may be used.")
    if check_types() == 0:
        raise ValueError("One of qemu, chroot, bwrap, or bare metal must be selected.")

    # Arch validation
    if args.arch != "x86":
        print("Only x86 is supported at the moment, other arches are for development only.")

    # Tmpfs validation
    if args.bwrap and args.tmpfs:
        raise ValueError("tmpfs cannot be used with bwrap.")

    # Cores validation
    if int(args.cores) < 1:
        raise ValueError("Must use one or more cores.")

    # Target image size validation
    if args.qemu:
        if int(str(args.target_size).rstrip('gGmM')) < 1:
            raise ValueError("Please specify a positive target size for qemu.")
        args.target_size = (int(str(args.target_size).rstrip('gGmM')) *
            (1024 if str(args.target_size).lower().endswith('g') else 1))
    else:
        args.target_size = 0

    # Swap file size validation
    if args.qemu or args.bare_metal:
        args.swap = (int(str(args.swap).rstrip('gGmM')) *
            (1024 if str(args.swap).lower().endswith('g') else 1))
    else:
        args.swap = 0

    # Validate mirrors
    if not args.mirrors and not args.stage0_image:
        raise ValueError("At least one mirror must be provided.")

    if args.internal_ci_break_after:
        if not args.internal_ci:
            raise ValueError("--internal-ci-break-after requires --internal-ci (e.g. pass2).")
        break_scope, _ = parse_internal_ci_break_after(args.internal_ci_break_after)
        if break_scope == "steps-guix" and not args.build_guix_also:
            raise ValueError("--internal-ci-break-after steps-guix:* requires --build-guix-also.")
        if not args.qemu:
            raise ValueError("--internal-ci-break-after currently requires --qemu.")
        if args.kernel:
            raise ValueError("--internal-ci-break-after cannot be used with --kernel.")

    if args.stage0_image:
        if not args.qemu:
            raise ValueError("--stage0-image can only be used with --qemu.")
        if args.kernel:
            raise ValueError("--stage0-image cannot be combined with --kernel.")
        if not os.path.isfile(args.stage0_image):
            raise ValueError(f"Stage0 image does not exist: {args.stage0_image}")
        args.stage0_image = os.path.abspath(args.stage0_image)

    # Set constant umask
    os.umask(0o022)

    # file:// mirrors
    mirror_servers = []
    if args.mirrors:
        for i, mirror in enumerate(args.mirrors):
            if mirror.startswith("file://"):
                path = mirror.removeprefix("file://")
                if not path.startswith("/"):
                    raise ValueError("A file:// mirror must be an absolute path.")

                server = SimpleMirror(path)
                args.mirrors[i] = f"http://127.0.0.1:{server.port}"
                mirror_servers.append(server)

    # bootstrap.cfg
    if not args.stage0_image:
        try:
            os.remove(os.path.join('steps', 'bootstrap.cfg'))
        except FileNotFoundError:
            pass
        if not args.no_create_config:
            create_configuration_file(args)
        else:
            with open(os.path.join('steps', 'bootstrap.cfg'), 'a', encoding='UTF-8'):
                pass

    # target
    target = Target(path=args.target)
    if args.tmpfs:
        target.tmpfs(size=args.tmpfs_size)

    for server in mirror_servers:
        thread = threading.Thread(target=server.serve_forever)
        thread.start()

    def cleanup(*_):
        for server in mirror_servers:
            server.shutdown()
    signal.signal(signal.SIGINT, cleanup)

    generator = None
    if not args.stage0_image:
        generator = Generator(arch=args.arch,
                              external_sources=args.external_sources,
                              repo_path=args.repo,
                              early_preseed=args.early_preseed,
                              mirrors=args.mirrors,
                              build_guix_also=args.build_guix_also)

    bootstrap(args, generator, target, args.target_size, cleanup)
    cleanup()

def bootstrap(args, generator, target, size, cleanup):
    """Kick off bootstrap process."""
    print(f"Bootstrapping {args.arch}", flush=True)
    if args.chroot:
        find_chroot = """
import shutil
print(shutil.which('chroot'))
"""
        chroot_binary = run_as_root('python3', '-c', find_chroot,
                                    capture_output=True).stdout.decode().strip()

        generator.prepare(target, using_kernel=False)

        arch = stage0_arch_map.get(args.arch, args.arch)
        init = os.path.join(os.sep, 'bootstrap-seeds', 'POSIX', arch, 'kaem-optional-seed')
        run_as_root('env', '-i', 'PATH=/bin', chroot_binary, generator.target_dir, init,
                    cleanup=cleanup)

    elif args.bwrap:
        init = '/init'
        if not args.internal_ci or args.internal_ci == "pass1":
            generator.prepare(target, using_kernel=False)

            arch = stage0_arch_map.get(args.arch, args.arch)
            init = os.path.join(os.sep, 'bootstrap-seeds', 'POSIX', arch, 'kaem-optional-seed')
        else:
            generator.reuse(target)

        run('env', '-i', 'bwrap', '--unshare-user',
                                  '--uid', '0',
                                  '--gid', '0',
                                  '--unshare-net' if args.external_sources else None,
                                  '--setenv', 'PATH', '/usr/bin',
                                  '--bind', generator.target_dir, '/',
                                  '--dir', '/dev',
                                  '--dev-bind', '/dev/null', '/dev/null',
                                  '--dev-bind', '/dev/zero', '/dev/zero',
                                  '--dev-bind', '/dev/random', '/dev/random',
                                  '--dev-bind', '/dev/urandom', '/dev/urandom',
                                  '--dev-bind', '/dev/ptmx', '/dev/ptmx',
                                  '--dev-bind', '/dev/tty', '/dev/tty',
                                  '--tmpfs', '/dev/shm',
                                  '--proc', '/proc',
                                  '--bind', '/sys', '/sys',
                                  '--tmpfs', '/tmp',
                                  init,
            cleanup=cleanup)

    elif args.bare_metal:
        if args.kernel:
            generator.prepare(target, using_kernel=True, target_size=size)
            path = os.path.join(args.target, os.path.relpath(generator.target_dir, args.target))
            print("Please:")
            print(f"  1. Take {path}/initramfs and your kernel, boot using this.")
            print(f"  2. Take {path}/disk.img and put this on a writable storage medium.")
        else:
            generator.prepare(target, kernel_bootstrap=True, target_size=size)
            path = os.path.join(args.target, os.path.relpath(generator.target_dir, args.target))
            print("Please:")
            print(f"  1. Take {path}.img and write it to a boot drive and then boot it.")
            external_disk = target.get_disk("external")
            if external_disk is not None:
                external_path = os.path.join(args.target, os.path.relpath(external_disk, args.target))
                if args.repo:
                    print("  2. Take " +
                          f"{external_path} and attach it as a second disk (/dev/sdb preferred).")
                else:
                    print("  2. Take " +
                          f"{external_path} and attach it as a second raw container disk "
                          "(/dev/sdb preferred).")

    else:
        if args.stage0_image:
            work_image = prepare_stage0_work_image(
                args.stage0_image,
                target.path,
                args.build_guix_also,
                mirrors=args.mirrors,
                internal_ci=args.internal_ci,
                internal_ci_break_after=args.internal_ci_break_after,
            )
            arg_list = [
                '-enable-kvm',
                '-m', str(args.qemu_ram) + 'M',
                '-smp', str(args.cores),
                '-drive', 'file=' + work_image + ',format=raw',
                '-machine', 'kernel-irqchip=split',
                '-nic', 'user,ipv6=off,model=e1000'
            ]
        elif args.kernel:
            generator.prepare(target, using_kernel=True, target_size=size)

            arg_list = [
                '-enable-kvm',
                '-m', str(args.qemu_ram) + 'M',
                '-smp', str(args.cores),
                '-drive', 'file=' + target.get_disk("disk") + ',format=raw'
            ]
            if target.get_disk("external") is not None:
                arg_list += [
                    '-drive', 'file=' + target.get_disk("external") + ',format=raw',
                ]
            arg_list += [
                '-nic', 'user,ipv6=off,model=e1000',
                '-kernel', args.kernel,
                '-append',
            ]
            if args.interactive:
                arg_list += ['consoleblank=0 earlyprintk=vga root=/dev/sda1 '
                             'rootfstype=ext3 init=/init rw']
            else:
                arg_list += ['console=ttyS0 earlycon=uart8250,io,0x3f8,115200n8 '
                             'root=/dev/sda1 rootfstype=ext3 init=/init rw']
        else:
            generator.prepare(target, kernel_bootstrap=True, target_size=size)
            if args.internal_ci_break_after:
                break_scope, break_step = parse_internal_ci_break_after(args.internal_ci_break_after)
                apply_internal_ci_break_to_tree(
                    generator.target_dir,
                    break_scope,
                    break_step,
                    args.internal_ci,
                )
                os.remove(generator.target_dir + '.img')
                generator.create_builder_hex0_disk_image(generator.target_dir + '.img', size)
            arg_list = [
                '-enable-kvm',
                '-m', str(args.qemu_ram) + 'M',
                '-smp', str(args.cores),
                '-drive', 'file=' + generator.target_dir + '.img' + ',format=raw'
            ]
            if target.get_disk("external") is not None:
                arg_list += [
                    '-drive', 'file=' + target.get_disk("external") + ',format=raw',
                ]
            arg_list += [
                '-machine', 'kernel-irqchip=split',
                '-nic', 'user,ipv6=off,model=e1000'
            ]
        if not args.interactive:
            arg_list += ['-no-reboot', '-nographic']
        run(args.qemu_cmd, *arg_list, cleanup=cleanup)

if __name__ == "__main__":
    main()
