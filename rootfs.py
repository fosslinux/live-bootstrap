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
# SPDX-FileCopyrightText: 2021-23 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2023-24 Gábor Stefanik <netrolller.3d@gmail.com>

import argparse
import os

from lib.utils import run, run_as_root
from lib.target import Target
from lib.generator import Generator, stage0_arch_map

def create_configuration_file(args):
    """
    Creates bootstrap.cfg file which would contain options used to
    customize bootstrap.
    """
    config_path = os.path.join('steps', 'bootstrap.cfg')
    with open(config_path, "w", encoding="utf_8") as config:
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
        config.write(f"BARE_METAL={args.bare_metal}\n")
        if (args.bare_metal or args.qemu) and not args.kernel:
            if args.repo or args.external_sources:
                config.write("DISK=sdb1\n")
            else:
                config.write("DISK=sda\n")
            config.write("KERNEL_BOOTSTRAP=True\n")
        else:
            config.write("DISK=sda1\n")
            config.write("KERNEL_BOOTSTRAP=False\n")
        config.write(f"BUILD_KERNELS={args.update_checksums or args.build_kernels}\n")

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
    parser.add_argument("--no-create-config",
                        help="Do not automatically create config file",
                        action="store_true")
    parser.add_argument("-i", "--interactive",
                        help="Use interactive prompts to resolve issues during bootstrap",
                        action="store_true")
    parser.add_argument("-r", "--repo",
                        help="Path to prebuilt binary packages", nargs=None)
    parser.add_argument("--early-preseed",
                        help="Skip early stages of live-bootstrap", nargs=None)
    parser.add_argument("--internal-ci", help="INTERNAL for github CI")
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

    # Set constant umask
    os.umask(0o022)

    # bootstrap.cfg
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

    generator = Generator(arch=args.arch,
                          external_sources=args.external_sources,
                          repo_path=args.repo,
                          early_preseed=args.early_preseed)

    bootstrap(args, generator, target, args.target_size)

def bootstrap(args, generator, target, size):
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
        run_as_root('env', '-i', 'PATH=/bin', chroot_binary, generator.target_dir, init)

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
                                  init)

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

    else:
        if args.kernel:
            generator.prepare(target, using_kernel=True, target_size=size)

            arg_list = [
                '-enable-kvm',
                '-m', str(args.qemu_ram) + 'M',
                '-smp', str(args.cores),
                '-no-reboot',
                '-drive', 'file=' + target.get_disk("disk") + ',format=raw'
            ]
            if target.get_disk("external") is not None:
                arg_list += [
                    '-drive', 'file=' + target.get_disk("external") + ',format=raw',
                ]
            arg_list += [
                '-nic', 'user,ipv6=off,model=e1000',
                '-kernel', args.kernel,
                '-nographic',
                '-append', 'console=ttyS0 root=/dev/sda1 rootfstype=ext3 init=/init rw'
            ]
            run(args.qemu_cmd, *arg_list)
        else:
            generator.prepare(target, kernel_bootstrap=True, target_size=size)
            arg_list = [
                '-enable-kvm',
                '-m', str(args.qemu_ram) + 'M',
                '-smp', str(args.cores),
                '-no-reboot',
                '-drive', 'file=' + generator.target_dir + '.img' + ',format=raw'
            ]
            if target.get_disk("external") is not None:
                arg_list += [
                    '-drive', 'file=' + target.get_disk("external") + ',format=raw',
                ]
            arg_list += [
                '-machine', 'kernel-irqchip=split',
                '-nic', 'user,ipv6=off,model=e1000',
                '-nographic'
            ]
            run(args.qemu_cmd, *arg_list)

if __name__ == "__main__":
    main()
