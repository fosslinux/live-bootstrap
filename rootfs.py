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

import argparse
import os

from sysa import SysA
from sysc import SysC
from lib.utils import run
from lib.sysgeneral import stage0_arch_map
from lib.tmpdir import Tmpdir

def create_configuration_file(args):
    """
    Creates bootstrap.cfg file which would contain options used to
    customize bootstrap.
    """
    config_path = os.path.join('sysa', 'bootstrap.cfg')
    with open(config_path, "w", encoding="utf_8") as config:
        config.write(f"FORCE_TIMESTAMPS={args.force_timestamps}\n")
        config.write(f"CHROOT={args.chroot or args.bwrap}\n")
        config.write(f"CHROOT_ONLY_SYSA={args.bwrap}\n")
        config.write(f"UPDATE_CHECKSUMS={args.update_checksums}\n")
        config.write(f"JOBS={args.cores}\n")
        config.write("DISK=sda1\n")
        if (args.bare_metal or args.qemu) and not args.kernel:
            config.write("KERNEL_BOOTSTRAP=True\n")
        else:
            config.write("KERNEL_BOOTSTRAP=False\n")
        config.write(f"BUILD_KERNELS={args.update_checksums or args.build_kernels}\n")

# pylint: disable=too-many-statements
def main():
    """
    A few command line arguments to customize bootstrap.
    This function also creates SysA object which prepares directory
    structure with bootstrap seeds and all sources.
    """
    parser = argparse.ArgumentParser()
    parser.add_argument("-a", "--arch", help="Bootstrap architecture",
                        default="x86")
    parser.add_argument("-c", "--chroot", help="Run inside chroot",
                        action="store_true")
    parser.add_argument("-bw", "--bwrap", help="Run inside a bwrap sandbox",
                        action="store_true")
    parser.add_argument("-p", "--preserve", help="Do not remove temporary dir",
                        action="store_true")
    parser.add_argument("-t", "--tmpdir", help="Temporary directory",
                        default="tmp")
    parser.add_argument("--tmpfs", help="Use a tmpfs on tmpdir",
                        action="store_true")
    parser.add_argument("--tmpfs-size", help="Size of the tmpfs",
                        default="8G")
    parser.add_argument("--cores", help="Cores to use for building",
                         default=2)
    parser.add_argument("--force-timestamps",
                        help="Force all files timestamps to be 0 unix time",
                        action="store_true")
    parser.add_argument("--update-checksums",
                        help="Update checksum files.",
                        action="store_true")
    parser.add_argument("--external-sources",
                        help="Download sources externally from live-bootstrap.",
                        action="store_true")
    parser.add_argument("--build-kernels",
                        help="Also build kernels in chroot and bwrap builds.",
                        action="store_true")
    parser.add_argument("--no-create-config",
                        help="Do not automatically create config file",
                        action="store_true")
    parser.add_argument("-r", "--repo",
                        help="Path to prebuilt binary packages.", nargs=None)
    parser.add_argument("--early-preseed",
                        help="Skip early stages of live-bootstrap.", nargs=None)
    parser.add_argument("--internal-ci", help="INTERNAL for github CI")

    # QEMU arguments
    parser.add_argument("-q", "--qemu", help="Use QEMU",
                        action="store_true")
    parser.add_argument("-qc", "--qemu-cmd", help="QEMU command to run",
                        default="qemu-system-x86_64")
    parser.add_argument("-qr", "--qemu-ram", help="Memory (in megabytes) allocated to QEMU VM",
                        default=4096)
    parser.add_argument("-qk", "--kernel", help="Custom sysa kernel to use")

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
        raise ValueError("Only x86 is supported at the moment.")

    # Tmp validation
    if args.bwrap and args.tmpfs:
        raise ValueError("tmpfs cannot be used with bwrap.")

    # Cores validation
    if int(args.cores) < 1:
        raise ValueError("Must use one or more cores.")

    # bootstrap.cfg
    if args.bare_metal:
        args.no_create_config = True

    try:
        os.remove(os.path.join('sysa', 'bootstrap.cfg'))
    except FileNotFoundError:
        pass
    if not args.no_create_config:
        create_configuration_file(args)
    else:
        with open(os.path.join('sysa', 'bootstrap.cfg'), 'a', encoding='UTF-8'):
            pass

    # tmpdir
    tmpdir = Tmpdir(path=args.tmpdir, preserve=args.preserve)
    if args.tmpfs:
        tmpdir.tmpfs(size=args.tmpfs_size)

    # sys
    system_c = SysC(arch=args.arch, tmpdir=tmpdir,
                    external_sources=args.external_sources)
    system_a = SysA(arch=args.arch, early_preseed=args.early_preseed,
                    tmpdir=tmpdir, external_sources=args.external_sources,
                    repo_path=args.repo)

    bootstrap(args, system_a, system_c, tmpdir)

def bootstrap(args, system_a, system_c, tmpdir):
    """Kick off bootstrap process."""
    print(f"Bootstrapping {args.arch} -- SysA")
    if args.chroot:
        find_chroot = """
import shutil
print(shutil.which('chroot'))
"""
        chroot_binary = run('sudo', 'python3', '-c', find_chroot,
                            capture_output=True).stdout.decode().strip()

        system_c.prepare(create_disk_image=False)
        system_a.prepare(create_initramfs=False)

        arch = stage0_arch_map.get(args.arch, args.arch)
        init = os.path.join(os.sep, 'bootstrap-seeds', 'POSIX', arch, 'kaem-optional-seed')
        run('sudo', 'env', '-i', 'PATH=/bin', chroot_binary, system_a.tmp_dir, init)

    elif args.bwrap:
        if not args.internal_ci or args.internal_ci == "pass1":
            system_c.prepare(create_disk_image=False)
            system_a.prepare(create_initramfs=False)

            arch = stage0_arch_map.get(args.arch, args.arch)
            init = os.path.join(os.sep, 'bootstrap-seeds', 'POSIX', arch, 'kaem-optional-seed')
            run('bwrap', '--unshare-user',
                         '--uid', '0',
                         '--gid', '0',
                         '--unshare-net',
                         '--clearenv',
                         '--setenv', 'PATH', '/usr/bin',
                         '--bind', system_a.tmp_dir, '/',
                         '--dir', '/dev',
                         '--dev-bind', '/dev/null', '/dev/null',
                         '--dev-bind', '/dev/zero', '/dev/zero',
                         '--dev-bind', '/dev/random', '/dev/random',
                         '--dev-bind', '/dev/urandom', '/dev/urandom',
                         init)

        if not args.internal_ci or args.internal_ci == "pass2":
            run('bwrap', '--unshare-user',
                         '--uid', '0',
                         '--gid', '0',
                         '--unshare-net' if args.external_sources else None,
                         '--clearenv',
                         '--setenv', 'PATH', '/usr/bin',
                         '--bind', system_a.tmp_dir + "/sysc_image", '/',
                         '--dir', '/dev',
                         '--dev-bind', '/dev/null', '/dev/null',
                         '--dev-bind', '/dev/zero', '/dev/zero',
                         '--dev-bind', '/dev/random', '/dev/random',
                         '--dev-bind', '/dev/urandom', '/dev/urandom',
                         '--tmpfs', '/dev/shm',
                         '--proc', '/proc',
                         '--bind', '/sys', '/sys',
                         '--tmpfs', '/tmp',
                         '/init')

    elif args.bare_metal:
        if args.kernel:
            system_c.prepare(create_disk_image=True)
            system_a.prepare(create_initramfs=True)
            print("Please:")
            print("  1. Take tmp/sysa/initramfs and your kernel, boot using this.")
            print("  2. Take tmp/sysc/disk.img and put this on a writable storage medium.")
        else:
            system_a.prepare(create_initramfs=True, kernel_bootstrap=True)
            print("Please:")
            print("  1. Take tmp/sysa/sysa.img and write it to a boot drive and then boot it.")

    else:
        if args.kernel:
            system_c.prepare(create_disk_image=True)
            system_a.prepare(create_initramfs=True)

            run(args.qemu_cmd,
                '-enable-kvm',
                '-m', str(args.qemu_ram) + 'M',
                '-smp', str(args.cores),
                '-no-reboot',
                '-hda', tmpdir.get_disk("sysc"),
                '-nic', 'user,ipv6=off,model=e1000',
                '-kernel', args.kernel,
                '-initrd', system_a.initramfs_path,
                '-nographic',
                '-append', 'console=ttyS0')
        else:
            system_a.prepare(create_initramfs=True, kernel_bootstrap=True)
            run(args.qemu_cmd,
                '-enable-kvm',
                '-m', "4G",
                '-smp', str(args.cores),
                '-no-reboot',
                '-drive', 'file=' + os.path.join(system_a.tmp_dir, 'sysa.img') + ',format=raw',
                '-machine', 'kernel-irqchip=split',
                '-nic', 'user,ipv6=off,model=e1000',
                '-nographic')

if __name__ == "__main__":
    main()
