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
# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>

import argparse
import os
import shutil

from sysa import SysA
from sysb import SysB
from sysc import SysC
from lib.utils import run
from lib.sysgeneral import stage0_arch_map

def create_configuration_file(args):
    """
    Creates bootstrap.cfg file which would contain options used to
    customize bootstrap.
    """
    config_path = os.path.join('sysa', 'bootstrap.cfg')
    with open(config_path, "w", encoding="utf_8") as config:
        config.write("FORCE_TIMESTAMPS=" + str(args.force_timestamps) + "\n")
        config.write("CHROOT=" + str(args.chroot or args.bwrap) + "\n")
        config.write("UPDATE_CHECKSUMS=" + str(args.update_checksums) + "\n")
        config.write("DISK=sda1\n")

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
    parser.add_argument("-t", "--tmpdir", help="Temporary directory")
    parser.add_argument("--force-timestamps",
                        help="Force all files timestamps to be 0 unix time",
                        action="store_true")
    parser.add_argument("--update-checksums",
                        help="Update checksum files.",
                        action="store_true")
    parser.add_argument("--no-create-config",
                        help="Do not automatically create config file",
                        action="store_true")
    parser.add_argument("-r", "--repo",
                        help="Path to prebuilt binary packages.", nargs=None)

    # QEMU arguments
    parser.add_argument("-q", "--qemu", help="Use QEMU",
                        action="store_true")
    parser.add_argument("-qc", "--qemu-cmd", help="QEMU command to run",
                        default="qemu-system-x86_64")
    parser.add_argument("-qr", "--qemu-ram", help="Memory (in megabytes) allocated to QEMU VM",
                        default=4096)
    parser.add_argument("-qk", "--kernel", help="Kernel to use (default is ./kernel)",
                        default="kernel")

    parser.add_argument("-m", "--minikernel", help="Use minikernel",
                        action="store_true")
    parser.add_argument("-b", "--bare-metal", help="Build images for bare metal",
                        action="store_true")

    args = parser.parse_args()

    def check_types():
        count = 0
        if args.qemu:
            count += 1
        if args.chroot:
            count += 1
        if args.bwrap:
            count += 1
        if args.minikernel:
            count += 1
        if args.bare_metal:
            count += 1
        return count

    if check_types() > 1:
        raise ValueError("No more than one of qemu, chroot, minikernel, bare metal may be used.")
    if check_types() == 0:
        raise ValueError("One of qemu, chroot, minikernel or bare metal must be selected.")

    if args.bare_metal:
        args.no_create_config = True

    if args.arch != "x86":
        raise ValueError("Only x86 is supported at the moment.")

    try:
        os.remove(os.path.join('sysa', 'bootstrap.cfg'))
    except FileNotFoundError:
        pass
    if not args.no_create_config:
        create_configuration_file(args)
    else:
        with open(os.path.join('sysa', 'bootstrap.cfg'), 'a', encoding='UTF-8'):
            pass

    system_c = SysC(arch=args.arch, preserve_tmp=args.preserve,
                    tmpdir=args.tmpdir)
    system_b = SysB(arch=args.arch, preserve_tmp=args.preserve)
    system_a = SysA(arch=args.arch, preserve_tmp=args.preserve,
                    tmpdir=args.tmpdir,
                    sysb_dir=system_b.sys_dir, sysc_tmp=system_c.tmp_dir)

    bootstrap(args, system_a, system_b, system_c)

def bootstrap(args, system_a, system_b, system_c):
    """Kick off bootstrap process."""
    print(f"Bootstrapping {args.arch} -- SysA")
    if args.chroot:
        find_chroot = """
import shutil
print(shutil.which('chroot'))
"""
        chroot_binary = run('sudo', 'python3', '-c', find_chroot,
                            capture_output=True).stdout.decode().strip()

        system_c.prepare(mount_tmpfs=True,
                         create_disk_image=False)
        system_a.prepare(mount_tmpfs=True,
                         copy_sysc=True,
                         create_initramfs=False,
                         repo_path=args.repo)

        # sysa
        arch = stage0_arch_map.get(args.arch, args.arch)
        init = os.path.join(os.sep, 'bootstrap-seeds', 'POSIX', arch, 'kaem-optional-seed')
        run('sudo', 'env', '-i', 'PATH=/bin', chroot_binary, system_a.tmp_dir, init)

    elif args.bwrap:
        system_c.prepare(mount_tmpfs=False,
                         create_disk_image=False)
        system_a.prepare(mount_tmpfs=False,
                         copy_sysc=True,
                         create_initramfs=False)

        # sysa
        arch = stage0_arch_map.get(args.arch, args.arch)
        init = os.path.join(os.sep, 'bootstrap-seeds', 'POSIX', arch, 'kaem-optional-seed')
        run('bwrap', '--unshare-user',
                     '--uid', '0',
                     '--gid', '0',
                     '--cap-add', 'CAP_SYS_CHROOT', # Required for chroot from sysa to sysc
                     '--clearenv',
                     '--setenv', 'PATH', '/usr/bin',
                     '--bind', system_a.tmp_dir, '/',
                     '--dir', '/dev',
                     '--dev-bind', '/dev/null', '/dev/null',
                     '--dev-bind', '/dev/zero', '/dev/zero',
                     '--dev-bind', '/dev/random', '/dev/random',
                     '--dev-bind', '/dev/urandom', '/dev/urandom',
                     '--dir', '/sysc/dev',
                     '--dev-bind', '/dev/null', '/sysc/dev/null',
                     '--dev-bind', '/dev/zero', '/sysc/dev/zero',
                     '--dev-bind', '/dev/random', '/sysc/dev/random',
                     '--dev-bind', '/dev/urandom', '/sysc/dev/urandom',
                     '--proc', '/sysc/proc',
                     '--bind', '/sys', '/sysc/sys',
                     '--tmpfs', '/sysc/tmp',
                     init)

    elif args.minikernel:
        if os.path.isdir('kritis-linux'):
            shutil.rmtree('kritis-linux')

        system_c.prepare(mount_tmpfs=True,
                         create_disk_image=True)
        system_a.prepare(mount_tmpfs=True,
                         copy_sysc=False,
                         create_initramfs=True)

        run('git', 'clone',
            '--depth', '1', '--branch', 'v0.7',
            'https://github.com/bittorf/kritis-linux.git')
        run('kritis-linux/ci_helper.sh',
            '--private',
            '--multi', '1',
            '--repeat', '1',
            '--arch', args.arch,
            '--qemucpu', '486',
            '--kernel', '3.18.140',
            '--features', 'kflock,highrestimers',
            # Hack to add -hda /dev/blah
            '--ramsize', str(args.qemu_ram) + 'M -hda ' + system_b.dev_name,
            '--initrd', system_a.initramfs_path,
            '--log', '/tmp/bootstrap.log')

    elif args.bare_metal:
        system_c.prepare(mount_tmpfs=True,
                         create_disk_image=True)
        system_a.prepare(mount_tmpfs=True,
                         copy_sysc=False,
                         create_initramfs=True)

        print("Please:")
        print("  1. Take sysa/tmp/initramfs and your kernel, boot using this.")
        print("  2. Take sysc/tmp/disk.img and put this on a writable storage medium.")

    else:
        system_c.prepare(mount_tmpfs=True,
                         create_disk_image=True)
        system_a.prepare(mount_tmpfs=True,
                         copy_sysc=False,
                         create_initramfs=True)

        run(args.qemu_cmd,
            '-enable-kvm',
            '-m', str(args.qemu_ram) + 'M',
            '-no-reboot',
            '-hda', system_c.dev_name,
            '-kernel', args.kernel,
            '-initrd', system_a.initramfs_path,
            '-nographic',
            '-append', 'console=ttyS0')

if __name__ == "__main__":
    main()
