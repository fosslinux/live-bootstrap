#!/usr/bin/env python3
"""
A helper application used to start bootstrapping process.
It has a few modes of operation, you can create initramfs with
binary seeds and sources that you can boot into or alternatively
you can run bootstap inside chroot.
"""

# SPDX-License-Identifier: GPL-3.0-or-later
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>

import argparse
import glob
import os
import subprocess

from sysa import SysA
from lib.utils import run

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
    parser.add_argument("-p", "--preserve", help="Do not unmount temporary dir",
                        action="store_true")
    parser.add_argument("-t", "--tmpdir", help="Temporary directory")

    # QEMU arguments
    parser.add_argument("-q", "--qemu-cmd", help="QEMU command",
                        default="qemu-system-x86_64")
    parser.add_argument("-r", "--qemu-ram", help="Memory (in megabytes) allocated to QEMU VM",
                        default=8000)
    parser.add_argument("-k", "--kernel", help="Kernel to use (default is ./kernel)",
                        default="kernel")

    parser.add_argument("-m", "--minikernel", help="Use minikernel",
                        action="store_true")

    args = parser.parse_args()
    if args.chroot and args.minikernel:
        raise ValueError("chroot and minikernel options cannot be used simultaneously.")

    if args.arch != "x86":
        raise ValueError("Only x86 is supported at the moment.")

    system_a = SysA(arch=args.arch, preserve_tmp=args.preserve, tmpdir=args.tmpdir)
    initramfs_path = os.path.join(system_a.tmp_dir, "initramfs")

    if not args.chroot:
        make_initramfs(system_a.tmp_dir, initramfs_path)

    bootstrap(args, system_a.tmp_dir, initramfs_path)

def make_initramfs(tmp_dir, initramfs_path):
    """Package binary bootstrap seeds and sources into initramfs."""
    file_list = glob.glob(os.path.join(tmp_dir, '**'), recursive=True)

    # Use built-in removeprefix once we can use Python 3.9
    def remove_prefix(text, prefix):
        if text.startswith(prefix):
            return text[len(prefix):]
        return text  # or whatever

    file_list = [remove_prefix(f, tmp_dir + os.sep) for f in file_list]

    with open(initramfs_path, "w") as initramfs:
        cpio = subprocess.Popen(["cpio", "--format", "newc", "--create", "--directory", tmp_dir],
                             stdin=subprocess.PIPE, stdout=initramfs)
        cpio.communicate(input='\n'.join(file_list).encode())

def bootstrap(args, tmp_dir, initramfs_path):
    """Kick off bootstrap process."""
    print("Bootstrapping %s" % (args.arch))
    if args.chroot:
        init = os.path.join(os.sep, 'bootstrap-seeds', 'POSIX', args.arch, 'kaem-optional-seed')
        run('sudo', 'env', '-i', 'PATH=/bin', 'chroot', tmp_dir, init)
        return

    if args.minikernel:
        run('git', 'clone', '--depth', '1', '--branch', 'v0.4',
            'https://github.com/bittorf/kritis-linux.git')
        run('kritis-linux/ci_helper.sh', '--arch', 'x86_64', '--ramsize',
            '-m', str(args.qemu_ram) + 'M', '--kernel', '5.10.8', '--initrd', initramfs_path)
        return

    run(args.qemu_cmd,
        '-enable-kvm',
        '-m', str(args.qemu_ram) + 'M',
        '-nographic',
        '-no-reboot',
        '-kernel', args.kernel,
        '-initrd', initramfs_path,
        '-append', "console=ttyS0")

if __name__=="__main__":
    main()
