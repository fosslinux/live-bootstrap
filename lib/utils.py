#!/usr/bin/env python3
"""
This file contains a few self-contained helper functions
"""

# SPDX-License-Identifier: GPL-3.0-or-later
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021-23 fosslinux <fosslinux@aussies.space>

import os
import shutil
import subprocess
import sys

def run(*args, **kwargs):
    """A small wrapper around subprocess.run"""
    arguments = [str(arg) for arg in args if arg is not None]

    if kwargs.pop('verbose', False):
        print(arguments)

    try:
        return subprocess.run(arguments, check=True, **kwargs)
    except subprocess.CalledProcessError:
        print("Bootstrapping failed")
        sys.exit(1)

def run_as_root(*args, **kwargs):
    """A helper for run that invokes sudo when unprivileged"""
    if os.geteuid() != 0:
        return run("sudo", *args, **kwargs)
    return run(*args, **kwargs)

# pylint: disable=too-many-arguments,too-many-positional-arguments
def create_disk(image, disk_type, fs_type, size, bootable=False, mkfs_args=None):
    """Create a disk image, with a filesystem on it"""
    if mkfs_args is None:
        mkfs_args = []
    run('truncate', '-s', size, image)
    # Create the partition
    if disk_type != "none":
        # 1 GiB if bootable, 1 MiB otherwise
        offset = str(1024 * 1024 * (1024 if bootable else 1))
        run('parted', '--script', image, 'mklabel', disk_type, 'mkpart',
            'primary', fs_type, offset + 'B', '100%')
        run('mkfs.' + fs_type, image, '-E', 'offset=' + offset, *mkfs_args)

def mount(source, target, fs_type, options='', **kwargs):
    """Mount filesystem"""
    run_as_root('mount', source, target, '-t', fs_type, '-o', options, **kwargs)

def umount(target, **kwargs):
    """Unmount filesystem"""
    run_as_root('umount', '--recursive', target, **kwargs)

def copytree(src, dst, ignore=shutil.ignore_patterns('*.git*')):
    """Copy directory tree into another directory"""
    if not os.path.exists(dst):
        os.makedirs(dst)
    lst = os.listdir(src)
    if ignore:
        excl = ignore(src, lst)
        lst = [x for x in lst if x not in excl]
    for item in lst:
        source = os.path.join(src, item)
        dest = os.path.join(dst, item)
        if os.path.isdir(source):
            copytree(source, dest, ignore)
        else:
            shutil.copy2(source, dest)
