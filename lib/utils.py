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

def create_disk(image, disk_type, fs_type, size):
    """Create a disk image, with a filesystem on it"""
    run('truncate', '-s', size, image)
    # First find the device we will use, then actually use it
    loop_dev = run('sudo', 'losetup', '-f', capture_output=True).stdout.decode().strip()
    run('sudo', 'losetup', '-P', loop_dev, image)
    # Create the partition
    if disk_type != "none":
        run('sudo', 'parted', '--script', image, 'mklabel', disk_type, 'mkpart',
                'primary', 'ext4', '0%', '100%')
        run('sudo', 'partprobe', loop_dev)
        run('sudo', 'mkfs.' + fs_type, loop_dev + "p1")
    return loop_dev

def mount(source, target, fs_type, options='', **kwargs):
    """Mount filesystem"""
    run('sudo', 'mount', source, target, '-t', fs_type, '-o', options, **kwargs)

def umount(target, **kwargs):
    """Unmount filesystem"""
    run('sudo', 'umount', '--recursive', target, **kwargs)

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
