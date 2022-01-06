#!/usr/bin/env python3
"""
This file contains a few self-contained helper functions
"""

# SPDX-License-Identifier: GPL-3.0-or-later
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>

import os
import shutil
import subprocess
import sys

def run(*args, **kwargs):
    """A small wrapper around subprocess.run"""
    arguments = [str(arg) for arg in args]

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
    run('sudo', 'losetup', loop_dev, image)
    # Create the partition
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
    file_name = os.path.basename(src)
    shutil.copytree(src, os.path.join(dst, file_name), ignore=ignore)

def get_target(file_name):
    """Determine package installation directory"""
    bname = os.path.basename(file_name)

    # Remove file extension. This is not completely trivial because
    # tar archives often come with double extensions.
    first_ext = os.path.splitext(os.path.basename(bname))
    second_ext = os.path.splitext(first_ext[0])
    if second_ext[1] == '.tar':
        bname = second_ext[0]
    else:
        bname = first_ext[0]
    return bname
