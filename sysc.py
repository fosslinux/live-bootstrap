#!/usr/bin/env python3
"""System C"""
# SPDX-License-Identifier: GPL-3.0-or-later
# SPDX-FileCopyrightText: 2022 Dor Askayo <dor.askayo@gmail.com>
# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>

import os
import getpass

from lib.utils import mount, umount, create_disk, run, copytree
from lib.sysgeneral import SysGeneral

# pylint: disable=consider-using-with
# pylint: disable=too-many-instance-attributes
class SysC(SysGeneral):
    """
    Class responsible for preparing sources for System C.
    """

    git_dir = os.path.dirname(os.path.join(__file__))
    sys_dir = os.path.join(git_dir, 'sysc')
    cache_dir = os.path.join(sys_dir, 'distfiles')
    dev_name = None

    def __init__(self, arch, preserve_tmp, tmpdir, external_sources):
        self.arch = arch
        self.preserve_tmp = preserve_tmp
        self.external_sources = external_sources

        if tmpdir is None:
            self.tmp_dir = os.path.join(self.sys_dir, 'tmp')
        else:
            self.tmp_dir = os.path.join(tmpdir, 'sysc')

    def __del__(self):
        if not self.preserve_tmp:
            if self.dev_name is not None:
                print(f"Detaching {self.dev_name}")
                run('sudo', 'losetup', '-d', self.dev_name)

        super().__del__()

    def prepare(self, mount_tmpfs, create_disk_image):
        """
        Prepare directory structure for System C.
        """
        if mount_tmpfs:
            self.mount_tmpfs()
        else:
            os.mkdir(self.tmp_dir)

        rootfs_dir = None

        if create_disk_image:
            # Create + mount a disk for QEMU to use
            disk_path = os.path.join(self.tmp_dir, 'disk.img')
            if self.external_sources:
                self.dev_name = create_disk(disk_path, "msdos", "ext4", '8G')
                rootfs_dir = os.path.join(self.tmp_dir, 'mnt')
                os.mkdir(rootfs_dir)
                mount(self.dev_name + "p1", rootfs_dir, 'ext4')
            else:
                self.dev_name = create_disk(disk_path, "none", "ext4", '8G')
            # Use chown to allow executing user to access it
            run('sudo', 'chown', getpass.getuser(), self.dev_name)
            if self.external_sources:
                run('sudo', 'chown', getpass.getuser(), rootfs_dir)
        else:
            rootfs_dir = self.tmp_dir

        if self.external_sources:
            self.get_packages()
            copytree(self.cache_dir, os.path.join(rootfs_dir, "distfiles"))

        # Unmount tmp/mnt if it was mounted
        if create_disk_image and self.external_sources:
            umount(rootfs_dir)
