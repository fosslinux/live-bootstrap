#!/usr/bin/env python3
"""System C"""
# SPDX-License-Identifier: GPL-3.0-or-later
# SPDX-FileCopyrightText: 2022-2023 Dor Askayo <dor.askayo@gmail.com>
# SPDX-FileCopyrightText: 2021-23 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>

import os

from lib.utils import copytree
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

    def __init__(self, tmpdir, arch, external_sources):
        self.arch = arch
        self.external_sources = external_sources
        self._tmpdir = tmpdir

        self.tmp_dir = tmpdir.add_sys("sysc")

    def prepare(self, create_disk_image):
        """
        Prepare directory structure for System C.
        """
        if create_disk_image:
            self._tmpdir.add_disk("sysc")

        if self.external_sources:
            if create_disk_image:
                rootfs_dir = self._tmpdir.mount_disk("sysc", size="16G")
            else:
                rootfs_dir = self.tmp_dir
            source_manifest = self.get_source_manifest()
            self.get_packages(source_manifest)

            copytree(self.cache_dir, os.path.join(rootfs_dir, "distfiles"))

            if create_disk_image:
                self._tmpdir.umount_disk("sysc")
