#!/usr/bin/env python3

# SPDX-FileCopyrightText: 2023 fosslinux <fosslinux@aussies.space>
# SPDX-License-Identifier: GPL-3.0-or-later

"""
Contains a class that represents a target directory
"""

import enum
import os

from lib.utils import mount, create_disk

class TargetType(enum.Enum):
    """Different types of target dirs we can have"""
    NONE = 0
    TMPFS = 1

class Target:
    """
    Represents a target directory
    """

    _disks = {}
    _mountpoints = {}

    def __init__(self, path="target"):
        self.path = os.path.abspath(path)
        self._type = TargetType.NONE

        if not os.path.exists(self.path):
            os.mkdir(self.path)

    def tmpfs(self, size="8G"):
        """Mount a tmpfs"""
        print(f"Mounting tmpfs on {self.path}")
        mount("tmpfs", self.path, "tmpfs", f"size={size}")
        self._type = TargetType.TMPFS

    # pylint: disable=too-many-arguments,too-many-positional-arguments
    def add_disk(self,
                 name,
                 size="16G",
                 filesystem="ext4",
                 tabletype="msdos",
                 bootable=False,
                 mkfs_args=None):
        """Add a disk"""
        disk_path = os.path.join(self.path, f"{name}.img")
        create_disk(disk_path,
                    tabletype,
                    filesystem,
                    size,
                    bootable,
                    mkfs_args)
        self._disks[name] = disk_path

    def get_disk(self, name):
        """Get the path to a device of a disk"""
        return self._disks.get(name)
