#!/usr/bin/env python3

# SPDX-FileCopyrightText: 2023 fosslinux <fosslinux@aussies.space>
# SPDX-License-Identifier: GPL-3.0-or-later

"""
Contains a class that represents a tmpdir
"""

import enum
import getpass
import os
import shutil

from lib.utils import mount, umount, create_disk, run_as_root

class TmpType(enum.Enum):
    """Different types of tmpdirs we can have"""
    NONE = 0
    TMPFS = 1

class Tmpdir:
    """
    Represents a tmpdir
    """

    _disks = {}
    _disk_filesystems = {}
    _mountpoints = {}

    def __init__(self, preserve, path="tmp"):
        self.path = os.path.abspath(path)
        self.preserve = preserve
        self._type = TmpType.NONE

        if not os.path.exists(self.path):
            os.mkdir(self.path)

    def __del__(self):
        for path in self._mountpoints:
            print(f"Unmounting {path}")
            umount(path)

        if not self.preserve:
            for disk in self._disks.values():
                print(f"Detaching {disk}")
                run_as_root("losetup", "-d", disk)

            if self._type == TmpType.TMPFS:
                print(f"Unmounting tmpdir from {self.path}")
                umount(self.path)

            print(f"Removing {self.path}")
            shutil.rmtree(self.path, ignore_errors=True)

    def tmpfs(self, size="8G"):
        """Mount a tmpfs"""
        print(f"Mounting tmpfs on {self.path}")
        mount("tmpfs", self.path, "tmpfs", f"size={size}")
        self._type = TmpType.TMPFS

    # pylint: disable=too-many-arguments
    def add_disk(self, name, size="16G", filesystem="ext4", tabletype="msdos", mkfs_args=None):
        """Add a disk"""
        disk_path = os.path.join(self.path, f"{name}.img")
        self._disks[name] = create_disk(disk_path, tabletype, filesystem, size, mkfs_args=mkfs_args)
        self._disk_filesystems[name] = filesystem
        # Allow executing user to access it
        run_as_root("chown", getpass.getuser(), self._disks[name])

    def mount_disk(self, name, mountpoint=None):
        """Mount the disk"""
        if mountpoint is None:
            mountpoint = f"{name}_mnt"
        mountpoint = os.path.join(self.path, mountpoint)
        os.mkdir(mountpoint)
        mount(self._disks[name] + "p1", mountpoint, self._disk_filesystems[name])
        # Allow executing user to access it
        run_as_root("chown", getpass.getuser(), mountpoint)
        self._mountpoints[name] = mountpoint
        return mountpoint

    def umount_disk(self, name):
        """Unmount a disk"""
        umount(self._mountpoints[name])
        del self._mountpoints[name]

    def get_disk(self, name):
        """Get the path to a device of a disk"""
        return self._disks[name]
