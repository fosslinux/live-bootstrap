#!/usr/bin/env python3
"""System C"""
# SPDX-License-Identifier: GPL-3.0-or-later
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space> 

import os
import shutil
import getpass

from lib.utils import mount, umount, copytree, create_disk, run
from lib.sysgeneral import SysGeneral

class SysC(SysGeneral):
    """
    Class responsible for preparing sources for System C.
    """
    def __init__(self, arch, preserve_tmp, tmpdir, chroot):
        self.git_dir = os.path.dirname(os.path.join(__file__))
        self.arch = arch
        self.preserve_tmp = preserve_tmp
        self.chroot = chroot

        self.sys_dir = os.path.join(self.git_dir, 'sysc')
        if tmpdir is None:
            self.tmp_dir = os.path.join(self.sys_dir, 'tmp')
        else:
            self.tmp_dir = os.path.join(tmpdir, 'sysc')
            os.mkdir(self.tmp_dir)

        self.prepare()

    def __del__(self):
        if not self.preserve_tmp:
            if not self.chroot:
                print("Deleting %s" % (self.dev_name))
                run('sudo', 'losetup', '-d', self.dev_name)
            print("Unmounting tmpfs from %s" % (self.tmp_dir))
            umount(self.tmp_dir)
            os.rmdir(self.tmp_dir)

    def prepare(self):
        """
        Prepare directory structure for System C.
        """
        self.mount_tmpfs()
        if not self.chroot:
            # Create + mount a disk for QEMU to use
            disk_path = os.path.join(self.tmp_dir, 'disk.img')
            self.dev_name = create_disk(disk_path, "msdos", "ext2", '8G')
            self.rootfs_dir = os.path.join(self.tmp_dir, 'mnt')
            os.mkdir(self.rootfs_dir)
            mount(self.dev_name + "p1", self.rootfs_dir, 'ext2')
            # Use chown to allow executing user to access it
            run('sudo', 'chown', getpass.getuser(), self.dev_name)
            run('sudo', 'chown', getpass.getuser(), self.rootfs_dir)
        else:
            self.rootfs_dir = self.tmp_dir
 
        # Expand to the full base dir
        self.base_dir = os.path.join(self.rootfs_dir, 'usr', 'src')
        os.makedirs(self.base_dir)

        # Misc files/scripts
        self.deploy_scripts()
        self.deploy_sysglobal_files()

        self.get_packages()

        # Unmount tmp/mnt if it exists
        if not self.chroot:
            umount(self.rootfs_dir)

    def chroot_transition(sysb_tmp, self):
        # See create_sysb in sysb/run.sh
        # We skip sysb when using chroot, as sysb is entirely irrelevant
        # to chrooting (only for kernel shenanigans)
        # Copy directories from /after (sysa) -> /usr (sysc)
        usr_dirs = ['bin', 'include', 'lib', 'sbin', 'share']
        for d in usr_dirs:
            copy_tree(os.path.join(sysa_tmp, 'after', d),
                      os.path.join(self.rootfs_dir, 'usr'))
        # Copy /boot
        copy_tree(os.path.join(sysa_tmp, 'after', 'boot'),
                  os.path.join(self.rootfs_dir, 'boot'))

    def deploy_scripts(self):
        """Add the scripts to the chroot"""
        src_files = ['run.sh', 'run2.sh']
        for f in src_files:
            shutil.copy2(os.path.join(self.sys_dir, f),
                         os.path.join(self.base_dir, f))
        # init script
        os.mkdir(os.path.join(self.rootfs_dir, 'sbin'))
        shutil.copy2(os.path.join(self.sys_dir, 'init'), self.rootfs_dir)

    # pylint: disable=line-too-long,too-many-statements
    def get_packages(self):
        """Prepare remaining sources"""
        # bash 5.1
        self.get_file("https://mirrors.kernel.org/gnu/bash/bash-5.1.tar.gz")

        # xz 5.0.5
        self.get_file("https://tukaani.org/xz/xz-5.0.5.tar.bz2")

        # automake 1.11.2
        self.get_file("https://mirrors.kernel.org/gnu/automake/automake-1.11.2.tar.bz2")

        # autoconf 2.69
        self.get_file("https://mirrors.kernel.org/gnu/autoconf/autoconf-2.69.tar.xz")

        # automake 1.15.1
        self.get_file("https://mirrors.kernel.org/gnu/automake/automake-1.15.1.tar.xz")

        # tar 1.34
        self.get_file(["https://mirrors.kernel.org/gnu/tar/tar-1.34.tar.xz",
                       "https://git.savannah.gnu.org/cgit/gnulib.git/snapshot/gnulib-30820c.tar.gz"])

        # coreutils 8.32
        self.get_file(["https://git.savannah.gnu.org/cgit/coreutils.git/snapshot/coreutils-8.32.tar.gz",
                       "https://git.savannah.gnu.org/cgit/gnulib.git/snapshot/gnulib-d279bc.tar.gz"])

        # make 4.2.1
        self.get_file("https://ftp.gnu.org/gnu/make/make-4.2.1.tar.gz")

        # gmp 6.2.1
        self.get_file("https://mirrors.kernel.org/gnu/gmp/gmp-6.2.1.tar.xz")

        # autoconf archive 2021.02.19
        self.get_file("https://mirrors.kernel.org/gnu/autoconf-archive/autoconf-archive-2021.02.19.tar.xz")

        # mpfr 4.1.0
        self.get_file("https://mirrors.kernel.org/gnu/mpfr/mpfr-4.1.0.tar.xz")

        # mpc 1.2.1
        self.get_file("https://mirrors.kernel.org/gnu/mpc/mpc-1.2.1.tar.gz")

        # flex 2.5.33
        self.get_file("http://download.nust.na/pub2/openpkg1/sources/DST/flex/flex-2.5.33.tar.gz")

        # bison 2.3
        self.get_file(["https://mirrors.kernel.org/gnu/bison/bison-2.3.tar.bz2",
                       "https://git.savannah.gnu.org/cgit/gnulib.git/snapshot/gnulib-b28236b.tar.gz"])

        # bison 3.4.2
        self.get_file(["https://mirrors.kernel.org/gnu/bison/bison-3.4.2.tar.xz",
                       "https://git.savannah.gnu.org/cgit/gnulib.git/snapshot/gnulib-672663a.tar.gz"])

        # perl 5.10.5
        self.get_file("https://www.cpan.org/src/5.0/perl-5.10.1.tar.bz2")

        # dist 3.5-236
        # Debian's version is used because upstream is not to be found (dead?)
        self.get_file("https://salsa.debian.org/perl-team/interpreter/dist/-/archive/d1de81f/dist-d1de81f.tar.gz",
                      output="dist-3.5-236.tar.gz")

        # perl 5.32.1
        self.get_file(["https://www.cpan.org/src/5.0/perl-5.32.1.tar.xz",
                       "https://salsa.debian.org/perl-team/interpreter/perl/-/archive/5f2dc80/perl-5f2dc80.tar.bz2"])
