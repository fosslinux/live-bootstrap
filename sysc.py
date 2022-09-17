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

    dev_name = None

    def __init__(self, arch, preserve_tmp, tmpdir, external_sources):
        self.git_dir = os.path.dirname(os.path.join(__file__))
        self.arch = arch
        self.preserve_tmp = preserve_tmp
        self.external_sources = external_sources

        self.sys_dir = os.path.join(self.git_dir, 'sysc')
        self.cache_dir = os.path.join(self.sys_dir, 'distfiles')
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

    # pylint: disable=line-too-long,too-many-statements
    def get_packages(self):
        """Prepare remaining sources"""
        # bash 5.1
        self.get_file("https://mirrors.kernel.org/gnu/bash/bash-5.1.tar.gz")

        # xz 5.0.5
        self.get_file("https://ixpeering.dl.sourceforge.net/project/lzmautils/xz-5.0.5.tar.bz2")

        # automake 1.11.2
        self.get_file("https://mirrors.kernel.org/gnu/automake/automake-1.11.2.tar.bz2")

        # libtool 2.4.7
        self.get_file(["https://mirrors.kernel.org/gnu/libtool/libtool-2.4.7.tar.xz",
                       "https://git.savannah.gnu.org/cgit/gnulib.git/snapshot/gnulib-a521820.tar.gz"])

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

        # pkg-config 0.29.2
        self.get_file("http://gentoo.osuosl.org/distfiles/pkg-config-0.29.2.tar.gz")

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
        self.get_file("http://deb.debian.org/debian/pool/main/d/dist/dist_3.5-236.orig.tar.gz")

        # perl 5.32.1
        self.get_file(["https://www.cpan.org/src/5.0/perl-5.32.1.tar.xz",
                       "http://deb.debian.org/debian/pool/main/p/perl/perl_5.32.1.orig-regen-configure.tar.gz"])

        # libarchive-3.5.2
        self.get_file("https://libarchive.org/downloads/libarchive-3.5.2.tar.xz")

        # openssl-1.1.1l
        self.get_file("https://artfiles.org/openssl.org/source/old/1.1.1/openssl-1.1.1l.tar.gz")

        # curl 7.83.0
        self.get_file("https://master.dl.sourceforge.net/project/curl.mirror/curl-7_83_0/curl-7.83.0.tar.xz")

        # ca-certificates-3.78
        self.get_file("https://ftp.mozilla.org/pub/security/nss/releases/NSS_3_78_RTM/src/nss-3.78.tar.gz")

        # xbps 0.59.1
        self.get_file("https://github.com/void-linux/xbps/archive/refs/tags/0.59.1.tar.gz")

        # autoconf 2.71
        self.get_file("https://mirrors.kernel.org/gnu/autoconf/autoconf-2.71.tar.xz")

        # automake 1.16.3
        self.get_file("https://mirrors.kernel.org/gnu/automake/automake-1.16.3.tar.xz")

        # patch 2.7.6
        self.get_file(["https://mirrors.kernel.org/gnu/patch/patch-2.7.6.tar.xz",
                       "https://git.savannah.gnu.org/cgit/gnulib.git/snapshot/gnulib-e017871.tar.gz"])

        # gettext 0.21
        self.get_file(["https://mirrors.kernel.org/gnu/gettext/gettext-0.21.tar.xz",
                       "https://git.savannah.gnu.org/cgit/gnulib.git/snapshot/gnulib-7daa86f.tar.gz"])

        # texinfo 6.7
        self.get_file(["https://mirrors.kernel.org/gnu/texinfo/texinfo-6.7.tar.xz",
                       "https://git.savannah.gnu.org/cgit/gnulib.git/snapshot/gnulib-b81ec69.tar.gz"])

        # zlib 1.2.12
        self.get_file("https://www.zlib.net/zlib-1.2.12.tar.xz")

        # gcc 4.7.4
        self.get_file("https://mirrors.kernel.org/gnu/gcc/gcc-4.7.4/gcc-4.7.4.tar.bz2")

        # binutils 2.38
        self.get_file("https://mirrors.kernel.org/gnu/binutils/binutils-2.38.tar.xz")

        # musl 1.2.3
        self.get_file("https://musl.libc.org/releases/musl-1.2.3.tar.gz")

        # gperf 3.1
        self.get_file("https://mirrors.kernel.org/gnu/gperf/gperf-3.1.tar.gz")

        # libunistring 0.9.10
        self.get_file(["https://mirrors.kernel.org/gnu/libunistring/libunistring-0.9.10.tar.xz",
                       "https://git.savannah.gnu.org/cgit/gnulib.git/snapshot/gnulib-52a06cb3.tar.gz"])

        # libffi 3.3
        self.get_file("https://github.com/libffi/libffi/releases/download/v3.3/libffi-3.3.tar.gz")

        # libatomic_ops 7.6.10
        self.get_file("https://github.com/ivmai/libatomic_ops/releases/download/v7.6.10/libatomic_ops-7.6.10.tar.gz")

        # boehm-gc 8.0.4
        self.get_file("https://www.hboehm.info/gc/gc_source/gc-8.0.4.tar.gz")

        # guile 3.0.7
        self.get_file(["https://mirrors.kernel.org/gnu/guile/guile-3.0.7.tar.xz",
                       "https://git.savannah.gnu.org/cgit/gnulib.git/snapshot/gnulib-901694b9.tar.gz",
                       "https://github.com/schierlm/guile-psyntax-bootstrapping/archive/refs/tags/guile-3.0.7.tar.gz"])

        # which 2.21
        self.get_file("https://carlowood.github.io/which/which-2.21.tar.gz")
