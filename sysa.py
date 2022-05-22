#!/usr/bin/env python3
"""System A"""
# SPDX-License-Identifier: GPL-3.0-or-later
# SPDX-FileCopyrightText: 2022 Dor Askayo <dor.askayo@gmail.com>
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 Melg Eight <public.melg8@gmail.com>
# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>

import os
from distutils.dir_util import copy_tree
import shutil

from lib.sysgeneral import SysGeneral, stage0_arch_map

# pylint: disable=consider-using-with
class SysA(SysGeneral):
    """
    Class responsible for preparing sources for System A.
    """
    # pylint: disable=too-many-instance-attributes,too-many-arguments
    def __init__(self, arch, preserve_tmp, tmpdir, sysb_dir, sysc_dir):
        self.git_dir = os.path.dirname(os.path.join(__file__))
        self.arch = arch
        self.preserve_tmp = preserve_tmp

        self.sys_dir = os.path.join(self.git_dir, 'sysa')
        if tmpdir is None:
            self.tmp_dir = os.path.join(self.git_dir, 'tmp')
        else:
            self.tmp_dir = os.path.join(tmpdir, 'sysa')
        self.sysa_dir = os.path.join(self.tmp_dir, 'sysa')
        self.base_dir = self.sysa_dir
        self.cache_dir = os.path.join(self.sys_dir, 'distfiles')
        self.sysb_dir = sysb_dir
        self.sysc_dir = sysc_dir

    def prepare(self, mount_tmpfs, create_initramfs, repo_path):
        """
        Prepare directory structure for System A.
        We create an empty tmp directory, unpack stage0-posix.
        Rest of the files are unpacked into more structured directory /sysa
        """
        if mount_tmpfs:
            self.mount_tmpfs()
        else:
            os.mkdir(self.tmp_dir)

        self.stage0_posix()
        self.sysa()

        # sysb must be added to sysa as it is another initramfs stage
        self.sysb()

        self.sysc()

        if repo_path:
            repo_dir = os.path.join(self.tmp_dir, 'usr', 'src', 'repo-preseeded')
            shutil.copytree(repo_path, repo_dir)

        if create_initramfs:
            self.make_initramfs()

    def sysa(self):
        """Copy in sysa files for sysa."""
        self.get_packages()

        shutil.copytree(self.sys_dir, os.path.join(self.tmp_dir, 'sysa'),
                ignore=shutil.ignore_patterns('tmp'))

    def sysb(self):
        """Copy in sysb files for sysb."""
        shutil.copytree(self.sysb_dir, os.path.join(self.tmp_dir, 'sysb'),
                ignore=shutil.ignore_patterns('tmp'))

    def sysc(self):
        """Copy in sysc files for sysc."""
        shutil.copytree(self.sysc_dir, os.path.join(self.tmp_dir, 'sysc'),
                ignore=shutil.ignore_patterns('tmp', 'distfiles'))

    def stage0_posix(self):
        """Copy in all of the stage0-posix"""
        stage0_posix_base_dir = os.path.join(self.sys_dir, 'stage0-posix', 'src')
        copy_tree(stage0_posix_base_dir, self.tmp_dir)

        arch = stage0_arch_map.get(self.arch, self.arch)
        kaem_optional_seed = os.path.join(self.sys_dir, 'stage0-posix', 'src', 'bootstrap-seeds',
                                          'POSIX', arch, 'kaem-optional-seed')
        shutil.copy2(kaem_optional_seed, os.path.join(self.tmp_dir, 'init'))

        # stage0-posix hook to continue running live-bootstrap
        shutil.copy2(os.path.join(self.sys_dir, 'after.kaem'),
                     os.path.join(self.tmp_dir, 'after.kaem'))

    # pylint: disable=line-too-long,too-many-statements
    def get_packages(self):
        """Prepare remaining sources"""

        # mes-0.24 snapshot
        self.get_file(["http://git.savannah.gnu.org/cgit/mes.git/snapshot/mes-aa5f1533e1736a89e60d2c34c2a0ab3b01f8d037.tar.gz",
                       "https://download.savannah.gnu.org/releases/nyacc/nyacc-1.00.2.tar.gz"],
                       output=["mes-0.24.tar.gz", "nyacc-1.00.2.tar.gz"])

        # tcc 0.9.26 patched by janneke
        self.get_file("https://lilypond.org/janneke/tcc/tcc-0.9.26-1136-g5bba73cc.tar.gz", output="tcc-0.9.26.tar.gz")

        # make 3.80
        self.get_file("https://mirrors.kernel.org/gnu/make/make-3.80.tar.gz")

        # gzip 1.2.4
        self.get_file("https://mirrors.kernel.org/gnu/gzip/gzip-1.2.4.tar.gz")

        # tar 1.12
        self.get_file("https://mirrors.kernel.org/gnu/tar/tar-1.12.tar.gz")

        # sed 4.0.9
        self.get_file("https://mirrors.kernel.org/gnu/sed/sed-4.0.9.tar.gz")

        # patch 2.5.9
        self.get_file("https://mirrors.kernel.org/gnu/patch/patch-2.5.9.tar.gz")

        # bzip2 1.0.8
        self.get_file("https://sourceware.org/pub/bzip2/bzip2-1.0.8.tar.gz")

        # tcc 0.9.27
        self.get_file("https://download.savannah.gnu.org/releases/tinycc/tcc-0.9.27.tar.bz2")

        # coreutils 5.0
        self.get_file("https://mirrors.kernel.org/gnu/coreutils/coreutils-5.0.tar.bz2")

        # heirloom-devtools
        self.get_file("http://downloads.sourceforge.net/project/heirloom/heirloom-devtools/070527/heirloom-devtools-070527.tar.bz2")

        # bash 2.05b
        self.get_file("https://mirrors.kernel.org/gnu/bash/bash-2.05b.tar.gz")

        # flex 2.5.11
        self.get_file("http://download.nust.na/pub2/openpkg1/sources/DST/flex/flex-2.5.11.tar.gz")

        # musl 1.1.24
        self.get_file("https://musl.libc.org/releases/musl-1.1.24.tar.gz")

        # m4 1.4.7
        self.get_file("https://mirrors.kernel.org/gnu/m4/m4-1.4.7.tar.gz")

        # flex 2.6.4
        self.get_file("https://github.com/westes/flex/releases/download/v2.6.4/flex-2.6.4.tar.gz")

        # bison 3.4.1
        self.get_file("https://mirrors.kernel.org/gnu/bison/bison-3.4.1.tar.gz")

        # grep 2.4
        self.get_file("https://mirrors.kernel.org/gnu/grep/grep-2.4.tar.gz")

        # diffutils 2.7
        self.get_file("https://mirrors.kernel.org/gnu/diffutils/diffutils-2.7.tar.gz")

        # coreutils 6.10
        self.get_file("https://mirrors.kernel.org/gnu/coreutils/coreutils-6.10.tar.gz")

        # gawk 3.0.4
        self.get_file("https://mirrors.kernel.org/gnu/gawk/gawk-3.0.4.tar.gz")

        # perl 5.000
        self.get_file("https://github.com/Perl/perl5/archive/perl-5.000.tar.gz")

        # perl 5.003
        self.get_file("https://github.com/Perl/perl5/archive/perl-5.003.tar.gz")

        # perl 5.004_05
        self.get_file("https://www.cpan.org/src/5.0/perl5.004_05.tar.gz",
                output="perl5.004-05.tar.gz")

        # perl 5.005_03
        self.get_file("https://www.cpan.org/src/5.0/perl5.005_03.tar.gz",
                output="perl5.005-03.tar.gz")

        # perl 5.6.2
        self.get_file("https://www.cpan.org/src/5.0/perl-5.6.2.tar.gz")

        # autoconf 2.52
        self.get_file("https://mirrors.kernel.org/gnu/autoconf/autoconf-2.52.tar.bz2")

        # automake 1.6.3
        self.get_file("https://mirrors.kernel.org/gnu/automake/automake-1.6.3.tar.bz2")

        # automake 1.4-p6
        self.get_file("https://mirrors.kernel.org/gnu/automake/automake-1.4-p6.tar.gz")

        # autoconf 2.13
        self.get_file("https://mirrors.kernel.org/gnu/autoconf/autoconf-2.13.tar.gz")

        # autoconf 2.12
        self.get_file("https://mirrors.kernel.org/gnu/autoconf/autoconf-2.12.tar.gz")

        # libtool 1.4
        self.get_file("https://mirrors.kernel.org/gnu/libtool/libtool-1.4.tar.gz")

        # binutils 2.14
        self.get_file("https://mirrors.kernel.org/gnu/binutils/binutils-2.14.tar.bz2")

        # autoconf 2.53
        self.get_file("https://mirrors.kernel.org/gnu/autoconf/autoconf-2.53.tar.bz2")

        # automake 1.7
        self.get_file("https://mirrors.kernel.org/gnu/automake/automake-1.7.tar.bz2")

        # autoconf 2.54
        self.get_file("https://mirrors.kernel.org/gnu/autoconf/autoconf-2.54.tar.bz2")

        # autoconf 2.55
        self.get_file("https://mirrors.kernel.org/gnu/autoconf/autoconf-2.55.tar.bz2")

        # automake 1.7.8
        self.get_file("https://mirrors.kernel.org/gnu/automake/automake-1.7.8.tar.bz2")

        # autoconf 2.57
        self.get_file("https://mirrors.kernel.org/gnu/autoconf/autoconf-2.57.tar.bz2")

        # autoconf 2.59
        self.get_file("https://mirrors.kernel.org/gnu/autoconf/autoconf-2.59.tar.bz2")

        # automake 1.8.5
        self.get_file("https://mirrors.kernel.org/gnu/automake/automake-1.8.5.tar.bz2")

        # help2man 1.36.4
        self.get_file("https://mirrors.kernel.org/gnu/help2man/help2man-1.36.4.tar.gz")

        # autoconf 2.61
        self.get_file("https://mirrors.kernel.org/gnu/autoconf/autoconf-2.61.tar.bz2")

        # automake 1.9.6
        self.get_file("https://mirrors.kernel.org/gnu/automake/automake-1.9.6.tar.bz2")

        # findutils 4.2.33
        self.get_file(["https://mirrors.kernel.org/gnu/findutils/findutils-4.2.33.tar.gz",
                       "https://git.savannah.gnu.org/cgit/gnulib.git/snapshot/gnulib-8e128e.tar.gz"])

        # libtool 2.2.4
        self.get_file("https://mirrors.kernel.org/gnu/libtool/libtool-2.2.4.tar.bz2")

        # automake 1.10.3
        self.get_file("https://mirrors.kernel.org/gnu/automake/automake-1.10.3.tar.bz2")

        # autoconf 2.64
        self.get_file("https://mirrors.kernel.org/gnu/autoconf/autoconf-2.64.tar.bz2")

        # gcc 4.0.4
        self.get_file(["https://mirrors.kernel.org/gnu/gcc/gcc-4.0.4/gcc-core-4.0.4.tar.bz2",
                      "https://mirrors.kernel.org/gnu/automake/automake-1.16.3.tar.gz"],
                      output=["gcc-4.0.4.tar.bz2", "automake-1.16.3.tar.gz"])

        # linux api headers 5.10.41
        self.get_file("https://mirrors.kernel.org/pub/linux/kernel/v5.x/linux-5.10.41.tar.gz",
                output="linux-headers-5.10.41.tar.gz")

        # musl 1.2.3
        self.get_file("https://musl.libc.org/releases/musl-1.2.3.tar.gz")

        # util-linux 2.19.1
        self.get_file("https://mirrors.kernel.org/pub/linux/utils/util-linux/v2.19/util-linux-2.19.1.tar.gz")

        # curl 7.83.0
        self.get_file("https://curl.se/download/curl-7.83.0.tar.bz2")

        # e2fsprogs 1.45.7
        self.get_file(["https://mirrors.edge.kernel.org/pub/linux/kernel/people/tytso/e2fsprogs/v1.45.7/e2fsprogs-1.45.7.tar.gz",
            "https://www.unicode.org/Public/11.0.0/ucd/CaseFolding.txt",
            "https://www.unicode.org/Public/11.0.0/ucd/DerivedAge.txt",
            "https://www.unicode.org/Public/11.0.0/ucd/extracted/DerivedCombiningClass.txt",
            "https://www.unicode.org/Public/11.0.0/ucd/DerivedCoreProperties.txt",
            "https://www.unicode.org/Public/11.0.0/ucd/NormalizationCorrections.txt",
            "https://www.unicode.org/Public/11.0.0/ucd/NormalizationTest.txt",
            "https://www.unicode.org/Public/11.0.0/ucd/UnicodeData.txt"])

        # dhcpcd 9.4.1
        self.get_file("https://roy.marples.name/git/dhcpcd/snapshot/dhcpcd-9.4.1.tar.gz")

        # kexec-tools 2.0.22
        self.get_file("https://github.com/horms/kexec-tools/archive/refs/tags/v2.0.22.tar.gz",
                output="kexec-tools-2.0.22.tar.gz")

        # kbd 1.15
        self.get_file("https://mirrors.edge.kernel.org/pub/linux/utils/kbd/kbd-1.15.tar.gz")

        # make 3.82
        self.get_file("https://mirrors.kernel.org/gnu/make/make-3.82.tar.gz")

        # linux kernel 2.6.16.62
        self.get_file(["https://cdn.kernel.org/pub/linux/kernel/v4.x/linux-4.9.10.tar.gz",
            "https://linux-libre.fsfla.org/pub/linux-libre/releases/old/gen6/4.9.10-gnu/deblob-4.9"])
