#!/usr/bin/env python3
"""
This file contains a few functions to be shared by all Sys* classes
"""

# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-License-Identifier: GPL-3.0-or-later

import os
import hashlib
import shutil
import glob
import subprocess

import requests

from lib.utils import mount, umount, get_target, copytree

class SysGeneral:
    """
    A class from which all Sys* class are extended.
    Contains functions used in all Sys*
    """

    # All of these are variables defined in the individual Sys* classes
    preserve_tmp = None
    tmp_dir = None
    base_dir = None
    git_dir = None
    sys_dir = None
    initramfs_path = None

    def __del__(self):
        if not self.preserve_tmp:
            print("Unmounting tmpfs from %s" % (self.tmp_dir))
            umount(self.tmp_dir)
            os.rmdir(self.tmp_dir)

    def mount_tmpfs(self):
        """Mount the tmpfs for this sysx"""
        if not os.path.isdir(self.tmp_dir):
            os.mkdir(self.tmp_dir)
        print("Mounting tmpfs on %s" % (self.tmp_dir))
        mount('tmpfs', self.tmp_dir, 'tmpfs', 'size=8G')

    def check_file(self, file_name):
        """Check hash of downloaded source file."""
        checksum_store = os.path.join(self.git_dir, 'SHA256SUMS.sources')
        with open(checksum_store, encoding="utf_8") as checksum_file:
            hashes = checksum_file.read().splitlines()
        for hash_line in hashes:
            if os.path.basename(file_name) in hash_line:
                # Hash is in store, check it
                expected_hash = hash_line.split()[0]

                with open(file_name, "rb") as downloaded_file:
                    downloaded_content = downloaded_file.read() # read entire file as bytes
                readable_hash = hashlib.sha256(downloaded_content).hexdigest()
                if expected_hash == readable_hash:
                    return
                raise Exception("Checksum mismatch")

        raise Exception("File checksum is not yet recorded")

    def download_file(self, url, file_name=None):
        """
        Download a single source archive.
        """
        cache_dir = os.path.join(self.git_dir, 'sources')

        # Automatically determine file name based on URL.
        if file_name is None:
            file_name = os.path.basename(url)
        abs_file_name = os.path.join(cache_dir, file_name)

        # Create a cache directory for downloaded sources
        if not os.path.isdir(cache_dir):
            os.mkdir(cache_dir)

        # Actually download the file
        if not os.path.isfile(abs_file_name):
            print("Downloading: %s" % (file_name))
            request = requests.get(url, allow_redirects=True)
            # pylint: disable=consider-using-with
            open(abs_file_name, 'wb').write(request.content)

        # Check SHA256 hash
        self.check_file(abs_file_name)
        return abs_file_name

    def get_file(self, url, mkbuild=False, output=None):
        """
        Download and prepare source packages

        url can be either:
          1. a single URL
          2. list of URLs to download. In this case the first URL is the primary URL
             from which we derive the name of package directory
        output can be used to override file name of the downloaded file(s).

        mkbuild=True can be used to pre-create build directories before
        mkdir is available.
        """
        # Single URL
        if isinstance(url, str):
            assert output is None or isinstance(output, str)
            file_name = url if output is None else output
            urls = [url]
            outputs = [output]
        # Multiple URLs
        elif isinstance(url, list):
            assert output is None or len(output) == len(url)
            file_name = url[0] if output is None else output[0]
            urls = url
            outputs = output if output is not None else [None] * len(url)
        else:
            raise TypeError("url must be either a string or a list of strings")
        # Determine installation directory
        target_name = get_target(file_name)
        target_src_dir = os.path.join(self.base_dir, target_name, 'src')
        # Install base files
        src_tree = os.path.join(self.sys_dir, target_name)
        copytree(src_tree, self.base_dir)
        if not os.path.isdir(target_src_dir):
            os.mkdir(target_src_dir)
        for i, _ in enumerate(urls):
            # Download files into cache directory
            tarball = self.download_file(urls[i], outputs[i])
            # Install sources into target directory
            shutil.copy2(tarball, target_src_dir)
        if mkbuild:
            os.mkdir(os.path.join(self.base_dir, target_name, 'build'))

    def deploy_sysglobal_files(self):
        """Deploy files common to all Sys*"""
        sysglobal_files = ['bootstrap.cfg', 'helpers.sh']
        for file in sysglobal_files:
            shutil.copy2(os.path.join(self.git_dir, 'sysglobal', file),
                         self.base_dir)

    def make_initramfs(self):
        """Package binary bootstrap seeds and sources into initramfs."""
        self.initramfs_path = os.path.join(self.tmp_dir, 'initramfs')

        # Create a list of files to go within the initramfs
        file_list = glob.glob(os.path.join(self.tmp_dir, '**'), recursive=True)

        # Use built-in removeprefix once we can use Python 3.9
        def remove_prefix(text, prefix):
            if text.startswith(prefix):
                return text[len(prefix):]
            return text  # or whatever

        file_list = [remove_prefix(f, self.tmp_dir + os.sep) for f in file_list]

        # Create the initramfs
        with open(self.initramfs_path, "w", encoding="utf_8") as initramfs:
            # pylint: disable=consider-using-with
            cpio = subprocess.Popen(
                    ["cpio", "--format", "newc", "--create",
                        "--directory", self.tmp_dir],
                     stdin=subprocess.PIPE, stdout=initramfs)
            cpio.communicate(input='\n'.join(file_list).encode())
