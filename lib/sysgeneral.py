#!/usr/bin/env python3
"""
This file contains a few functions to be shared by all Sys* classes
"""

# SPDX-FileCopyrightText: 2022-2023 Dor Askayo <dor.askayo@gmail.com>
# SPDX-FileCopyrightText: 2021-23 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-License-Identifier: GPL-3.0-or-later

import os
import hashlib
import glob
import subprocess

import requests

class SysGeneral:
    """
    A class from which all Sys* class are extended.
    Contains functions used in all Sys*
    """

    # All of these are variables defined in the individual Sys* classes
    cache_dir = None
    base_dir = None
    git_dir = None
    sys_dir = None
    initramfs_path = None
    tmp_dir = None

    def check_file(self, file_name, expected_hash):
        """Check hash of downloaded source file."""
        with open(file_name, "rb") as downloaded_file:
            downloaded_content = downloaded_file.read() # read entire file as bytes
        readable_hash = hashlib.sha256(downloaded_content).hexdigest()
        if expected_hash == readable_hash:
            return
        raise Exception(f"Checksum mismatch for file {os.path.basename(file_name)}:\n\
expected: {expected_hash}\n\
actual:   {readable_hash}\n\
When in doubt, try deleting the file in question -- it will be downloaded again when running \
this script the next time")

    def download_file(self, url, directory, file_name):
        """
        Download a single source archive.
        """
        abs_file_name = os.path.join(directory, file_name)

        # Create a directory for downloaded file
        if not os.path.isdir(directory):
            os.mkdir(directory)

        # Actually download the file
        headers = {
                "Accept-Encoding": "identity"
        }
        if not os.path.isfile(abs_file_name):
            print(f"Downloading: {file_name}")
            response = requests.get(url, allow_redirects=True, stream=True,
                    headers=headers, timeout=20)
            if response.status_code == 200:
                with open(abs_file_name, 'wb') as target_file:
                    target_file.write(response.raw.read())
            else:
                raise Exception("Download failed.")
        return abs_file_name

    def get_packages(self, source_manifest):
        """Prepare remaining sources"""
        for line in source_manifest.split("\n"):
            line = line.strip().split(" ")

            path = self.download_file(line[2], line[1], line[3])
            self.check_file(path, line[0])

    @classmethod
    def get_source_manifest(cls):
        """
        Generate a source manifest for the system.
        """
        manifest_lines = []
        directory = os.path.relpath(cls.cache_dir, cls.git_dir)

        # Find all source files
        for file in os.listdir(cls.sys_dir):
            if os.path.isdir(os.path.join(cls.sys_dir, file)):
                sourcef = os.path.join(cls.sys_dir, file, "sources")
                if os.path.exists(sourcef):
                    # Read sources from the source file
                    with open(sourcef, "r", encoding="utf_8") as sources:
                        for line in sources.readlines():
                            line = line.strip().split(" ")

                            if len(line) > 2:
                                file_name = line[2]
                            else:
                                # Automatically determine file name based on URL.
                                file_name = os.path.basename(line[0])

                            manifest_lines.append(f"{line[1]} {directory} {line[0]} {file_name}")

        return "\n".join(manifest_lines)

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

stage0_arch_map = {
    "amd64": "AMD64",
}
