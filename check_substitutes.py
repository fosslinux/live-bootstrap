#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
#
# SPDX-FileCopyrightText: 2024 fosslinux <fosslinux@aussies.space>

"""Check that substituted files are the same."""
import bz2
import filecmp
import gzip
import itertools
import lzma
import shutil
import tarfile
import tempfile
import sys
import os

from lib.generator import Generator

# Get a temporary directory to work in
working = tempfile.mkdtemp()

# Colour constants
# pylint: disable=too-few-public-methods
class Colors():
    """ANSI Color Codes"""
    GREY = "\033[90m"
    RED = "\033[91m"
    GREEN = "\033[92m"
    ORANGE = "\033[91m\033[93m"
    YELLOW = "\033[93m"
    END = "\033[0m"

def traverse_path(base_root):
    """Takes a path and returns a set of all directories and files in that path."""
    all_dirs = set()
    all_files = set()
    for root, directories, files in os.walk(base_root, topdown=True):
        for d in directories:
            all_dirs.add(os.path.join(root, d).lstrip(base_root))
        for f in files:
            all_files.add(os.path.join(root, f).lstrip(base_root))
    return (all_dirs, all_files)

class Distfile():
    """Represents one distfile and operations performed on it."""
    def __init__(self, i, url):
        self.i = i
        self.url = url
        self.out_file = f"{i}-{os.path.basename(url)}"
        self.filepath = ""

    def download(self):
        """Downloads the distfile."""
        Generator.download_file(self.url, working, self.out_file, silent=True)
        self.filepath = os.path.join(working, self.out_file)

    def decompress(self):
        """Decompresses the distfile."""
        compression = self.out_file.rsplit('.', maxsplit=1)[-1]
        decompress_func = {
            "gz": gzip.open,
            "tgz": gzip.open,
            "bz2": bz2.open,
            "xz": lzma.open,
            "lzma": lzma.open
        }
        if compression not in decompress_func:
            # No decompression needed
            return
        # Remove the compression extension
        new_path = '.'.join(self.filepath.split('.')[:-1])
        # tgz -> .tar
        if compression == "tgz":
            new_path = f"{new_path}.tar"
        # Move the decompressed binary stream to a new file
        with decompress_func[compression](self.filepath, 'rb') as fin:
            with open(new_path, 'wb') as fout:
                shutil.copyfileobj(fin, fout)
        self.filepath = new_path

    def extract(self):
        """Extracts the distfile."""
        # Sanity check
        if not tarfile.is_tarfile(self.filepath):
            return
        out_dir = os.path.join(working, f"{self.i}")
        os.mkdir(out_dir)
        with tarfile.open(self.filepath, 'r') as f:
            f.extractall(path=out_dir)
        self.filepath = out_dir

    # It makes more sense here to label them d1 and d2 rather than have one be self.
    # pylint: disable=no-self-argument
    def compare(d1, d2):
        """Compares the distfile to another distfile."""
        if not os.path.isdir(d1.filepath):
            # Compare files
            return filecmp.cmp(d1.filepath, d2.filepath, shallow=False)
        if not os.path.isdir(d2.filepath):
            # Then, d2 is a file and d1 is a directory
            return False
        # Otherwise it's two directories
        dirnames1, filenames1 = traverse_path(d1.filepath)
        dirnames2, filenames2 = traverse_path(d2.filepath)
        if dirnames1 != dirnames2:
            return False
        if filenames1 != filenames2:
            return False
        return filecmp.cmpfiles(d1.filepath, d2.filepath, filenames1, shallow=False)

def check(*args):
    """Check if a list of distfiles are equivalent."""
    notequiv = []
    # Find all pairs that are not equivalent
    for pair in itertools.combinations(args, 2):
        if pair[0].compare(pair[1]):
            print(f"{Colors.GREY}DEBUG: {pair[0].url} is equivalent to {pair[1].url}{Colors.END}")
        else:
            notequiv.append(pair)

    # Decompress all, and check again
    for d in {y for x in notequiv for y in x}:
        d.decompress()
    for pair in notequiv.copy():
        if pair[0].compare(pair[1]):
            # pylint: disable=line-too-long
            print(f"{Colors.YELLOW}NOTE: {pair[0].url} is equivalent to {pair[1].url} when decompressed{Colors.END}")
            notequiv.remove(pair)

    # Extract all, and check again
    for d in {y for x in notequiv for y in x}:
        d.extract()
    has_error = False
    for pair in notequiv:
        if pair[0].compare(pair[1]):
            # pylint: disable=line-too-long
            print(f"{Colors.ORANGE}WARN: {pair[0].url} is equivalent to {pair[1].url} when extracted{Colors.END}")
        else:
            has_error = True
            # pylint: disable=line-too-long
            print(f"{Colors.RED}ERROR: {pair[0].url} is not equivalent to {pair[1].url}!{Colors.END}")

    return has_error

def main():
    """Main function."""
    has_error = False
    with open("substitutes", 'r', encoding="utf-8") as f:
        for line in f.readlines():
            urls = line.strip().split(' ')
            distfiles = []
            for i, url in enumerate(urls):
                distfiles.append(Distfile(i, url))
            for distfile in distfiles:
                distfile.download()
            if check(*distfiles):
                has_error = True
    sys.exit(has_error)

if __name__ == "__main__":
    main()
