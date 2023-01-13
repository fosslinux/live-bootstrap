#!/usr/bin/env python3
"""
A helper application used to get a list of source files required
for the bootstrapping process.
"""

# SPDX-License-Identifier: GPL-3.0-or-later
# SPDX-FileCopyrightText: 2023 Dor Askayo <dor.askayo@gmail.com>

import argparse

from sysa import SysA
from sysc import SysC

def main():
    parser = argparse.ArgumentParser()

    parser.add_argument("-s", "--system",
                        help="Generate source manifest for the specified systems",
                        choices=["sysa", "sysc"],
                        nargs="+",
                        action="extend",
                        required=True)

    args = parser.parse_args()

    if "sysa" in args.system:
        print(SysA.get_source_manifest())

    if "sysc" in args.system:
        print(SysC.get_source_manifest())

if __name__ == "__main__":
    main()
