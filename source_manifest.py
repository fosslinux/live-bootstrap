#!/usr/bin/env python3
"""
A helper application used to get a list of source files required
for the bootstrapping process.
"""

# SPDX-License-Identifier: GPL-3.0-or-later
# SPDX-FileCopyrightText: 2023 Dor Askayo <dor.askayo@gmail.com>

import argparse

from lib.generator import Generator

def main():
    """Generate a source manifest for a system"""
    print(Generator.get_source_manifest())

if __name__ == "__main__":
    main()
