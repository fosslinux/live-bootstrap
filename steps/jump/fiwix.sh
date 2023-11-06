#!/bin/sh

set -ex

# Build the ext2 image
make_fiwix_initrd

# Boot Fiwix
kexec-fiwix
