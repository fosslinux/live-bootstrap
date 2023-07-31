#!/bin/bash

# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-License-Identifier: MIT

# Replace this hook if you wish to do more
# Add Dev Nodes 
# Disk Nodes
# 3 disks 3 partitions
major=0
alpha="a b c" 
# For each disk...
for a in ${alpha}; do
        mknod -m 600 "/dev/sd${a}" b 8 "$((major*16))"
        #For each partition do...
	minor=1 
        for p in $(seq 3); do
            mknod -m 600 "/dev/sd${a}${p}" b 8 "$((major*16+minor++))"
        done
	((major++))
done
# NVME Nodes
# 3 NVME disk with 3 partitions
major=0 
# For each disk...
for a in $(seq 3); do
	mknod -m 600 "/dev/nvme${a}" c 259 0 # NVME CHAR node
        mknod -m 600 "/dev/nvme${a}n1" b 259 "$((major))"
	((major++))
        # For each partition...
        for q in $(seq 3); do
            mknod -m 600 "/dev/nvme${a}n1p${q}" b 259 "$((major++))"
        done
done
# add cd-rom drive
mknod -m 600 /dev/sr0 b 11 0
. /usr/src/.env
exec env - PATH="${PREFIX}/bin" PS1="\w # " bash -i
