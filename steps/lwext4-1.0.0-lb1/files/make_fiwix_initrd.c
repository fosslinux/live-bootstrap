/*
 * SPDX-FileCopyrightText: Copyright (c) 2015 Grzegorz Kostka (kostka.grzegorz@gmail.com)
 * SPDX-License-Identifier: BSD-3-Clause
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 * - The name of the author may not be used to endorse or promote products
 *   derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Modifications:
 * SPDX-FileCopyrightText: 2023 Richard Masters <grick23@gmail.com>
 * SPDX-License-Identifier: MIT
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdbool.h>
#include <inttypes.h>
#include <time.h>
#include <unistd.h>
#include <sys/time.h>

#include <ext4.h>
#include <ext4_mkfs.h>
#include "../blockdev/linux/file_dev.h"
#include "../blockdev/windows/file_windows.h"

#define BLOCK_SIZE 1024
#define FILENAME_LENGTH 1024

const char *input_name = NULL;
/**@brief   Block device handle.*/
static struct ext4_blockdev *bd;
/**@brief   Block cache handle.*/
static struct ext4_bcache *bc;
static struct ext4_fs fs;


static struct ext4_mkfs_info info = {
	.block_size = BLOCK_SIZE,
	.journal = false,
	.inode_size = 128,
};

#define MKDEV(major, minor) (((major) << 8) | (minor))

static bool open_filedev(void)
{
	file_dev_name_set(input_name);
	bd = file_dev_get();
	if (!bd) {
		puts("open_filedev: fail");
		return false;
	}
	return true;
}

bool lwext4_mount(struct ext4_blockdev *bdev, struct ext4_bcache *bcache)
{
	int r;

	bc = bcache;
	bd = bdev;

	if (!bd) {
		puts("lwext4_mount: no block device");
		return false;
	}

	/* ext4_dmask_set(DEBUG_ALL); */

	r = ext4_device_register(bd, "ext4_fs");
	if (r != EOK) {
		printf("ext4_device_register: rc = %d\n", r);
		return false;
	}

	r = ext4_mount("ext4_fs", "/mp/", false);
	if (r != EOK) {
		printf("ext4_mount: rc = %d\n", r);
		return false;
	}

	r = ext4_recover("/mp/");
	if (r != EOK && r != ENOTSUP) {
		printf("ext4_recover: rc = %d\n", r);
		return false;
	}

	/* ext4_cache_write_back("/mp/", 1); */

	return true;
}

bool lwext4_umount(void)
{
	int r;

	/* ext4_cache_write_back("/mp/", 0); */

	r = ext4_umount("/mp/");
	if (r != EOK) {
		printf("ext4_umount: fail %d", r);
		return false;
	}
	return true;
}


bool copy_file(char *src_path, char *dest_path)
{
	ext4_file dest_file;
	int err;
	err = ext4_inode_exist(dest_path, EXT4_DE_UNKNOWN);
	if (err == EOK) {
		/* file already copied or directory already created */
		return EXIT_SUCCESS;
	}

	printf("copy_file: %s\n", src_path);

	FILE *src_file = fopen(src_path, "rb");
	if (!src_file) {
		printf("fopen '%s' error.\n", src_path);
		return EXIT_FAILURE;
	}
	fseek(src_file, 0, SEEK_END);
	int src_len = ftell(src_file);

	fseek(src_file, 0, SEEK_SET);

	char * src_mem = malloc(src_len);
	int read_len = fread(src_mem, src_len, 1, src_file);
	fclose(src_file);
	if (read_len < 1) {
		printf("src fread error file: '%s' read count: %d\n", src_path, read_len);
	}

	err = ext4_fopen(&dest_file, dest_path, "wb");
	if (err != EOK) {
		printf("ext4_open error: %d\n", err);
		return EXIT_FAILURE;
	}

	if (src_len > 0) {
		err = ext4_fwrite(&dest_file, src_mem, src_len, 0);
		if (err != EOK) {
			printf("ext4_fwrite error: %d\n", err);
			return EXIT_FAILURE;
		}
	}

	err = ext4_fclose(&dest_file);
	if (err != EOK) {
		printf("ext4_fclose error: %d\n", err);
		return EXIT_FAILURE;
	}

	free(src_mem);
}

bool copy_file_system()
{
	int filenum;
	unsigned int filename_addr;
	/* +4 to account for /mp and null termination */
	char dst_filename[FILENAME_LENGTH + 4];
	for (filenum = 14335, filename_addr = 0xfffc00; filenum >= 3; filenum--, filename_addr -= FILENAME_LENGTH) {
		/* Avoid including fiwix.ext2 in itself */
		if (((char *) filename_addr)[0] != '/' ||
			!strcmp((char *) filename_addr, "/") ||
			!strcmp((char *) filename_addr, "/boot/fiwix.ext2")) {
			continue;
		}
		strcpy(dst_filename, "/mp");
		strcat(dst_filename, (char *) filename_addr);
		copy_file((char *) filename_addr, dst_filename);
	}
}

int main(int argc, char **argv)
{
	int err;

	char zeros[BLOCK_SIZE];

	unsigned int next_file_address;
	int filenum;
	unsigned int filename_addr;
	unsigned int file_addr;
	unsigned int initrd_size;
	int i;

	next_file_address = 0x54000000;
	for (filenum = 14335, filename_addr = 0xfffc00, file_addr = 0x1037FF4;
		 filenum >= 3;
		 filenum--, filename_addr -= FILENAME_LENGTH, file_addr -= 16) {
		if (((char *) filename_addr)[0] == '/') {
			next_file_address = *((unsigned int *) file_addr);
			next_file_address += *((unsigned int *) (file_addr + 4));
			break;
		}
	}

	/* Create zeroed out disk image file */
	input_name = "/boot/fiwix.ext2";
	initrd_size = 262144;

	for (i = 1; i < argc; i++) {
		if (!strcmp(argv[i], "-s")) {
			i++;
			initrd_size = atoi(argv[i]);
		} else {
			input_name = argv[i];
		}
	}

	printf("Starting %s at addr 0x%08x\n", input_name, next_file_address);

	memset(zeros, 0, BLOCK_SIZE);
	FILE *ext2file = fopen(input_name, "w");
	int b;
	for (b=0; b < initrd_size; b++)
		fwrite(zeros, BLOCK_SIZE, 1, ext2file);
	fclose(ext2file);

	if (!open_filedev()) {
		printf("open_filedev error\n");
		return EXIT_FAILURE;
	}

	/* ext4_dmask_set(DEBUG_ALL); */

	err = ext4_mkfs(&fs, bd, &info, F_SET_EXT2_V0);
	if (err != EOK) {
		printf("ext4_mkfs error: %d\n", err);
		return EXIT_FAILURE;
	}

	memset(&info, 0, sizeof(struct ext4_mkfs_info));
	err = ext4_mkfs_read_info(bd, &info);
	if (err != EOK) {
		printf("ext4_mkfs_read_info error: %d\n", err);
		return EXIT_FAILURE;
	}

	printf("Created filesystem with parameters:\n");
	printf("Size: %"PRIu64"\n", info.len);
	printf("Block size: %"PRIu32"\n", info.block_size);
	printf("Blocks per group: %"PRIu32"\n", info.blocks_per_group);
	printf("Inodes per group: %"PRIu32"\n",	info.inodes_per_group);
	printf("Inode size: %"PRIu32"\n", info.inode_size);
	printf("Inodes: %"PRIu32"\n", info.inodes);
	printf("Journal blocks: %"PRIu32"\n", info.journal_blocks);
	printf("Features ro_compat: 0x%x\n", info.feat_ro_compat);
	printf("Features compat: 0x%x\n", info.feat_compat);
	printf("Features incompat: 0x%x\n", info.feat_incompat);
	printf("BG desc reserve: %"PRIu32"\n", info.bg_desc_reserve_blocks);
	printf("Descriptor size: %"PRIu32"\n",info.dsc_size);
	printf("Label: %s\n", info.label);

	if (!lwext4_mount(bd, bc))
		return EXIT_FAILURE;

	puts("ext4_dir_mk /mp/dev");
	err = ext4_dir_mk("/mp/dev");
	if (err != EOK) {
		printf("ext4_dir_mk error: %d\n", err);
	}
	puts("ext4_dir_mk /mp/tmp");
	err = ext4_dir_mk("/mp/tmp");
	if (err != EOK) {
		printf("ext4_dir_mk error: %d\n", err);
	}

	puts("ext4_mknod /mp/dev/console");
	err = ext4_mknod("/mp/dev/console", EXT4_DE_CHRDEV, MKDEV(5, 1));
	if (err != EOK) {
		printf("ext4_mknod error: %d\n", err);
		return EXIT_FAILURE;
	}
	puts("ext4_mknod /mp/dev/ram0");
	err = ext4_mknod("/mp/dev/ram0", EXT4_DE_BLKDEV, MKDEV(1, 0));
	if (err != EOK) {
		printf("ext4_mknod error: %d\n", err);
		return EXIT_FAILURE;
	}
	puts("ext4_mknod /mp/dev/ram1");
	err = ext4_mknod("/mp/dev/ram1", EXT4_DE_BLKDEV, MKDEV(1, 1));
	if (err != EOK) {
		printf("ext4_mknod error: %d\n", err);
		return EXIT_FAILURE;
	}

	copy_file_system();

	if (!lwext4_umount())
		return EXIT_FAILURE;

	puts("Fiwix ext2 initrd created successfully.");
	return EXIT_SUCCESS;
}
