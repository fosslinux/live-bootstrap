/* SPDX-FileCopyrightText: 2023 Richard Masters <grick23@gmail.com> */
/* SPDX-License-Identifier: MIT */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <sys/reboot.h>
#include <sys/stat.h>
#include <time.h>

int append_file(FILE *dst_file, char *src_file_name);

int main(int argc, char **argv) {
	char *ramdrive_file_name, *kernel_file_name, *initramfs_file_name;
	FILE *ramdrive_file;
	struct stat stats;
	uint32_t size;

	if (argc < 3) {
		puts("Usage: fiwix-kexec-linux <ram-drive-name> <kernel-file-name> <initramfs-file-name>");
		exit(1);
	}

	ramdrive_file_name = argv[1];
	kernel_file_name = argv[2];
	initramfs_file_name = argv[3];

	ramdrive_file = fopen(ramdrive_file_name, "wb");

	/* Move past where lengths go */
	int length_offset = 2 * sizeof(uint32_t);
	fseek(ramdrive_file, length_offset, SEEK_SET);
	uint32_t last_pos = ftell(ramdrive_file);

	/* Write the kernel and initramfs */
	if (append_file(ramdrive_file, kernel_file_name)) {
		fprintf(stderr, "Cannot append kernel '%s'\n", kernel_file_name);
		exit(1);
	}

	uint32_t kernel_size = ftell(ramdrive_file) - last_pos;
	last_pos = ftell(ramdrive_file);

	if (append_file(ramdrive_file, initramfs_file_name)) {
		fprintf(stderr, "Cannot append initramfs '%s'\n", initramfs_file_name);
		exit(1);
	}

	uint32_t initramfs_size = ftell(ramdrive_file) - last_pos;

	/* Now write the lengths */
	fseek(ramdrive_file, 0, SEEK_SET);
	fwrite(&kernel_size, sizeof(kernel_size), 1, ramdrive_file);
	fwrite(&initramfs_size, sizeof(initramfs_size), 1, ramdrive_file);

	fclose(ramdrive_file);

	/* Flush ram drive writes to device */
	sync();

	/* Perform syscall reboot to initiate kexec */
	reboot(RB_HALT_SYSTEM);
}

int append_file(FILE *dst_file, char *src_file_name) {
	FILE *src_file;
	char buff[BUFSIZ];
	size_t n;

	if (*src_file_name == '!') {
		src_file_name++;
		src_file = popen(src_file_name, "r");
	} else {
		src_file = fopen(src_file_name, "rb");
	}
 
	if (src_file) {
		while ((n = fread(buff, 1, BUFSIZ, src_file)) != 0) {
			fwrite(buff, 1, n, dst_file);
		}
		fclose(src_file);
		return 0;
	} else {
		printf("Cannot open file '%s'\n", src_file_name);
		return 1;
	}
}
