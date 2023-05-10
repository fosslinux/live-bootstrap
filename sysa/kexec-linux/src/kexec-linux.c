/* SPDX-FileCopyrightText: 2023 Richard Masters <grick23@gmail.com> */
/* SPDX-License-Identifier: MIT */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include <time.h>

int append_file(FILE *dst_file, char *src_file_name);


int main(int argc, char **argv) {
	char *ramdrive_file_name, *kernel_file_name, *initramfs_file_name;
	FILE *ramdrive_file;
	struct stat stats;

	if (argc < 3) {
		puts("Usage: fiwix-kexec-linux <ram-drive-name> <kernel-file-name> <initramfs-file-name>");
		exit(1);
	}

	ramdrive_file_name = argv[1];
	kernel_file_name = argv[2];
	initramfs_file_name = argv[3];


	ramdrive_file = fopen(ramdrive_file_name, "wb");

	/* Write length of kernel */
	if (stat(kernel_file_name, &stats) == 0) {
		fwrite(&stats.st_size, sizeof(stats.st_size), 1, ramdrive_file);
	} else {
		fprintf(stderr, "Cannot stat kernel file '%s'\n", kernel_file_name);
		exit(1);
	}

	/* Write length of initramfs */
	if (stat(initramfs_file_name, &stats) == 0) {
		fwrite(&stats.st_size, sizeof(stats.st_size), 1, ramdrive_file);
	} else {
		fprintf(stderr, "Cannot stat initramfs file '%s'\n", initramfs_file_name);
		exit(1);
	}

	if (append_file(ramdrive_file, kernel_file_name)) {
		fprintf(stderr, "Cannot append kernel '%s'\n", kernel_file_name);
		exit(1);
	}
	if (append_file(ramdrive_file, initramfs_file_name)) {
		fprintf(stderr, "Cannot append initramfs '%s'\n", initramfs_file_name);
		exit(1);
	}
	fclose(ramdrive_file);

	/* Perform syscall sync */
	__asm__ __volatile__(
			"movl $0x00000024, %%eax\n\t"
			"int $0x80\n\t"
			: /* no output */
			: /* no input */
	       );

	/* Perform syscall reboot to initiate kexec */
	__asm__ __volatile__(
			"movl $0x58, %%eax\n\t"
			"movl $0xfee1dead, %%ebx\n\t"
			"movl $0x28121969, %%ecx\n\t"
			"movl $0xcdef0123, %%edx\n\t"
			"movl $0x00, %%esi\n\t"
			"int $0x80\n\t"
			: /* no output */
			: /* no input */
	       );
}

int append_file(FILE *dst_file, char *src_file_name) {
	FILE *src_file;
	char buff[BUFSIZ];
	size_t n;
 
	if (src_file = fopen(src_file_name, "rb")) {
		while ((n = fread(buff, 1, BUFSIZ, src_file)) != 0) {
			fwrite(buff, 1, n, dst_file );
		}
		fclose(src_file);
		return 0;
	} else {
		printf("Cannot open file '%s'\n", src_file_name);
		return 1;
	}
}
