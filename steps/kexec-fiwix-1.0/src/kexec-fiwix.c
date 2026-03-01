/* SPDX-FileCopyrightText: 2023 Richard Masters <grick23@gmail.com> */
/* SPDX-License-Identifier: MIT */
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "multiboot1.h"

#define MULTIBOOT_MAGIC 0x2BADB002

multiboot_uint32_t get_memmap(char *filename, void *memmap_addr) {

	if (!filename) {
		return 0;
	}

	multiboot_uint32_t memmap_length = 0;

	FILE *memmap_file = fopen(filename, "r");
	if (!memmap_file) {
		return 0;
	}

	fseek(memmap_file, 0, SEEK_END);
	memmap_length = ftell(memmap_file);
	if (!memmap_length) {
		fclose(memmap_file);
		return 0;
	}
	printf("kexec-fiwix: Memory map file length: %d\n", memmap_length);

	puts("kexec-fiwix: Reading memory map file...");
	fseek(memmap_file, 0, SEEK_SET);
	int memmap_read_len = fread(memmap_addr, memmap_length, 1, memmap_file);
	fclose(memmap_file);

	if (memmap_read_len < 1) {
		printf("kexec-fiwix: memory map fread error: %d\n", memmap_read_len);
		return 0;
	}

	return memmap_length;
}

int main(int argc, char **argv) {

	char *fiwix_filename = "/boot/fiwix";
	char *initrd_filename = "/boot/fiwix.ext2";
	char *cmdline = NULL;
	char *memmap = NULL;
	int i;

	for (i = 1; i < argc; i++) {
		if (!strcmp(argv[i], "-c")) {
			i++;
			cmdline = argv[i];
		} else if (!strcmp(argv[i], "-i")) {
			i++;
			initrd_filename = argv[i];
		} else if (!strcmp(argv[i], "-m")) {
			i++;
			memmap = argv[i];
		} else {
			fiwix_filename = argv[i];
		}
	}

	/* Read the kernel */

	printf("kexec-fiwix: starting kernel %s...\n\n", fiwix_filename);
	FILE *fiwix_file = fopen(fiwix_filename, "r");
	fseek(fiwix_file, 0, SEEK_END);
	int fiwix_len = ftell(fiwix_file);
	printf("kexec-fiwix: Fiwix kernel file length: %d\n", fiwix_len);

	puts("kexec-fiwix: Reading kernel...");
	fseek(fiwix_file, 0, SEEK_SET);
	char * fiwix_mem = malloc(fiwix_len);
	int read_len = fread(fiwix_mem, fiwix_len, 1, fiwix_file);
	fclose(fiwix_file);

	if (read_len < 1) {
		printf("kexec-fiwix: kernel fread error: %d\n", read_len);
		return EXIT_FAILURE;
	}


	/* Display info from ELF header */

	unsigned int e_entry = *((unsigned int *) (&fiwix_mem[0x18]));
	printf("ELF virtual entry point       : 0x%x\n", e_entry);

	unsigned int e_phoff = *((unsigned int *) (&fiwix_mem[0x1C]));
	printf("ELF program header offset     : 0x%x\n", e_phoff);

	unsigned int e_phnum = *((unsigned int *) (&fiwix_mem[0x2C]));
	e_phnum &= 0xFFFF;
	printf("ELF number of program  headers: %d\n", e_phnum);

	unsigned int e_phentsize = *((unsigned int *) (&fiwix_mem[0x2A]));
	e_phentsize &= 0xFFFF;
	printf("ELF size of program  headers  : %d\n", e_phentsize);


	/* Load the kernel */
	puts("kexec-fiwix: Placing kernel in memory...");

	int header_num;
	for (header_num = 0; header_num < e_phnum; header_num++) {
		char * fiwix_prog_header = &fiwix_mem[e_phoff + header_num * e_phentsize];

		unsigned int p_offset = *((unsigned int *) (&fiwix_prog_header[0x04]));
		unsigned int p_vaddr = *((unsigned int *) (&fiwix_prog_header[0x08]));
		unsigned int p_paddr = *((unsigned int *) (&fiwix_prog_header[0x0C]));
		unsigned int p_filesz = *((unsigned int *) (&fiwix_prog_header[0x10]));
		unsigned int p_memsz = *((unsigned int *) (&fiwix_prog_header[0x14]));

		if (header_num == 0) {
        		e_entry -= (p_vaddr - p_paddr);
			printf("ELF physical entry point      : 0x%x\n", e_entry);
		}

		printf("header %d:\n", header_num);
		printf("    p_offset: 0x%08x\n", p_offset);
		printf("    p_paddr : 0x%08x\n", p_paddr);
		printf("    p_filesz: 0x%08x\n", p_filesz);
		printf("    p_memsz : 0x%08x\n", p_memsz);

		memset((void *)p_paddr, 0, p_memsz + 0x10000);
		memcpy((void *)p_paddr, &fiwix_mem[p_offset], p_filesz);
	}

	puts("Preparing multiboot info for kernel...");

	char * boot_loader_name = "kexec-fiwix";

	unsigned int next_avail_mem = 0x9800;
	multiboot_info_t * pmultiboot_info = (multiboot_info_t *) next_avail_mem;
	memset(pmultiboot_info, 0, sizeof(multiboot_info_t));

	pmultiboot_info->flags = MULTIBOOT_INFO_BOOT_LOADER_NAME
		| MULTIBOOT_INFO_MEMORY
		| MULTIBOOT_INFO_MODS
		| MULTIBOOT_INFO_MEM_MAP;

	next_avail_mem += sizeof(multiboot_info_t);

	pmultiboot_info->mem_lower = 0x0000027F;
	pmultiboot_info->mem_upper = 0x002FFB80;

	/* Set command line */
	if (cmdline != NULL) {
		pmultiboot_info->flags = pmultiboot_info->flags | MULTIBOOT_INFO_CMDLINE;
		pmultiboot_info->cmdline = next_avail_mem;
		strcpy((char *) next_avail_mem, cmdline);
		next_avail_mem += (strlen(cmdline) + 1);
	}

	int filenum;
	unsigned int filename_addr;
	for (filenum = 4, filename_addr = 0x201000; filenum <= 14335; filenum++, filename_addr += 1024) {
		if (!strcmp((char *) filename_addr, initrd_filename)) {
			printf("Found image at filenum %d\n", filenum);
			break;
		}
	}

	unsigned int initrd_src = *((unsigned int *) (0x01000000 + (16 * filenum) + 4));
	unsigned int initrd_len = *((unsigned int *) (0x01000000 + (16 * filenum) + 8));
	printf("initrd_src: 0x%08x\n", initrd_src);
	printf("initrd_len: 0x%08x\n", initrd_len);

	/* Set ramdrive info */
	pmultiboot_info->mods_count = 1;
	pmultiboot_info->mods_addr = next_avail_mem;
	multiboot_module_t *pmultiboot_module = (multiboot_module_t *) next_avail_mem;
	pmultiboot_module->mod_start = 0x1C6000;
	pmultiboot_module->mod_end = 0x1C6000 + initrd_len;
	next_avail_mem += sizeof(multiboot_module_t);
	pmultiboot_module->cmdline = next_avail_mem;
	strcpy((char *) next_avail_mem, "fiwix.ext2");
	next_avail_mem += (strlen("fiwix.ext2") + 1);

	/* Set memory map info */
	pmultiboot_info->mmap_addr = next_avail_mem;
	pmultiboot_info->mmap_length = get_memmap(memmap, (void *) pmultiboot_info->mmap_addr);

	/* if we have no memory map, fall back to a safe default */
	if (pmultiboot_info->mmap_length == 0) {
		pmultiboot_info->mmap_length = 7 * sizeof(multiboot_memory_map_t);
		multiboot_memory_map_t *pmultiboot_memory_map = (multiboot_memory_map_t *) next_avail_mem;

		pmultiboot_memory_map->size = sizeof(multiboot_memory_map_t) - sizeof(multiboot_uint32_t);
		pmultiboot_memory_map->addr = 0x00000000;
		pmultiboot_memory_map->len  = 0x00080000;
		pmultiboot_memory_map->type = MULTIBOOT_MEMORY_AVAILABLE;
		pmultiboot_memory_map++;

		pmultiboot_memory_map->size = sizeof(multiboot_memory_map_t) - sizeof(multiboot_uint32_t);
		pmultiboot_memory_map->addr = 0x00080000;
		pmultiboot_memory_map->len  = 0x00020000;
		pmultiboot_memory_map->type = MULTIBOOT_MEMORY_RESERVED;
		pmultiboot_memory_map++;

		pmultiboot_memory_map->size = sizeof(multiboot_memory_map_t) - sizeof(multiboot_uint32_t);
		pmultiboot_memory_map->addr = 0x000F0000;
		pmultiboot_memory_map->len =  0x00010000;
		pmultiboot_memory_map->type = MULTIBOOT_MEMORY_RESERVED;
		pmultiboot_memory_map++;

		pmultiboot_memory_map->size = sizeof(multiboot_memory_map_t) - sizeof(multiboot_uint32_t);
		pmultiboot_memory_map->addr = 0x00100000;
		pmultiboot_memory_map->len  = 0xBBF00000;
		pmultiboot_memory_map->type = MULTIBOOT_MEMORY_AVAILABLE;
		pmultiboot_memory_map++;

		pmultiboot_memory_map->size = sizeof(multiboot_memory_map_t) - sizeof(multiboot_uint32_t);
		pmultiboot_memory_map->addr = 0XBC000000;
		pmultiboot_memory_map->len  = 0x04000000;
		pmultiboot_memory_map->type = MULTIBOOT_MEMORY_RESERVED;
		pmultiboot_memory_map++;

		pmultiboot_memory_map->size = sizeof(multiboot_memory_map_t) - sizeof(multiboot_uint32_t);
		pmultiboot_memory_map->addr = 0XFEFFC000;
		pmultiboot_memory_map->len  = 0x00004000;
		pmultiboot_memory_map->type = MULTIBOOT_MEMORY_RESERVED;
		pmultiboot_memory_map++;

		pmultiboot_memory_map->size = sizeof(multiboot_memory_map_t) - sizeof(multiboot_uint32_t);
		pmultiboot_memory_map->addr = 0XFFFC0000;
		pmultiboot_memory_map->len  = 0x00040000;
		pmultiboot_memory_map->type = MULTIBOOT_MEMORY_RESERVED;
		pmultiboot_memory_map++;
	}

	next_avail_mem += pmultiboot_info->mmap_length;

	/* Set boot loader name */
	pmultiboot_info->boot_loader_name = next_avail_mem;
	strcpy((char *) next_avail_mem, boot_loader_name);
	/* next_avail_mem += (strlen(boot_loader_name) + 1); */

	/* Jump to kernel entry point */
	unsigned int magic = MULTIBOOT_BOOTLOADER_MAGIC;
	unsigned int dummy = 0;
	unsigned int multiboot_info_num = (unsigned int) pmultiboot_info;

	printf("Preparing trampoline...\n");

	/* The ramdrive needs to be written to a location that would overwrite this program.
	 * Therfore, the code that copies the ram drive and jumps to the kernel needs to be
	 * run from a safe location. So, we put that code into an array variable and
	 * copy the code (called a "trampoline") to a safe location and then jump to it.
	 * The 0x00000000 values below are place holders which are set below
	 */
	char trampoline[] = {
		0xBE, 0x00, 0x00, 0x00, 0x00,   /* mov esi, 0x00000000 */
		0xBF, 0x00, 0x00, 0x00, 0x00,   /* mov edi, 0x00000000 */
		0xB9, 0x00, 0x00, 0x00, 0x00,   /* mov ecx, 0x00000000 */
		0xFC,                           /* cld                 */
		0xF3, 0xA4,                     /* rep movsb           */
		0xB8, 0x00, 0x00, 0x00, 0x00,   /* mov eax, 0x00000000 */
		0xBB, 0x00, 0x00, 0x00, 0x00,   /* mov ebx, 0x00000000 */
		0xFB,                           /* sti                 */
		0xEA, 0x00, 0x00, 0x00, 0x00, 0x08, 0x00  /* jmp far 0x0008:0x00000000 */
	};

	/* Set place holder values */
	*((unsigned int *) &trampoline[1])  = initrd_src;
	*((unsigned int *) &trampoline[6])  = 0x001C6000;
	*((unsigned int *) &trampoline[11]) = initrd_len;
	*((unsigned int *) &trampoline[19])  = magic;
	*((unsigned int *) &trampoline[24])  = multiboot_info_num;
	*((unsigned int *) &trampoline[30])  = e_entry;

	memcpy((void *)0x4000, trampoline, sizeof(trampoline));

	printf("kexec-fiwix: jumping to trampoline...\n");
	__asm__ __volatile__ (
		"ljmp $0x8, $0x00004000\n\t"
	);
}
