/* SPDX-FileCopyrightText: 2026 live-bootstrap contributors */
/* SPDX-License-Identifier: MIT */

#include <errno.h>
#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#define MAGIC "LBPAYLD1"
#define MAGIC_LEN 8
#define MAX_NAME_LEN 1024
#define COPY_BUFSZ 65536

/* mount(2) is available in the bootstrap libc, but some header stacks
 * don't expose a prototype consistently. */
extern int mount(const char *source, const char *target,
	const char *filesystemtype, unsigned int mountflags, const void *data);

static unsigned int read_u32le(const unsigned char *buf)
{
	return (unsigned int)buf[0]
		| ((unsigned int)buf[1] << 8)
		| ((unsigned int)buf[2] << 16)
		| ((unsigned int)buf[3] << 24);
}

static int read_exact(FILE *in, void *buf, unsigned int len)
{
	unsigned int got = 0;
	unsigned char *out = (unsigned char *)buf;

	while (got < len) {
		size_t n = fread(out + got, 1, len - got, in);
		if (n == 0) {
			return -1;
		}
		got += (unsigned int)n;
	}
	return 0;
}

static int copy_exact(FILE *in, FILE *out, unsigned int len)
{
	unsigned char *buf;
	unsigned int remaining = len;

	buf = (unsigned char *)malloc(COPY_BUFSZ);
	if (buf == NULL) {
		fputs("payload-import: out of memory\n", stderr);
		return 1;
	}

	while (remaining > 0) {
		unsigned int chunk = remaining;
		size_t written;
		if (chunk > COPY_BUFSZ) {
			chunk = COPY_BUFSZ;
		}
		if (read_exact(in, buf, chunk) != 0) {
			free(buf);
			return 1;
		}
		written = fwrite(buf, 1, chunk, out);
		if (written != chunk) {
			free(buf);
			return 1;
		}
		remaining -= chunk;
	}

	free(buf);
	return 0;
}

static int is_valid_name(const char *name)
{
	const unsigned char *s = (const unsigned char *)name;

	if (*s == 0) {
		return 0;
	}

	while (*s != 0) {
		if (*s == '/' || *s == '\\') {
			return 0;
		}
		s += 1;
	}
	return 1;
}

static int has_payload_magic(const char *path)
{
	FILE *in;
	char magic[MAGIC_LEN];

	in = fopen(path, "rb");
	if (in == NULL) {
		return 1;
	}
	if (read_exact(in, magic, MAGIC_LEN) != 0) {
		fclose(in);
		return 1;
	}
	fclose(in);
	if (memcmp(magic, MAGIC, MAGIC_LEN) != 0) {
		return 1;
	}
	return 0;
}

static int starts_with(const char *s, const char *prefix)
{
	while (*prefix != 0) {
		if (*s != *prefix) {
			return 0;
		}
		s++;
		prefix++;
	}
	return 1;
}

static int ends_with_digit(const char *s)
{
	char c = 0;

	while (*s != 0) {
		c = *s;
		s++;
	}
	return c >= '0' && c <= '9';
}

static int is_proc_partition_candidate(const char *name)
{
	if (*name == 0 || strcmp(name, "name") == 0) {
		return 0;
	}
	if (starts_with(name, "ram")
	 || starts_with(name, "loop")
	 || starts_with(name, "fd")
	 || starts_with(name, "sr")
	 || starts_with(name, "md")
	 || starts_with(name, "dm-")
	 || starts_with(name, "nbd")) {
		return 0;
	}
	if (ends_with_digit(name)) {
		/* Skip partitions; payload is attached as a whole disk. */
		return 0;
	}
	return 1;
}

static unsigned int mkdev_u8(unsigned int major, unsigned int minor)
{
	return ((major & 0xFFU) << 8) | (minor & 0xFFU);
}

static int extract_payload(const char *device, const char *dest_dir)
{
	FILE *in;
	char magic[MAGIC_LEN];
	unsigned char u32buf[4];
	unsigned int file_count;
	unsigned int i;

	in = fopen(device, "rb");
	if (in == NULL) {
		fprintf(stderr, "payload-import: cannot open %s: %s\n", device, strerror(errno));
		return 1;
	}

	if (read_exact(in, magic, MAGIC_LEN) != 0 || memcmp(magic, MAGIC, MAGIC_LEN) != 0) {
		fclose(in);
		fprintf(stderr, "payload-import: %s is not a payload image\n", device);
		return 1;
	}

	if (read_exact(in, u32buf, 4) != 0) {
		fclose(in);
		fputs("payload-import: malformed payload header\n", stderr);
		return 1;
	}
	file_count = read_u32le(u32buf);
	if (file_count > 200000U) {
		fclose(in);
		fprintf(stderr, "payload-import: unreasonable file count: %u\n", file_count);
		return 1;
	}

	if (mkdir(dest_dir, 0755) != 0 && errno != EEXIST) {
		fclose(in);
		fprintf(stderr, "payload-import: cannot create %s: %s\n", dest_dir, strerror(errno));
		return 1;
	}

	printf("payload-import: reading %u files from %s\n", file_count, device);
	for (i = 0; i < file_count; ++i) {
		unsigned int name_len;
		unsigned int data_len;
		char *name;
		char out_path[4096];
		FILE *out;

		if (read_exact(in, u32buf, 4) != 0) {
			fclose(in);
			fputs("payload-import: truncated entry header\n", stderr);
			return 1;
		}
		name_len = read_u32le(u32buf);
		if (read_exact(in, u32buf, 4) != 0) {
			fclose(in);
			fputs("payload-import: truncated entry size\n", stderr);
			return 1;
		}
		data_len = read_u32le(u32buf);

		if (name_len == 0 || name_len > MAX_NAME_LEN) {
			fclose(in);
			fprintf(stderr, "payload-import: invalid name length %u\n", name_len);
			return 1;
		}

		name = (char *)malloc(name_len + 1);
		if (name == NULL) {
			fclose(in);
			fputs("payload-import: out of memory\n", stderr);
			return 1;
		}

		if (read_exact(in, name, name_len) != 0) {
			free(name);
			fclose(in);
			fputs("payload-import: truncated file name\n", stderr);
			return 1;
		}
		name[name_len] = 0;

		if (!is_valid_name(name)) {
			fclose(in);
			fprintf(stderr, "payload-import: invalid payload file name: %s\n", name);
			free(name);
			return 1;
		}

		if (snprintf(out_path, sizeof(out_path), "%s/%s", dest_dir, name) >= (int)sizeof(out_path)) {
			free(name);
			fclose(in);
			fputs("payload-import: output path too long\n", stderr);
			return 1;
		}

		out = fopen(out_path, "wb");
		if (out == NULL) {
			fprintf(stderr, "payload-import: cannot write %s: %s\n", out_path, strerror(errno));
			free(name);
			fclose(in);
			return 1;
		}

		if (copy_exact(in, out, data_len) != 0) {
			fprintf(stderr, "payload-import: failed while copying %s\n", name);
			free(name);
			fclose(out);
			fclose(in);
			return 1;
		}

		fclose(out);
		printf("payload-import: %s\n", name);
		free(name);
	}

	fclose(in);
	return 0;
}

static int ensure_proc_partitions(void)
{
	struct stat st;

	if (stat("/proc/partitions", &st) == 0) {
		return 0;
	}

	if (stat("/proc", &st) != 0) {
		if (mkdir("/proc", 0755) != 0 && errno != EEXIST) {
			return 1;
		}
	}

	if (mount("proc", "/proc", "proc", 0, (const void *)0) != 0) {
		return 1;
	}

	if (stat("/proc/partitions", &st) != 0) {
		return 1;
	}
	return 0;
}

static int import_from_proc_partitions(const char *dest_dir)
{
	FILE *fp;
	char line[256];

	if (ensure_proc_partitions() != 0) {
		return 1;
	}

	fp = fopen("/proc/partitions", "r");
	if (fp == NULL) {
		return 1;
	}

	while (fgets(line, sizeof(line), fp) != NULL) {
		unsigned int major, minor, blocks;
		char name[64];
		char dev_path[96];

		if (sscanf(line, " %u %u %u %63s", &major, &minor, &blocks, name) != 4) {
			continue;
		}
		if (!is_proc_partition_candidate(name)) {
			continue;
		}

		if (snprintf(dev_path, sizeof(dev_path), "/dev/%s", name) >= (int)sizeof(dev_path)) {
			continue;
		}

		if (access(dev_path, F_OK) != 0) {
			if (mknod(dev_path, S_IFBLK | 0600, mkdev_u8(major, minor)) != 0) {
				continue;
			}
		}
		if (has_payload_magic(dev_path) == 0) {
			fclose(fp);
			return extract_payload(dev_path, dest_dir);
		}
	}

	fclose(fp);
	return 2;
}

static int import_from_dev_nodes(const char *dest_dir)
{
	DIR *dir;
	struct dirent *entry;

	dir = opendir("/dev");
	if (dir == NULL) {
		return 2;
	}

	entry = readdir(dir);
	while (entry != NULL) {
		char path[512];
		struct stat st;

		if (entry->d_name[0] == '.') {
			entry = readdir(dir);
			continue;
		}
		if (snprintf(path, sizeof(path), "/dev/%s", entry->d_name) >= (int)sizeof(path)) {
			entry = readdir(dir);
			continue;
		}
		if (lstat(path, &st) != 0 || !S_ISBLK(st.st_mode)) {
			entry = readdir(dir);
			continue;
		}
		if (has_payload_magic(path) == 0) {
			closedir(dir);
			return extract_payload(path, dest_dir);
		}
		entry = readdir(dir);
	}

	closedir(dir);
	return 2;
}

static void usage(const char *name)
{
	fprintf(stderr,
		"Usage:\n"
		"  %s --probe <device>\n"
		"  %s --from-proc <dest-dir>\n"
		"  %s [--device <device>] <dest-dir>\n",
		name, name, name);
}

int main(int argc, char **argv)
{
	const char *device = NULL;
	const char *dest_dir = NULL;
	int i;

	if (argc == 3 && strcmp(argv[1], "--probe") == 0) {
		return has_payload_magic(argv[2]);
	}
	if (argc == 3 && strcmp(argv[1], "--from-proc") == 0) {
		return import_from_proc_partitions(argv[2]);
	}

	for (i = 1; i < argc; ++i) {
		if (strcmp(argv[i], "--device") == 0) {
			i += 1;
			if (i >= argc) {
				usage(argv[0]);
				return 1;
			}
			device = argv[i];
		} else if (dest_dir == NULL) {
			dest_dir = argv[i];
		} else {
			usage(argv[0]);
			return 1;
		}
	}

	if (dest_dir == NULL) {
		usage(argv[0]);
		return 1;
	}

	if (device != NULL) {
		return extract_payload(device, dest_dir);
	}
	i = import_from_proc_partitions(dest_dir);
	if (i == 0) {
		return 0;
	}
	if (i == 1) {
		fputs("payload-import: /proc/partitions unavailable, falling back to /dev scan\n", stderr);
	}
	return import_from_dev_nodes(dest_dir);
}
