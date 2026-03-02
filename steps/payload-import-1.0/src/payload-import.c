/* SPDX-FileCopyrightText: 2026 live-bootstrap contributors */
/* SPDX-License-Identifier: MIT */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#define MAGIC "LBPAYLD1"
#define MAGIC_LEN 8
#define MAX_NAME_LEN 1024
#define COPY_BUFSZ 65536
#define SYS_MOUNT 21

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

static int copy_exact(FILE *in, FILE *out, unsigned int len,
	const char *device, const char *name, const char *out_path)
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
		unsigned int copied = len - remaining;
		size_t nread;
		size_t written;
		if (chunk > COPY_BUFSZ) {
			chunk = COPY_BUFSZ;
		}
		nread = fread(buf, 1, chunk, in);
		if (nread != chunk) {
			if (feof(in)) {
				fprintf(stderr,
					"payload-import: truncated payload while reading %s from %s "
					"(offset=%u wanted=%u got=%u)\n",
					name, device, copied, chunk, (unsigned int)nread);
			} else {
				fprintf(stderr,
					"payload-import: read error while reading %s from %s "
					"(offset=%u): %s\n",
					name, device, copied, strerror(errno));
			}
			free(buf);
			return 1;
		}
		written = fwrite(buf, 1, chunk, out);
		if (written != chunk) {
			fprintf(stderr,
				"payload-import: write error while writing %s to %s "
				"(offset=%u wanted=%u wrote=%u): %s\n",
				name, out_path, copied, chunk, (unsigned int)written, strerror(errno));
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

#ifndef __i386__
#error "This is only for x86 i386 fiwix/linux"
#endif
static int sys_mount(const char *source, const char *target,
	const char *fstype, unsigned int flags, const void *data)
{
	int ret;
	// Only for x86 fiwix/linux
	__asm__ __volatile__(
		"int $0x80"
		: "=a"(ret)
		: "0"(SYS_MOUNT),
		  "b"(source),
		  "c"(target),
		  "d"(fstype),
		  "S"(flags),
		  "D"(data)
		: "memory"
	);

	return ret;
}

static int ensure_proc_partitions(void)
{
	struct stat st;
	int ret;

	if (stat("/proc/partitions", &st) == 0) {
		return 0;
	}

	if (mkdir("/proc", 0755) != 0 && errno != EEXIST) {
		return 1;
	}

	ret = sys_mount("proc", "/proc", "proc", 0U, (const void *)0);
	if (ret < 0) {
		return 1;
	}

	if (stat("/proc/partitions", &st) != 0) {
		return 1;
	}

	return 0;
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

		if (copy_exact(in, out, data_len, device, name, out_path) != 0) {
			free(name);
			fclose(out);
			unlink(out_path);
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

static void usage(const char *name)
{
	fprintf(stderr,
		"Usage:\n"
		"  %s --mount-proc\n"
		"  %s --probe <device>\n"
		"  %s --device <device> <dest-dir>\n",
		name, name, name);
}

int main(int argc, char **argv)
{
	const char *device = NULL;
	const char *dest_dir = NULL;
	int i;

	if (argc == 2 && strcmp(argv[1], "--mount-proc") == 0) {
		return ensure_proc_partitions();
	}

	if (argc == 3 && strcmp(argv[1], "--probe") == 0) {
		return has_payload_magic(argv[2]);
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

	if (device == NULL || dest_dir == NULL) {
		usage(argv[0]);
		return 1;
	}

	return extract_payload(device, dest_dir);
}
