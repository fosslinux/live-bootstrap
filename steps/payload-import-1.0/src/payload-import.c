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

static int import_from_first_payload(const char *dest_dir)
{
	const char *prefixes[] = {"/dev/sd", "/dev/vd", "/dev/hd"};
	int p;

	for (p = 0; p < 3; ++p) {
		char letter;
		for (letter = 'b'; letter <= 'z'; ++letter) {
			char device[16];
			if (snprintf(device, sizeof(device), "%s%c", prefixes[p], letter) >= (int)sizeof(device)) {
				continue;
			}
			if (access(device, R_OK) != 0) {
				continue;
			}
			if (has_payload_magic(device) == 0) {
				return extract_payload(device, dest_dir);
			}
		}
	}

	fputs("payload-import: no payload disk found\n", stderr);
	return 2;
}

static void usage(const char *name)
{
	fprintf(stderr,
		"Usage:\n"
		"  %s --probe <device>\n"
		"  %s [--device <device>] <dest-dir>\n",
		name, name);
}

int main(int argc, char **argv)
{
	const char *device = NULL;
	const char *dest_dir = NULL;
	int i;

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

	if (dest_dir == NULL) {
		usage(argv[0]);
		return 1;
	}

	if (device != NULL) {
		return extract_payload(device, dest_dir);
	}
	return import_from_first_payload(dest_dir);
}
