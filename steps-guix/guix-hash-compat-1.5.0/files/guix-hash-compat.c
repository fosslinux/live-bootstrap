#define _POSIX_C_SOURCE 200809L

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#define SHA256_BLOCK_SIZE 32

struct sha256_ctx {
    uint8_t data[64];
    uint32_t datalen;
    uint64_t bitlen;
    uint32_t state[8];
};

static const uint32_t k256[64] = {
    0x428a2f98U, 0x71374491U, 0xb5c0fbcfU, 0xe9b5dba5U,
    0x3956c25bU, 0x59f111f1U, 0x923f82a4U, 0xab1c5ed5U,
    0xd807aa98U, 0x12835b01U, 0x243185beU, 0x550c7dc3U,
    0x72be5d74U, 0x80deb1feU, 0x9bdc06a7U, 0xc19bf174U,
    0xe49b69c1U, 0xefbe4786U, 0x0fc19dc6U, 0x240ca1ccU,
    0x2de92c6fU, 0x4a7484aaU, 0x5cb0a9dcU, 0x76f988daU,
    0x983e5152U, 0xa831c66dU, 0xb00327c8U, 0xbf597fc7U,
    0xc6e00bf3U, 0xd5a79147U, 0x06ca6351U, 0x14292967U,
    0x27b70a85U, 0x2e1b2138U, 0x4d2c6dfcU, 0x53380d13U,
    0x650a7354U, 0x766a0abbU, 0x81c2c92eU, 0x92722c85U,
    0xa2bfe8a1U, 0xa81a664bU, 0xc24b8b70U, 0xc76c51a3U,
    0xd192e819U, 0xd6990624U, 0xf40e3585U, 0x106aa070U,
    0x19a4c116U, 0x1e376c08U, 0x2748774cU, 0x34b0bcb5U,
    0x391c0cb3U, 0x4ed8aa4aU, 0x5b9cca4fU, 0x682e6ff3U,
    0x748f82eeU, 0x78a5636fU, 0x84c87814U, 0x8cc70208U,
    0x90befffaU, 0xa4506cebU, 0xbef9a3f7U, 0xc67178f2U
};

#define ROTRIGHT(a, b) (((a) >> (b)) | ((a) << (32 - (b))))
#define CH(x, y, z) (((x) & (y)) ^ (~(x) & (z)))
#define MAJ(x, y, z) (((x) & (y)) ^ ((x) & (z)) ^ ((y) & (z)))
#define EP0(x) (ROTRIGHT((x), 2) ^ ROTRIGHT((x), 13) ^ ROTRIGHT((x), 22))
#define EP1(x) (ROTRIGHT((x), 6) ^ ROTRIGHT((x), 11) ^ ROTRIGHT((x), 25))
#define SIG0(x) (ROTRIGHT((x), 7) ^ ROTRIGHT((x), 18) ^ ((x) >> 3))
#define SIG1(x) (ROTRIGHT((x), 17) ^ ROTRIGHT((x), 19) ^ ((x) >> 10))

static void sha256_transform(struct sha256_ctx *ctx, const uint8_t data[]) {
    uint32_t a, b, c, d, e, f, g, h;
    uint32_t t1, t2, m[64];
    uint32_t i;

    for (i = 0; i < 16; ++i) {
        m[i] = ((uint32_t)data[i * 4] << 24) |
               ((uint32_t)data[i * 4 + 1] << 16) |
               ((uint32_t)data[i * 4 + 2] << 8) |
               ((uint32_t)data[i * 4 + 3]);
    }
    for (; i < 64; ++i) {
        m[i] = SIG1(m[i - 2]) + m[i - 7] + SIG0(m[i - 15]) + m[i - 16];
    }

    a = ctx->state[0];
    b = ctx->state[1];
    c = ctx->state[2];
    d = ctx->state[3];
    e = ctx->state[4];
    f = ctx->state[5];
    g = ctx->state[6];
    h = ctx->state[7];

    for (i = 0; i < 64; ++i) {
        t1 = h + EP1(e) + CH(e, f, g) + k256[i] + m[i];
        t2 = EP0(a) + MAJ(a, b, c);
        h = g;
        g = f;
        f = e;
        e = d + t1;
        d = c;
        c = b;
        b = a;
        a = t1 + t2;
    }

    ctx->state[0] += a;
    ctx->state[1] += b;
    ctx->state[2] += c;
    ctx->state[3] += d;
    ctx->state[4] += e;
    ctx->state[5] += f;
    ctx->state[6] += g;
    ctx->state[7] += h;
}

static void sha256_init(struct sha256_ctx *ctx) {
    ctx->datalen = 0;
    ctx->bitlen = 0;
    ctx->state[0] = 0x6a09e667U;
    ctx->state[1] = 0xbb67ae85U;
    ctx->state[2] = 0x3c6ef372U;
    ctx->state[3] = 0xa54ff53aU;
    ctx->state[4] = 0x510e527fU;
    ctx->state[5] = 0x9b05688cU;
    ctx->state[6] = 0x1f83d9abU;
    ctx->state[7] = 0x5be0cd19U;
}

static void sha256_update(struct sha256_ctx *ctx, const uint8_t *data, size_t len) {
    size_t i;
    for (i = 0; i < len; ++i) {
        ctx->data[ctx->datalen] = data[i];
        ctx->datalen++;
        if (ctx->datalen == 64) {
            sha256_transform(ctx, ctx->data);
            ctx->bitlen += 512;
            ctx->datalen = 0;
        }
    }
}

static void sha256_final(struct sha256_ctx *ctx, uint8_t hash[SHA256_BLOCK_SIZE]) {
    uint32_t i;

    i = ctx->datalen;

    if (ctx->datalen < 56) {
        ctx->data[i++] = 0x80;
        while (i < 56) {
            ctx->data[i++] = 0;
        }
    } else {
        ctx->data[i++] = 0x80;
        while (i < 64) {
            ctx->data[i++] = 0;
        }
        sha256_transform(ctx, ctx->data);
        memset(ctx->data, 0, 56);
    }

    ctx->bitlen += (uint64_t)ctx->datalen * 8;
    ctx->data[63] = (uint8_t)(ctx->bitlen);
    ctx->data[62] = (uint8_t)(ctx->bitlen >> 8);
    ctx->data[61] = (uint8_t)(ctx->bitlen >> 16);
    ctx->data[60] = (uint8_t)(ctx->bitlen >> 24);
    ctx->data[59] = (uint8_t)(ctx->bitlen >> 32);
    ctx->data[58] = (uint8_t)(ctx->bitlen >> 40);
    ctx->data[57] = (uint8_t)(ctx->bitlen >> 48);
    ctx->data[56] = (uint8_t)(ctx->bitlen >> 56);
    sha256_transform(ctx, ctx->data);

    for (i = 0; i < 4; ++i) {
        hash[i]      = (uint8_t)((ctx->state[0] >> (24 - i * 8)) & 0xff);
        hash[i + 4]  = (uint8_t)((ctx->state[1] >> (24 - i * 8)) & 0xff);
        hash[i + 8]  = (uint8_t)((ctx->state[2] >> (24 - i * 8)) & 0xff);
        hash[i + 12] = (uint8_t)((ctx->state[3] >> (24 - i * 8)) & 0xff);
        hash[i + 16] = (uint8_t)((ctx->state[4] >> (24 - i * 8)) & 0xff);
        hash[i + 20] = (uint8_t)((ctx->state[5] >> (24 - i * 8)) & 0xff);
        hash[i + 24] = (uint8_t)((ctx->state[6] >> (24 - i * 8)) & 0xff);
        hash[i + 28] = (uint8_t)((ctx->state[7] >> (24 - i * 8)) & 0xff);
    }
}

static void die_perror(const char *msg) {
    fprintf(stderr, "%s: %s\n", msg, strerror(errno));
    exit(1);
}

static void die_msg(const char *msg) {
    fprintf(stderr, "%s\n", msg);
    exit(1);
}

static void sha_update_u32le_in_8(struct sha256_ctx *ctx, uint32_t n) {
    uint8_t b[8] = {0, 0, 0, 0, 0, 0, 0, 0};
    b[0] = (uint8_t)(n & 0xffU);
    b[1] = (uint8_t)((n >> 8) & 0xffU);
    b[2] = (uint8_t)((n >> 16) & 0xffU);
    b[3] = (uint8_t)((n >> 24) & 0xffU);
    sha256_update(ctx, b, sizeof(b));
}

static void sha_update_u64le(struct sha256_ctx *ctx, uint64_t n) {
    uint8_t b[8];
    b[0] = (uint8_t)(n & 0xffU);
    b[1] = (uint8_t)((n >> 8) & 0xffU);
    b[2] = (uint8_t)((n >> 16) & 0xffU);
    b[3] = (uint8_t)((n >> 24) & 0xffU);
    b[4] = (uint8_t)((n >> 32) & 0xffU);
    b[5] = (uint8_t)((n >> 40) & 0xffU);
    b[6] = (uint8_t)((n >> 48) & 0xffU);
    b[7] = (uint8_t)((n >> 56) & 0xffU);
    sha256_update(ctx, b, sizeof(b));
}

static void nar_write_padding(struct sha256_ctx *ctx, size_t n) {
    static const uint8_t zeros[8] = {0, 0, 0, 0, 0, 0, 0, 0};
    size_t m = n % 8;
    if (m != 0) {
        sha256_update(ctx, zeros, 8 - m);
    }
}

static void nar_write_string_n(struct sha256_ctx *ctx, const char *s, size_t n) {
    if (n > UINT32_MAX) {
        die_msg("string too long for NAR serialization");
    }
    sha_update_u32le_in_8(ctx, (uint32_t)n);
    if (n > 0) {
        sha256_update(ctx, (const uint8_t *)s, n);
    }
    nar_write_padding(ctx, n);
}

static void nar_write_string(struct sha256_ctx *ctx, const char *s) {
    nar_write_string_n(ctx, s, strlen(s));
}

static void sha_update_from_fd(struct sha256_ctx *ctx, int fd, uint64_t *out_size) {
    uint8_t buf[65536];
    ssize_t n;
    uint64_t total = 0;

    for (;;) {
        n = read(fd, buf, sizeof(buf));
        if (n == 0) {
            break;
        }
        if (n < 0) {
            die_perror("read failed");
        }
        sha256_update(ctx, buf, (size_t)n);
        total += (uint64_t)n;
    }

    if (out_size != NULL) {
        *out_size = total;
    }
}

static char *join_path(const char *dir, const char *name) {
    size_t a = strlen(dir);
    size_t b = strlen(name);
    int need_slash = (a > 0 && dir[a - 1] != '/');
    char *out = malloc(a + (size_t)need_slash + b + 1);
    if (out == NULL) {
        die_msg("out of memory");
    }
    memcpy(out, dir, a);
    if (need_slash) {
        out[a] = '/';
    }
    memcpy(out + a + (size_t)need_slash, name, b);
    out[a + (size_t)need_slash + b] = '\0';
    return out;
}

static int cmp_cstring_ptr(const void *a, const void *b) {
    const char *const *sa = (const char *const *)a;
    const char *const *sb = (const char *const *)b;
    return strcmp(*sa, *sb);
}

static void nar_dump_path(struct sha256_ctx *ctx, const char *path);

static void nar_dump_directory(struct sha256_ctx *ctx, const char *path) {
    DIR *dir;
    struct dirent *de;
    char **entries = NULL;
    size_t count = 0;
    size_t cap = 0;
    size_t i;

    dir = opendir(path);
    if (dir == NULL) {
        die_perror("opendir failed");
    }

    for (;;) {
        errno = 0;
        de = readdir(dir);
        if (de == NULL) {
            if (errno != 0) {
                closedir(dir);
                die_perror("readdir failed");
            }
            break;
        }
        if (strcmp(de->d_name, ".") == 0 || strcmp(de->d_name, "..") == 0) {
            continue;
        }

        if (count == cap) {
            size_t new_cap = (cap == 0) ? 16 : cap * 2;
            char **new_entries = realloc(entries, new_cap * sizeof(char *));
            if (new_entries == NULL) {
                closedir(dir);
                die_msg("out of memory");
            }
            entries = new_entries;
            cap = new_cap;
        }

        entries[count] = strdup(de->d_name);
        if (entries[count] == NULL) {
            closedir(dir);
            die_msg("out of memory");
        }
        count++;
    }

    if (closedir(dir) != 0) {
        die_perror("closedir failed");
    }

    qsort(entries, count, sizeof(char *), cmp_cstring_ptr);

    nar_write_string(ctx, "(");
    nar_write_string(ctx, "type");
    nar_write_string(ctx, "directory");

    for (i = 0; i < count; ++i) {
        char *child = join_path(path, entries[i]);

        nar_write_string(ctx, "entry");
        nar_write_string(ctx, "(");
        nar_write_string(ctx, "name");
        nar_write_string(ctx, entries[i]);
        nar_write_string(ctx, "node");
        nar_dump_path(ctx, child);
        nar_write_string(ctx, ")");

        free(child);
        free(entries[i]);
    }

    free(entries);

    nar_write_string(ctx, ")");
}

static void nar_dump_regular(struct sha256_ctx *ctx, const char *path, mode_t mode) {
    int fd;
    uint64_t size;

    fd = open(path, O_RDONLY);
    if (fd < 0) {
        die_perror("open failed");
    }

    nar_write_string(ctx, "(");
    nar_write_string(ctx, "type");
    nar_write_string(ctx, "regular");
    if ((mode & 0100) != 0) {
        nar_write_string(ctx, "executable");
        nar_write_string(ctx, "");
    }
    nar_write_string(ctx, "contents");

    size = 0;
    {
        struct stat st;
        if (fstat(fd, &st) != 0) {
            close(fd);
            die_perror("fstat failed");
        }
        size = (uint64_t)st.st_size;
    }

    sha_update_u64le(ctx, size);
    sha_update_from_fd(ctx, fd, NULL);
    nar_write_padding(ctx, (size_t)(size % 8));

    if (close(fd) != 0) {
        die_perror("close failed");
    }

    nar_write_string(ctx, ")");
}

static void nar_dump_symlink(struct sha256_ctx *ctx, const char *path) {
    ssize_t n;
    size_t buf_size = 256;
    char *buf = NULL;

    for (;;) {
        char *new_buf = realloc(buf, buf_size);
        if (new_buf == NULL) {
            free(buf);
            die_msg("out of memory");
        }
        buf = new_buf;

        n = readlink(path, buf, buf_size);
        if (n < 0) {
            free(buf);
            die_perror("readlink failed");
        }

        if ((size_t)n < buf_size) {
            break;
        }

        buf_size *= 2;
    }

    nar_write_string(ctx, "(");
    nar_write_string(ctx, "type");
    nar_write_string(ctx, "symlink");
    nar_write_string(ctx, "target");
    nar_write_string_n(ctx, buf, (size_t)n);
    nar_write_string(ctx, ")");

    free(buf);
}

static void nar_dump_path(struct sha256_ctx *ctx, const char *path) {
    struct stat st;

    if (lstat(path, &st) != 0) {
        die_perror("lstat failed");
    }

    if (S_ISDIR(st.st_mode)) {
        nar_dump_directory(ctx, path);
        return;
    }

    if (S_ISREG(st.st_mode)) {
        nar_dump_regular(ctx, path, st.st_mode);
        return;
    }

    if (S_ISLNK(st.st_mode)) {
        nar_dump_symlink(ctx, path);
        return;
    }

    fprintf(stderr, "unsupported file type: %s\n", path);
    exit(1);
}

static void hash_flat_file(const char *path, uint8_t out[SHA256_BLOCK_SIZE]) {
    struct sha256_ctx ctx;
    int fd;

    sha256_init(&ctx);

    if (strcmp(path, "-") == 0) {
        sha_update_from_fd(&ctx, STDIN_FILENO, NULL);
    } else {
        fd = open(path, O_RDONLY);
        if (fd < 0) {
            die_perror("open failed");
        }
        sha_update_from_fd(&ctx, fd, NULL);
        if (close(fd) != 0) {
            die_perror("close failed");
        }
    }

    sha256_final(&ctx, out);
}

static void hash_nar_path(const char *path, uint8_t out[SHA256_BLOCK_SIZE]) {
    struct sha256_ctx ctx;

    sha256_init(&ctx);
    nar_write_string(&ctx, "nix-archive-1");
    nar_dump_path(&ctx, path);
    sha256_final(&ctx, out);
}

static uint8_t bytevector_quintet_ref_right(const uint8_t *bv, size_t bv_len, size_t index) {
    size_t offset = (index * 5) / 8;
    uint8_t a = (offset < bv_len) ? bv[offset] : 0;
    uint8_t b = ((offset + 1) < bv_len) ? bv[offset + 1] : 0;

    switch (index % 8) {
        case 0:
            return a & 0x1fU;
        case 1:
            return (uint8_t)(((a >> 5) & 0x07U) | ((b & 0x03U) << 3));
        case 2:
            return (uint8_t)((a >> 2) & 0x1fU);
        case 3:
            return (uint8_t)(((a >> 7) & 0x01U) | ((b & 0x0fU) << 1));
        case 4:
            return (uint8_t)(((a >> 4) & 0x0fU) | ((b & 0x01U) << 4));
        case 5:
            return (uint8_t)((a >> 1) & 0x1fU);
        case 6:
            return (uint8_t)(((a >> 6) & 0x03U) | ((b & 0x07U) << 2));
        case 7:
            return (uint8_t)((a >> 3) & 0x1fU);
        default:
            return 0;
    }
}

static void sha256_to_nix_base32(const uint8_t digest[SHA256_BLOCK_SIZE], char out[53]) {
    static const char alphabet[] = "0123456789abcdfghijklmnpqrsvwxyz";
    const size_t quintets = (SHA256_BLOCK_SIZE * 8 + 4) / 5;
    size_t i;

    for (i = 0; i < quintets; ++i) {
        size_t idx = quintets - 1 - i;
        uint8_t q = bytevector_quintet_ref_right(digest, SHA256_BLOCK_SIZE, idx);
        out[i] = alphabet[q & 0x1fU];
    }
    out[quintets] = '\0';
}

static void usage(const char *prog) {
    fprintf(stderr,
            "Usage: %s [OPTION] FILE\n"
            "  -r, --recursive   hash FILE using NAR serialization\n",
            prog);
}

int main(int argc, char **argv) {
    int recursive = 0;
    int i;
    int first_arg = 1;

    for (i = 1; i < argc; ++i) {
        if (strcmp(argv[i], "--") == 0) {
            first_arg = i + 1;
            break;
        }
        if (argv[i][0] != '-') {
            first_arg = i;
            break;
        }
        if (strcmp(argv[i], "-r") == 0 || strcmp(argv[i], "--recursive") == 0) {
            recursive = 1;
            continue;
        }
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
            usage(argv[0]);
            return 0;
        }
        fprintf(stderr, "unsupported option: %s\n", argv[i]);
        usage(argv[0]);
        return 1;
    }

    if (first_arg >= argc) {
        fprintf(stderr, "no arguments specified\n");
        usage(argv[0]);
        return 1;
    }

    for (i = first_arg; i < argc; ++i) {
        uint8_t digest[SHA256_BLOCK_SIZE];
        char out[53];

        if (recursive) {
            hash_nar_path(argv[i], digest);
        } else {
            hash_flat_file(argv[i], digest);
        }

        sha256_to_nix_base32(digest, out);
        puts(out);
    }

    return 0;
}
