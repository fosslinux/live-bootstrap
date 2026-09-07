# live-bootstrap

This repository uses [`README.rst`](./README.rst) as the canonical main documentation.

## Kernel-bootstrap raw `external.img`

`external.img` is a raw container disk used in kernel-bootstrap mode when
`--external-sources` is set and `--repo` is unset.

### Why not put everything in the initial image?

In kernel-bootstrap mode, the first boot image is consumed by very early
runtime code before the system reaches the normal bash-based build stage.
That early stage has tight assumptions about memory layout and file table usage.

When too many distfiles are packed into the initial image, those assumptions can
be exceeded, which leads to unstable handoff behavior (for example, failures
around the Fiwix transition in QEMU or on bare metal).

So the design is intentionally split:

- Initial image: only what is required to reach `improve: import_payload`
- `external.img`: the rest of distfiles

This is not a patch-style workaround. It is a two-phase transport design that
keeps early boot deterministic and moves bulk data import to a stage where the
runtime is robust enough to process it safely.

### Why import from an external image and copy into main filesystem?

Because the bootstrap still expects distfiles to end up under the normal local
path (`/external/distfiles`) for later steps. `external.img` is used as a
transport medium only.

The flow is:

1. Boot minimal initial image.
2. Reach `improve: import_payload`.
3. Detect the external container disk by magic (`LBPAYLD1`) across detected block devices.
4. Copy payload files into `/external/distfiles`.
5. Continue the build exactly as if files had been present locally all along.

### Format

- Magic: `LBPAYLD1` (8 bytes)
- Then: little-endian `u64` file count
- Repeated entries:
  - little-endian `u64` name length
  - little-endian `u64` file size
  - file name string, encoded as UTF-8 bytes (no terminator)
  - file bytes

`name length` is the number of bytes in the UTF-8 encoded file name (not the number of Unicode code points).

The importer probes detected block devices and selects the one with magic `LBPAYLD1`.

### Manual creation without Python

Prepare `external.list` as:

```text
<archive-name> <absolute-path-to-archive>
```

Then:

```sh
cat > make-payload.sh <<'SH'
#!/bin/sh
set -e
out="${1:-external.img}"
list="${2:-external.list}"

write_u64le() {
  v="$1"
  printf '%016x' "$v" | sed -E 's/(..)(..)(..)(..)(..)(..)(..)(..)/\8\7\6\5\4\3\2\1/' | xxd -r -p
}

count="$(wc -l < "${list}" | tr -d ' ')"
: > "${out}"
printf 'LBPAYLD1' >> "${out}"
write_u64le "${count}" >> "${out}"

while read -r name path; do
  [ -n "${name}" ] || continue
  size="$(wc -c < "${path}" | tr -d ' ')"
  name_len="$(printf '%s' "${name}" | wc -c | tr -d ' ')"
  write_u64le "${name_len}" >> "${out}"
  write_u64le "${size}" >> "${out}"
  printf '%s' "${name}" >> "${out}"
  cat "${path}" >> "${out}"
done < "${list}"
SH
chmod +x make-payload.sh
./make-payload.sh external.img external.list
```

Attach `external.img` as an extra raw disk in QEMU, or as the second disk on bare metal.

### When it is used

- Used in kernel-bootstrap with `--external-sources` and without `--repo`.
- Not used with `--repo` (that path still uses an ext filesystem disk).
- Without `--external-sources` and without `--repo`, there is no second disk:
  the initial image only includes distfiles needed before `improve: get_network`,
  and later distfiles are downloaded from mirrors.
- `--extra-builds=guix` increases container contents (includes post-early `steps-guix`
  sources), but does not change the mechanism.
