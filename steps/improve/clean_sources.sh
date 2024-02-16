# SPDX-FileCopyrightText: 2023 Eduardo Sánchez Muñoz <eduardosm-dev@e64.io>
# SPDX-FileCopyrightText: 2024 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# Delete sources of packages before linux kernel

get_source_filename() {
    local url="${1}"
    local fname="${3}"
    # Default to basename of url if not given
    echo "${fname:-$(basename "${url}")}"
}

# List all packages from linux kernel onwards
# Ideally, we would use arrays here, but they are not supported by
# the bash version we have at this point.
pkgs="$(awk '/^build:/ { print $2 }' "${SRCDIR}/manifest" | awk '/^linux-[0-9]/,EOF { print $0 }')"

# Gather source names for all packages in pkgs, which we want to keep
keep_sources=""
for pkg in ${pkgs}; do
    while read line; do
        keep_sources="${keep_sources} $(get_source_filename ${line})"
    done < "${SRCDIR}/${pkg}/sources"
done

for source in "${DISTFILES}/"*; do
    source_name="$(basename "${source}")"
    for keep_source in ${keep_sources}; do
        if [ "${keep_source}" = "${source_name}" ]; then
            # Countinue the outer loop to skip deletion
            continue 2
        fi
    done

    # Delete this source
    rm "${source}"
done

if [ -e "/external/repo-preseeded/linux-4.14.336_0.tar.bz2" ]; then
    # This is done in src_extract out of necessity usually -- I can't think of a better solution :(
    rm -f "${DISTFILES}/linux-4.14.336.tar.xz"
fi

unset get_source_filename
unset pkgs pkg line
unset keep_sources keep_source
unset source source_name
