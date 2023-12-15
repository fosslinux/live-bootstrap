# SPDX-FileCopyrightText: 2023 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# Save because linux deletes all distfiles to save space
cp "${DISTFILES}"/musl-1.2.4.tar.gz /tmp
cp "${DISTFILES}"/curl-7.88.1.tar.bz2 /tmp

# Clear up some RAM space
grep --no-filename '^build' "${SOURCES}"/run*.sh | grep -v musl-1.2.4 | sed "s/build //" | sed "s/ .*$//" | while read -r p ; do
    rm -rf "${SOURCES:?}/${p:?}"
done

mv /tmp/musl-1.2.4.tar.gz "${DISTFILES}"
mv /tmp/curl-7.88.1.tar.bz2 "${DISTFILES}"
