# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_compile() {
    cp -a nss/lib/ckfw/builtins/certdata.txt .
    mk-ca-bundle -n -s ALL -m
}

src_install() {
    install -D -m 644 ca-bundle.crt "${DESTDIR}/etc/ssl/certs/ca-certificates.crt"
    ln -s /etc/ssl/certs/ca-certificates.crt "${DESTDIR}/etc/ssl/certs.pem"
}
