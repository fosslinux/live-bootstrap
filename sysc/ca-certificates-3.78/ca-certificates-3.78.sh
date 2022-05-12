# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

urls="http://ftp.mozilla.org/pub/security/nss/releases/NSS_3_78_RTM/src/nss-3.78.tar.gz"

src_compile() {
    cp nss/lib/ckfw/builtins/certdata.txt .
    mk-ca-bundle -n -s ALL -m
}

src_install() {
    install -D -m 644 ca-bundle.crt "${DESTDIR}/etc/ssl/certs/ca-certificates.crt"
    ln -s /etc/ssl/certs/ca-certificates.crt "${DESTDIR}/etc/ssl/certs.pem"
}
