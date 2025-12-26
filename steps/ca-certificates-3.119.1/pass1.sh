# SPDX-FileCopyrightText: 2022 Samuel Tyler <samuel@samuelt.me>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_compile() {
    # Cannot be UNIX time 0 because mk-ca-bundle treats that as falsey
    touch -t 197001010001.00 certdata.txt
    mk-ca-bundle -n -s ALL -m
}

src_install() {
    install -D -m 644 ca-bundle.crt "${DESTDIR}/etc/ssl/certs/ca-certificates.crt"
    ln -s certs/ca-certificates.crt "${DESTDIR}/etc/ssl/certs.pem"
}
