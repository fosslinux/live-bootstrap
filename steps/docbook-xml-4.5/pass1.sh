dirname="."

src_compile() {
    :
}

src_install() {
    install -v -d -m755 "${DESTDIR}/usr/share/xml/docbook/xml-dtd-4.5"
    install -v -d -m755 "${DESTDIR}/etc/xml"
    cp -v -af --no-preserve=ownership docbook.cat *.dtd ent/ *.mod "${DESTDIR}/usr/share/xml/docbook/xml-dtd-4.5"
}
