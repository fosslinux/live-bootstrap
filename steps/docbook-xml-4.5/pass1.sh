dirname="."

src_compile() {
    :
}

src_install() {
    install -v -d -m755 "${DESTDIR}/usr/share/xml/docbook/xml-dtd-4.5"
    install -v -d -m755 "${DESTDIR}/etc/xml"
    cp -v -af --no-preserve=ownership docbook.cat *.dtd ent/ *.mod "${DESTDIR}/usr/share/xml/docbook/xml-dtd-4.5"

    xmlcatalog --noout --create "${DESTDIR}/etc/xml/docbook"

    xmlcatalog --noout --add "public" \
        "-//OASIS//DTD DocBook XML V4.5//EN" \
        "file:///usr/share/xml/docbook/xml-dtd-4.5/docbookx.dtd" \
        "${DESTDIR}/etc/xml/docbook"
    xmlcatalog --noout --add "public" \
        "-//OASIS//DTD DocBook XML CALS Table Model V4.5//EN" \
        "file:///usr/share/xml/docbook/xml-dtd-4.5/calstblx.dtd" \
        "${DESTDIR}/etc/xml/docbook"
    xmlcatalog --noout --add "public" \
        "-//OASIS//DTD XML Exchange Table Model 19990315//EN" \
        "file:///usr/share/xml/docbook/xml-dtd-4.5/soextblx.dtd" \
        "${DESTDIR}/etc/xml/docbook"
    xmlcatalog --noout --add "public" \
        "-//OASIS//ELEMENTS DocBook XML Information Pool V4.5//EN" \
        "file:///usr/share/xml/docbook/xml-dtd-4.5/dbpoolx.mod" \
        "${DESTDIR}/etc/xml/docbook"
    xmlcatalog --noout --add "public" \
        "-//OASIS//ELEMENTS DocBook XML Document Hierarchy V4.5//EN" \
        "file:///usr/share/xml/docbook/xml-dtd-4.5/dbhierx.mod" \
        "${DESTDIR}/etc/xml/docbook"
    xmlcatalog --noout --add "public" \
        "-//OASIS//ELEMENTS DocBook XML HTML Tables V4.5//EN" \
        "file:///usr/share/xml/docbook/xml-dtd-4.5/htmltblx.mod" \
        "${DESTDIR}/etc/xml/docbook"
    xmlcatalog --noout --add "public" \
        "-//OASIS//ENTITIES DocBook XML Notations V4.5//EN" \
        "file:///usr/share/xml/docbook/xml-dtd-4.5/dbnotnx.mod" \
        "${DESTDIR}/etc/xml/docbook"
    xmlcatalog --noout --add "public" \
        "-//OASIS//ENTITIES DocBook XML Character Entities V4.5//EN" \
        "file:///usr/share/xml/docbook/xml-dtd-4.5/dbcentx.mod" \
        "${DESTDIR}/etc/xml/docbook"
    xmlcatalog --noout --add "public" \
        "-//OASIS//ENTITIES DocBook XML Additional General Entities V4.5//EN" \
        "file:///usr/share/xml/docbook/xml-dtd-4.5/dbgenent.mod" \
        "${DESTDIR}/etc/xml/docbook"

    xmlcatalog --noout --add "rewriteSystem" \
        "http://www.oasis-open.org/docbook/xml/4.5" \
        "file:///usr/share/xml/docbook/xml-dtd-4.5" \
        "${DESTDIR}/etc/xml/docbook"
    xmlcatalog --noout --add "rewriteURI" \
        "http://www.oasis-open.org/docbook/xml/4.5" \
        "file:///usr/share/xml/docbook/xml-dtd-4.5" \
        "${DESTDIR}/etc/xml/docbook"

    xmlcatalog --noout --create "${DESTDIR}/etc/xml/catalog"

    xmlcatalog --noout --add "delegatePublic" \
        "-//OASIS//ENTITIES DocBook XML" \
        "file:///etc/xml/docbook" \
        "${DESTDIR}/etc/xml/catalog"
    xmlcatalog --noout --add "delegatePublic" \
        "-//OASIS//DTD DocBook XML" \
        "file:///etc/xml/docbook" \
        "${DESTDIR}/etc/xml/catalog"
    xmlcatalog --noout --add "delegateSystem" \
        "http://www.oasis-open.org/docbook/" \
        "file:///etc/xml/docbook" \
        "${DESTDIR}/etc/xml/catalog"
    xmlcatalog --noout --add "delegateURI" \
        "http://www.oasis-open.org/docbook/" \
        "file:///etc/xml/docbook" \
        "${DESTDIR}/etc/xml/catalog"
}
