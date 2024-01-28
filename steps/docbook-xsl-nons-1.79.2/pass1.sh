src_compile() {
    :
}

src_install() {
    install -v -m755 -d "${DESTDIR}/usr/share/xml/docbook/xsl-stylesheets-nons-1.79.2"

    cp -v -R VERSION assembly common eclipse epub epub3 extensions fo        \
             highlighting html htmlhelp images javahelp lib manpages params  \
             profiling roundtrip slides template tests tools webhelp website \
             xhtml xhtml-1_1 xhtml5                                          \
        "${DESTDIR}/usr/share/xml/docbook/xsl-stylesheets-nons-1.79.2"

    ln -s VERSION "${DESTDIR}/usr/share/xml/docbook/xsl-stylesheets-nons-1.79.2/VERSION.xsl"
}
