# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2026 Samuel Tyler <samuel@samuelt.me>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    find . \( -name '*.mo' -o -name '*.gmo' -o -name '*.pdf' \) -delete
    rm man/{info,install-info,makeinfo,pod2texi,texi2dvi,texindex}.1

    rm tp/Texinfo/XS/convert/converters_options.{c,h} \
        tp/Texinfo/XS/convert/cmd_converter.c \
        tp/Texinfo/XS/main/{cmd,global,options}_* \
        tp/Texinfo/XS/main/{element_types,html_conversion_data}.{c,h} \
        tp/Texinfo/XS/main/{accent_tables_8bit_codepoints.c,command_data.c,command_ids.h} \
        tp/Texinfo/Documentlanguages.pm \
        tp/maintain/lib/libintl-perl/{gettext_xs,}/Makefile \
        tp/maintain/lib/{Text-Unidecode,libintl-perl}/META.json \
        tp/maintain/template.pod

    touch tp/Texinfo/XS/main/command_{ids.h,data.c} \
        tp/Texinfo/XS/main/global_{multi,unique}_commands_case.c \
        tp/Texinfo/XS/main/global_commands_types.h

    rm -r tp/t/results \
        tp/tests/test_scripts \
        tp/tests/*/res_parser \
        tp/tests/many_input_files/*_res \
        install-info/tests/ii-*-file*

    for f in \
        tp/Texinfo/{Commands,Options,HTMLData}.pm \
        tp/Texinfo/Convert/{Info,DocBook,Plaintext,TexinfoXML,HTML}.pm; do
        sed '/^__END__$/q' "$f" > "$f.new"
        mv "$f.new" "$f"
    done

    grep generated tp/maintain/lib/libintl-perl/lib/Locale/RecodeData/*.pm -l | xargs rm

    pushd tp
    mv ../../ISO-639-2_utf-8.txt ../../country-codes.csv maintain/
    perl maintain/regenerate_documentlanguages-loc.pl
    popd

    ../../import-gnulib.sh
    autoreconf -fi
    pushd tp/Texinfo/XS
    autoreconf -fi
    popd
}

src_configure() {
    ./configure --prefix="${PREFIX}"
}
