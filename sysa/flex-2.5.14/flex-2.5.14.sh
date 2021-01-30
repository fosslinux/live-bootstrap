src_prepare() {
    default_src_prepare

    touch config.h
    rm parse.c parse.h scan.c
}
