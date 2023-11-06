#!/bin/sh

sed -i "/^LIBDIR/d" /steps/env
LIBDIR=${PREFIX}/lib/i386-unknown-linux-musl
echo "LIBDIR=${LIBDIR}" >> /steps/env
