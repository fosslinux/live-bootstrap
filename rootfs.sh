#!/bin/bash
set -ex

QEMU_CMD="${1:-qemu-system-x86_64}"	# or 'chroot' or 'minikernel'
QEMU_RAM="${2:-8G}"

pushd sysa

# SYSTEM A

# Setup tmp
mkdir -p tmp/
sudo mount -t tmpfs -o size=8G tmpfs tmp

LOGFILE="$PWD/tmp/bootstrap.log"

# base: mescc-tools-seed
# copy in all the mescc-tools-seed stuff
cp -r mescc-tools-seed/src/mescc-tools-seed/x86/* tmp
cp -r mescc-tools-seed/src/mescc-tools-seed/{M2-Planet,mes-m2} tmp/
cp -r mescc-tools-seed/src/mescc-tools-patched tmp/mescc-tools
# and the kaem seed
cp bootstrap-seeds/POSIX/x86/kaem-optional-seed tmp/init
cp bootstrap-seeds/POSIX/x86/kaem-optional-seed tmp/
cp -r bootstrap-seeds tmp/
# replace the init kaem with our own custom one
mv tmp/kaem.run tmp/mescc-tools-seed.kaem.run
cp base.kaem.run tmp/kaem.run
# create directories needed
mkdir -p tmp/bin

# after mescc-tools-seed we get into our own directory because
# the mescc-tools-seed one is hella messy
mkdir -p tmp/after/bin
mkdir -p tmp/after/{lib,include}
mkdir -p tmp/after/lib/{tcc,linux}
ln -s . tmp/after/lib/x86-mes
ln -s . tmp/after/lib/linux/x86-mes
mkdir -p tmp/after/include/{mes,gnu,linux,sys,mach}
mkdir -p tmp/after/include/linux/{x86,x86_64}
mkdir -p tmp/tmp
cp after.kaem tmp/
cp after.kaem.run tmp/after/kaem.run
cp helpers.sh run.sh tmp/after/

# mescc-tools-extra
cp -r mescc-tools-extra tmp/after/

# blynn-compiler
cp -r blynn-compiler tmp/after/
mkdir -p tmp/after/blynn-compiler/src/{bin,generated}

# mes
cp -r mes tmp/after/
#ln -s lib/x86-mes tmp/after/mes/src/mes/x86-mes
mkdir -p tmp/after/mes/src/mes/{bin,m2}

# tcc 0.9.26
cp -r tcc-0.9.26 tmp/after/
pushd tmp/after/tcc-0.9.26/src/tcc-0.9.26
ln -s ../mes/module .
ln -s ../mes/mes .
ln -s /after/lib x86-mes
ln -s /after/lib/linux .
popd

# tcc 0.9.27
cp -r tcc-0.9.27 tmp/after/

# sed 4.0.7
cp -r sed-4.0.7 tmp/after/

mkdir -p ../sources

# tar 1.12
url=https://ftp.gnu.org/gnu/tar/tar-1.12.tar.gz
pushd ../sources
if [ ! -f "$(basename $url)" ]; then
    wget "$url"
fi
popd
cp -r tar-1.12 tmp/after
tar -C tmp/after/tar-1.12/src -xf "../sources/$(basename $url)" --strip-components=1

get_file() {
    url=$1
    pushd ../sources
    if [ ! -f "$(basename "$url")" ]; then
        wget "$url"
    fi
    popd
    ext="${url##*.}"
    if [ "$ext" = "tar" ]; then
        bname=$(basename "$url" ".tar")
    else
        bname=$(basename "$url" ".tar.${ext}")
    fi
    cp -r "${bname}" tmp/after/
    cp "../sources/$(basename "$url")" "tmp/after/${bname}/src/"
}

# gzip 1.2.4
get_file https://ftp.gnu.org/gnu/gzip/gzip-1.2.4.tar

# diffutils 2.7
get_file https://ftp.gnu.org/gnu/diffutils/diffutils-2.7.tar.gz

# patch 2.5.9
get_file https://ftp.gnu.org/pub/gnu/patch/patch-2.5.9.tar.gz

# make 3.80
get_file https://ftp.gnu.org/gnu/make/make-3.80.tar.gz

# bzip2 1.0.8
get_file ftp://sourceware.org/pub/bzip2/bzip2-1.0.8.tar.gz

# coreutils 5.0
get_file https://ftp.gnu.org/gnu/coreutils/coreutils-5.0.tar.bz2

# grep 2.4
get_file https://ftp.gnu.org/gnu/grep/grep-2.4.tar.gz

# heirloom-devtools
get_file http://downloads.sourceforge.net/project/heirloom/heirloom-devtools/070527/heirloom-devtools-070527.tar.bz2

# bash 2.05b
get_file https://ftp.gnu.org/pub/gnu/bash/bash-2.05b.tar.gz

# m4 1.4
get_file https://ftp.gnu.org/gnu/m4/m4-1.4.tar.gz

# General cleanup
find tmp -name .git -exec rm -rf \;

# initramfs
cd tmp
find . | cpio -H newc -o | gzip > initramfs.igz

# Run
case "${QEMU_CMD}" in
	chroot)
		sudo PATH="/after/bin:${PATH}" chroot . /init | tee "$LOGFILE"
	;;
	minikernel)
		git clone --depth 1 --branch v0.4 https://github.com/bittorf/kritis-linux.git

		kritis-linux/ci_helper.sh \
			--arch x86_64 \
			--ramsize 4G \
			--kernel 5.10.8 \
			--initrd initramfs.igz \
			--log "$LOGFILE"
	;;
	*)
		${QEMU_CMD} -enable-kvm \
		    -m "${QEMU_RAM:-8G}" \
		    -nographic \
		    -no-reboot \
		    -kernel ../../kernel -initrd initramfs.igz -append console=ttyS0 | tee "$LOGFILE"
	;;
esac

cd ../..

# eventually keep logfile before unmount:
echo "see logfile: $LOGFILE"

# Cleanup
sudo umount sysa/tmp
