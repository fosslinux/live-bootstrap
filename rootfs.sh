#!/bin/bash
set -ex

QEMU_CMD="${1:-qemu-system-x86_64}"	# or 'chroot'
QEMU_RAM="${2:-8G}"

pushd sysa

# SYSTEM A

# Setup tmp
echo "trying to mount ramdisk/tmpfs: size 8G"
mkdir -p tmp/
sudo mount -t tmpfs -o size=8G tmpfs tmp

# base: mescc-tools-seed
# copy in all the mescc-tools-seed stuff
cp -r mescc-tools-seed/x86/* tmp
cp -r mescc-tools-seed/{M2-Planet,mes-m2} tmp/
cp -r mescc-tools-patched tmp/mescc-tools
# and the kaem seed
cp ../bootstrap-seeds/POSIX/x86/kaem-optional-seed tmp/init
cp ../bootstrap-seeds/POSIX/x86/kaem-optional-seed tmp/
cp -r ../bootstrap-seeds tmp/
# replace the init kaem with our own custom one
mv tmp/kaem.run tmp/mescc-tools-seed.kaem.run
cp base.kaem.run tmp/kaem.run
# create directories needed
mkdir tmp/bin

# after mescc-tools-seed we get into our own directory because
# the mescc-tools-seed one is hella messy
mkdir tmp/after/bin -p
mkdir tmp/after/{lib,include}
mkdir tmp/after/lib/{tcc,linux}
ln -s . tmp/after/lib/x86-mes
ln -s . tmp/after/lib/linux/x86-mes
mkdir tmp/after/include/{mes,gnu,linux,sys,mach}
mkdir tmp/after/include/linux/{x86,x86_64}
cp after.kaem tmp/
cp after.kaem.run tmp/after/kaem.run

# mescc-tools-extra
cp -r mescc-tools-extra tmp/after/

# blynn-compiler
pushd tmp/after
git clone ../../blynn-compiler-oriansj blynn-compiler
cp ../../blynn-compiler.kaem blynn-compiler/go.kaem
mkdir blynn-compiler/{bin,generated}
popd

# mes
cp -r mes tmp/after/
cp -r mes tmp/after/tcc-mes
ln -s lib/x86-mes tmp/after/mes/x86-mes
cp -r nyacc tmp/after/
cp mes.kaem tmp/after/
cp mes-files/mescc.scm tmp/after/bin/
cp mes-files/config.h tmp/after/mes/include/mes/
cp mes-files/config.h tmp/after/tcc-mes/include/mes/
mkdir tmp/after/mes/{bin,m2}

# tcc
cp tcc.kaem tmp/after/
cp -r tcc-0.9.26 tmp/after/
cp -r tcc-0.9.27 tmp/after/
touch tmp/after/tcc-0.9.27/config.h
touch tmp/after/tcc-0.9.26/config.h
pushd tmp/after/tcc-0.9.26
ln -s ../mes/module .
ln -s ../mes/mes .
ln -s /after/lib x86-mes
ln -s /after/lib/linux .
popd

mkdir -p ../sources

# sed 4.0.7
cp sed-4.0.7.kaem tmp/after
cp -r sed-4.0.7 tmp/after

# tar 1.12
url=https://ftp.gnu.org/gnu/tar/tar-1.12.tar.gz
pushd ../sources
wget --continue "$url"
popd
cp "$(basename $url .tar.gz).kaem" tmp/after
tar -C tmp/after -xf "../sources/$(basename $url)"

get_file() {
    url=$1
    pushd ../sources
    wget --continue "$url"
    popd
    ext="${url##*.}"
    if [ "$ext" = "tar" ]; then
	bname=$(basename "$url" .tar)
    else
	bname=$(basename "$url" ".tar.${ext}")
    fi
    cp "${bname}.kaem" tmp/after
    cp "../sources/$(basename "$url")" tmp/after
}

# gzip 1.2.4
get_file https://ftp.gnu.org/gnu/gzip/gzip-1.2.4.tar

# diffutils 2.7
get_file https://ftp.gnu.org/gnu/diffutils/diffutils-2.7.tar.gz

# make 3.80
get_file https://ftp.gnu.org/gnu/make/make-3.80.tar.gz

# General cleanup
find tmp -name .git -exec rm -rf \;

# initramfs
cd tmp
find . | cpio -H newc -o | gzip > initramfs.igz

# Run
if [ "${QEMU_CMD}" = 'chroot' ]; then
	sudo PATH="/after/bin:${PATH}" chroot . /init
else
	${QEMU_CMD} -enable-kvm \
	    -m "${QEMU_RAM:-8G}" \
	    -nographic \
	    -no-reboot \
	    -kernel ../../kernel -initrd initramfs.igz -append console=ttyS0
fi

cd ../..

# Cleanup
echo "trying to unmount ramdisk/tmpfs, abort with CTRL+C"
sudo umount sysa/tmp
