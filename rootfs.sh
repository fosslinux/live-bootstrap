#!/bin/bash
set -ex

pushd sysa

# SYSTEM A

# Setup tmp 
mkdir -p tmp/
sudo mount -t tmpfs -o size=8G tmpfs tmp

# base: mescc-tools-seed

# copy in all the mescc-tools-seed stuff
cp -r mescc-tools-seed/x86/* tmp
cp -r mescc-tools-seed/{M2-Planet,mes-m2,mescc-tools} tmp/
# and the kaem seed
cp ../bootstrap-seeds/POSIX/x86/kaem-optional-seed tmp/init
cp ../bootstrap-seeds/POSIX/x86/kaem-optional-seed tmp/
cp -r ../bootstrap-seeds tmp/
# replace the init kaem with our own custom one
mv tmp/kaem.run tmp/mescc-tools-seed.kaem.run
cp base.kaem.run tmp/kaem.run
# create directories needed
mkdir tmp/bin

# after mescc-tools-seed we get into our own little directory because
# the mescc-tools-seed one is hella messy
mkdir tmp/after/bin -p
mkdir tmp/after/{lib,include}
mkdir tmp/after/include/{mes,gnu,linux,sys,mach}
# put all the kaems for after in
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
cp -r mes-0.22 tmp/after/
ln -s . tmp/after/mes-0.22/x86-mes
#cp -r nyacc-0.99.3 tmp/after/
cp -r mes-bins tmp/after/
cp mes.kaem tmp/after/

# General cleanup
find tmp -name .git -exec rm -rf \;

# initramfs
cd tmp 
find . | cpio -H newc -o | gzip > initramfs.igz

# Run
qemu-system-x86_64 -enable-kvm \
    -m 16G \
    -nographic \
    -no-reboot \
    -kernel ../../kernel -initrd initramfs.igz -append console=ttyS0,kernel.panic=2

cd ../..

# Cleanup
sudo umount sysa/tmp
