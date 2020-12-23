#!/bin/bash
set -ex

pushd sysa

# SYSTEM A

# Setup tmp 
mkdir -p tmp/
sudo mount -t tmpfs -o size=8G tmpfs tmp

# base: mescc-tools-seed

#debugging
cp busybox.static tmp/

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
# put all the kaems for after in
cp after.kaem tmp/
cp after.kaem.run tmp/after/kaem.run

# blynn-compiler
pushd tmp/after
git clone ../../blynn-compiler-oriansj blynn-compiler
cp ../../blynn-compiler.kaem blynn-compiler/go.kaem
mkdir blynn-compiler/{bin,generated}
popd

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

# Cleanup
sudo umount tmp

popd
