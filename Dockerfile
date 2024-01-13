FROM local/stage0 as stage0

FROM debian as fetch
RUN apt update && apt install -y curl gcc
ADD . live-bootstrap
WORKDIR live-bootstrap
RUN ./download-distfiles.sh
RUN mv target/ /rootfs/

FROM scratch as build
COPY --from=fetch /rootfs .
ENV PATH=/bin
RUN ["/bootstrap-seeds/POSIX/x86/kaem-optional-seed"]
