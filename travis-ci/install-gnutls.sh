#!/bin/bash -x

# Setup a newer gnutls-cli on Travis CI
# We need this as long as the Travis workers are Ubuntu 14.04
# and the TLS cert chain on elpa.gnu.org is out-of-order

# adjust these versions as needed
export NETTLE_VERSION=3.4
export GNUTLS_VERSION=3.5.16

export WORKDIR=${HOME}/local/
export LD_LIBRARY_PATH=${WORKDIR}/lib/
export PKG_CONFIG_PATH=${WORKDIR}/lib/pkgconfig/

# make sure workdir exists
if [ ! -d ${WORKDIR} ]
then
    mkdir $WORKDIR
fi

# exit if the cache is valid and up-to-date
if [ -f ${WORKDIR}/bin/gnutls-cli ] && \
   [ -f ${WORKDIR}/nettle-${NETTLE_VERSION}.tar.gz ] && \
   [ -f ${WORKDIR}/gnutls-${GNUTLS_VERSION}.tar.xz ]
then
    exit 0
fi

# delete cache and rebuild
rm -rf $WORKDIR/bin $WORKDIR/lib $WORKDIR/share $WORKDIR/include
rm -rf $WORKDIR/nettle* $WORKDIR/gnutls*

cd $WORKDIR
curl -O https://ftp.gnu.org/gnu/nettle/nettle-${NETTLE_VERSION}.tar.gz \
    && tar xfz nettle-${NETTLE_VERSION}.tar.gz \
    && cd nettle-${NETTLE_VERSION} \
    && ./configure --prefix=${WORKDIR} \
    && make -j4 install \
    && make distclean

cd $WORKDIR
curl -O https://www.gnupg.org/ftp/gcrypt/gnutls/v3.5/gnutls-${GNUTLS_VERSION}.tar.xz \
    && xz -d -k gnutls-${GNUTLS_VERSION}.tar.xz \
    && tar xf gnutls-${GNUTLS_VERSION}.tar \
    && cd gnutls-${GNUTLS_VERSION} \
    && ./configure --prefix=${WORKDIR} \
                   --with-included-libtasn1 \
                   --with-included-unistring \
                   --without-p11-kit \
    && make -j4 install \
    && make distclean
