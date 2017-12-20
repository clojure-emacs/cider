#!/bin/bash -x

# Install evm for Travis CI
# or if already installed, then check for updates

WORKDIR=${HOME}/local
EVMDIR=$WORKDIR/evm
SCRIPTDIR=`dirname $(readlink -f $0)`

. $SCRIPTDIR/retry.sh

if [ -d $EVMDIR ]
then
    cd $EVMDIR
    git pull origin master
else
    git clone https://github.com/rejeep/evm.git $EVMDIR
    evm config path /tmp
    travis_retry evm install emacs-24.3-travis --use --skip
fi
