#!/bin/sh

ppa () {
    sudo apt-add-repository -y "$1"
}

apt_update () {
    sudo apt-get update -qq
}

apt () {
    sudo apt-get install -yy "$@"
}

install_emacs() {
    # no EMACS version is specified use snapshot.
    if [ -z $EMACS_BINARY ];then
        export EMACS_BINARY=emacs-snapshot
    fi
    echo $EMACS_BINARY

    if [ $EMACS_BINARY = "emacs-snapshot" ]; then
        ppa ppa:ubuntu-elisp/ppa
        apt_update
        apt emacs-snapshot emacs-snapshot-el
    else
        apt_update
        apt git
        # evm install
        sudo mkdir -p /usr/local/evm
        sudo chown $USER: /usr/local/evm
        curl -fsSkL https://raw.github.com/rejeep/evm/master/go | bash
        export PATH=$HOME/.evm/bin:$PATH
        evm install $EMACS_BINARY
        evm use $EMACS_BINARY
    fi
}

# Silence debconf
export DEBIAN_FRONTEND='noninteractive'

install_emacs
emacs --version

# Install Cask for Emacs dependency management
CASK_VERSION=0.7.4
CASK_DIR=/opt/cask-$CASK_VERSION
CASK_ARCHIVE=https://github.com/cask/cask/archive/v$CASK_VERSION.tar.gz
if ! [ -d "$CASK_DIR" -a -x "/$CASK_DIR/bin/cask" ]; then
    sudo rm -rf "$CASK_DIR"
    wget -q -O - $CASK_ARCHIVE | sudo tar xz -C /opt
    # Bring Cask into $PATH
    sudo ln -fs "$CASK_DIR/bin/cask" /usr/local/bin
fi
