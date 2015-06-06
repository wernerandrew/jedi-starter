#!/bin/bash

# debug
set -v
set -x

INSTALL='sudo apt-get -y install'

# install dependencies
sudo apt-get -y update
$INSTALL git
$INSTALL curl
$INSTALL python-setuptools
sudo easy_install pip
sudo pip install virtualenv

VAGRANT_HOME=/home/vagrant

# install package.el
cd $VAGRANT_HOME

# link to our .emacs file
sudo -u vagrant mkdir -p $VAGRANT_HOME/.emacs.d
sudo -u vagrant ln -sf /vagrant/jedi-starter.el $VAGRANT_HOME/.emacs.d/init.el

# get emacs

# build dependencies
$INSTALL build-essential
$INSTALL texinfo libtinfo-dev

# compile
EMACSBASE=emacs-24.5
EMACSPKG="$EMACSBASE.tar.gz"
if [ ! -f /vagrant/archive/$EMACSPKG ]; then
    mkdir -p /vagrant/archive
    curl -XGET -O "http://ftp.gnu.org/gnu/emacs/$EMACSPKG"
    mv -f $EMACSPKG /vagrant/archive/
fi
tar xzvf /vagrant/archive/$EMACSPKG
cd $EMACSBASE
./configure --without-all --with-zlib
make
sudo make install

# and cleanup
cd $VAGRANT_HOME
rm -rf $EMACSBASE
