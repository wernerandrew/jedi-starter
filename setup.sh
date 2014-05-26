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
sudo -u vagrant ln -s /vagrant/jedi-starter.el $VAGRANT_HOME/.emacs

# get emacs

# build dependencies
$INSTALL build-essential
$INSTALL libgtk-3-dev
$INSTALL libgif-dev libxpm-dev
$INSTALL texinfo
$INSTALL libtiff4-dev

# download emacs
TEMPDIR=install-temp
mkdir $TEMPDIR

# compile
EMACSBASE=emacs-24.3
EMACSPKG="$EMACSBASE.tar.gz"
curl -XGET -O "http://ftp.gnu.org/gnu/emacs/$EMACSPKG"
tar xzvf $EMACSPKG
cd $EMACSBASE
./configure
make
sudo make install

# and cleanup
cd $VAGRANT_HOME
rm -rf $TEMPDIR
