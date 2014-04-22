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
[ -d .emacs.d ] || mkdir .emacs.d
cd .emacs.d
curl -XGET -O http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el
sudo chown -R vagrant:vagrant $VAGRANT_HOME/.emacs.d

# link to our .emacs file
sudo -u vagrant ln -s $VAGRANT_HOME/.emacs /vagrant/jedi-starter.el

# get emacs
$INSTALL emacs
