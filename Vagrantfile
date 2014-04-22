$script = <<SCRIPT
set -v
set -x

sudo apt-get -y update
sudo apt-get -y install git
sudo apt-get -y install curl
sudo apt-get -y install python-setuptools
sudo easy_install pip
sudo pip install virtualenv

VAGRANT_HOME=/home/vagrant
cd $VAGRANT_HOME
[ -d .emacs.d ] || mkdir .emacs.d
cd .emacs.d
curl -XGET -O http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el
sudo chown -R vagrant:vagrant $VAGRANT_HOME

sudo apt-get -y install emacs
SCRIPT

Vagrant.configure("2") do |config|
  config.vm.box = "precise64"
  config.vm.box_url = "http://files.vagrantup.com/precise64.box"
  config.vm.provision "shell", inline: $script
end
