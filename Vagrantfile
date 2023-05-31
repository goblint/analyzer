# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  config.vm.box = "ubuntu/bionic64"

  config.vm.provision :shell, :path => "scripts/travis-ci.sh"

  config.vm.provider :virtualbox do |v|
    v.name = "goblint testing"
  end
end
