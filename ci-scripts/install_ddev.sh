#!/usr/bin/env bash
set -e

MAC=0
if ! command -v docker &> /dev/null
then
    echo "docker could not be found, trying to install it on Mac"
    brew install openssh
    brew install docker
    brew install colima
    colima start --cpu 2 --memory 2 --dns=1.1.1.1
    MAC=1
fi

echo "Install ddev."
curl -s -L https://raw.githubusercontent.com/drud/ddev/master/scripts/install_ddev.sh | bash -s v1.24.3

echo "Configuring ddev."
mkdir ~/.ddev
cp "ci-scripts/global_config.yaml" ~/.ddev/
docker network create ddev_default || ddev logs

if [[ "$MAC" == 1 ]];
then
  ddev config global --mutagen-enabled
fi

export MAC
