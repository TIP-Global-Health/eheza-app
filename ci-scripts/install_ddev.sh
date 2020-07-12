#!/usr/bin/env bash
set -e

# -------------------------------------------------- #
# Installing ddev dependencies.
# -------------------------------------------------- #
echo "Install Docker Compose."
sudo rm /usr/local/bin/docker-compose
curl -s -L "https://github.com/docker/compose/releases/download/1.24.1/docker-compose-$(uname -s)-$(uname -m)" > docker-compose
chmod +x docker-compose
sudo mv docker-compose /usr/local/bin

echo "Upgrade Docker."
sudo apt-get -y remove docker docker-engine docker.io containerd runc || true
sudo apt-key fingerprint 0EBFCD88
sudo add-apt-repository -y "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable"
sudo apt -q update -y || true
sudo apt -q install --only-upgrade docker-ce -y

# -------------------------------------------------- #
# Installing mkcert.
# -------------------------------------------------- #
echo "Install mkcert."
wget -nv https://github.com/FiloSottile/mkcert/releases/download/v1.4.0/mkcert-v1.4.0-linux-amd64
sudo mv mkcert-v1.4.0-linux-amd64 /usr/bin/mkcert
chmod +x /usr/bin/mkcert
mkcert -install

# -------------------------------------------------- #
# Installing ddev.
# -------------------------------------------------- #
echo "Install ddev."
curl -s -L https://raw.githubusercontent.com/drud/ddev/master/scripts/install_ddev.sh | bash

echo "Configure ddev."
mkdir ~/.ddev
cp "$TRAVIS_BUILD_DIR"/ci-scripts/global_config.yaml ~/.ddev/
