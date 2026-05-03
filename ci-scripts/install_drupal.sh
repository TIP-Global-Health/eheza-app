#!/usr/bin/env bash
set -e

# -------------------------------------------------- #
# Installing Profile.
# -------------------------------------------------- #
echo "Install Drupal."

echo composer_version: "2" > .ddev/config.local.yaml
cat .ddev/config.local.yaml.example >> .ddev/config.local.yaml
ddev restart || ddev logs
