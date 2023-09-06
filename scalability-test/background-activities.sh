#!/bin/bash

SITE=$1
ENV=$2

while : ; do
  terminus remote:drush "$SITE"."$ENV" core-cron
  terminus remote:drush "$SITE"."$ENV" uli
  terminus remote:drush "$SITE"."$ENV" watchdog-show
  sleep 20
done
