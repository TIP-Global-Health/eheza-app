#!/bin/bash
# Cron job/Jenkins schedule: H/5 * * * *
# This example assumes that we use Pantheon hosting,
# The current user is named "ubuntu" and that user
# has a Drush alias for the site and it's authenticated
# via public key.

ENVIRONMENT="live"
SITE="eheza-app"
ALIAS="@pantheon.$SITE.$ENVIRONMENT"
DRUSH=$(/home/ubuntu/.composer/vendor/bin/drush ${ALIAS} 2>&1 >/dev/null ) || true
NOT_FOUND="Could not find the alias"
if [[ "$DRUSH" = ${NOT_FOUND}* ]]
then
    echo "Alias ${ALIAS} not found. Recreating aliases."
    terminus alias
fi

/home/ubuntu/.composer/vendor/bin/drush ${ALIAS} advancedqueue --all --timeout=300 --verbose --uri="https://$ENVIRONMENT-$SITE.pantheonsite.io"
