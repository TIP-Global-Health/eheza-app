#!/bin/bash

# This example assumes that we use Pantheon hosting,
# the system has terminus installed and the user is authenticated
# via public key.
# Also markdown-to-email script should be available in the path.
# @see https://gist.github.com/AronNovak/20e7193296443eea8e3f15640ebcb44d
# Edit the recipient email address and the Pantheon Site ID below.

RECIPIENT="manager@example.com"
PANTHEON_ID="eheza-app.live"
ID="$(date +"%Y-%U")"
REPORT_FILE="/tmp/eheza-app-weekly-report-$ID-$PANTHEON_ID.txt"

if [[ -f "$REPORT_FILE.success" ]];
then
  echo "Report already exists for this week"
  exit 0
fi

export REPORT_RECIPIENT=$(terminus remote:drush $PANTHEON_ID vget hedley_admin_report_email -- --format=string)
export EMAIL_REGEX="^[a-z0-9!#\$%&'*+/=?^_\`{|}~-]+(\.[a-z0-9!#$%&'*+/=?^_\`{|}~-]+)*@([a-z0-9]([a-z0-9-]*[a-z0-9])?\.)+[a-z0-9]([a-z0-9-]*[a-z0-9])?\$"
if [[ $REPORT_RECIPIENT =~ $EMAIL_REGEX ]] ; then
    echo "Report to $REPORT_RECIPIENT"
else
    echo "Reporting is not enabled for $PANTHEON_ID"
    exit 0
fi

cat /dev/null > $REPORT_FILE
terminus remote:drush $PANTHEON_ID scr profiles/hedley/modules/custom/hedley_admin/scripts/generate-demographics-report.php >> $REPORT_FILE
terminus remote:drush $PANTHEON_ID scr profiles/hedley/modules/custom/hedley_admin/scripts/generate-nutrition-report.php >> $REPORT_FILE
terminus remote:drush $PANTHEON_ID scr profiles/hedley/modules/custom/hedley_admin/scripts/generate-anc-report.php >> $REPORT_FILE
terminus remote:drush $PANTHEON_ID scr profiles/hedley/modules/custom/hedley_admin/scripts/generate-closed-pregnancies-report.php >> $REPORT_FILE

echo "From: info@eheza-app.com
To: $RECIPIENT
Subject: Eheza-App Weekly Report # $ID
---
" > /tmp/head.txt
cat /tmp/head.txt $REPORT_FILE | markdown-to-email  -s
echo "1" > "$REPORT_FILE.success"
