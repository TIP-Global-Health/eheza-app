# Scalability test for an Eheza-App instance

## Requirements

 - Bash
 - [Linode.com](https://www.linode.com/) account
 - [Linode CLI](https://www.linode.com/docs/products/tools/cli/get-started/) with access token
 - [Pantheon](https://pantheon.io/) project with Eheza-App up and running
 - [Terminus CLI](https://pantheon.io/docs/terminus)

## Steps

Let's say you would like to have 50 concurrent sessions:

 - Create fake nurses and devices using: `terminus remote:drush [site].[environment] create-dummy-nurses-devices 50`
 - `./background-activities.sh [site].[environment]`
 - `vi index.js` - edit the client app URL ``baseUrl, possibly the `inputDelay` if needed (ie. later steps fail)
 - `./stress-test.sh 50`

## Analysis of the results

On Pantheon, you can rely on https://docs.newrelic.com/docs/apm/reports/performance-reports#scalability.
