# Eheza App

<a href="https://gitpod.io/#https://github.com/TIP-Global-Health/eheza-app"><img src="https://gitpod.io/button/open-in-gitpod.svg"/></a>

## Introduction

TIP’s E-Heza Data Solutions is a digital app designed by and for the frontline health worker that can be used on a smartphone or tablet.
E-Heza captures essential data at the point of care to improve the health of mother and child by providing frontline health workers with immediate insights to help personalize information and advice, and help the mother track the well-being of her child.

## Technical stack
 - [Elm](https://elm-lang.org/) for the mobile frontend
 - [Drupal](https://www.drupal.org/) for the backend

## Try out on GitPod

The project is integrated with [GitPod](https://www.gitpod.io/docs/).
Click on the badge above to try it out the project in action and start editing
the source code! By default, Drupal and Elm client is accessible publicly, and you
can access other DDEV services like [Mailhog](https://github.com/mailhog/MailHog) using the non-HTTPS port, for instance
`8026-` should work for checking the outgoing mails.
Primary ports:
- `8888` for Drupal
- `3000` for Elm frontend

### GitPod first steps
1. Wait until the Drupal login page shows up
2. Login with `admin` / `admin` into the Drupal backend.
3. Choose "Remote Explorer" on the left and open port 3000 too, either in new browser window or in preview.
4. Use `12345678` as the pairing code (tied to Device nodes at the Drupal side).
5. Use `1234` as the PIN code (tied to the Nurse nodes at the Drupal side).
6. Initiate a sync process. The device status page allows you to initiate a manual sync with the backend.
   You can also choose which health centers to sync, for instance "Nyange Health Center" for the tests.
7. Choose the synced health center.
8. Explore the system.

## Develop locally with DDEV

### Requirements

- https://ddev.readthedocs.io/en/latest/#installation . Minimum version: [v1.21.1](https://github.com/drud/ddev/releases/tag/v1.21.1)

### Backend

#### Installation

        cp .ddev/config.local.yaml.example .ddev/config.local.yaml
        ddev restart

Migrate content with either `ddev migrate default` or `ddev migrate sample`
depending on whether you want minimal development content or a full set of
sample content (takes much longer).

The installation script will perform following steps:

1. Delete the /www folder.
2. Recreate the /www folder.
3. Download and extract all contrib modules, themes & libraries to the proper
   subfolders of the profile.
4. Download and extract Drupal 7 core in the /www folder
5. Create an empty sites/default/files directory
6. Makes a symlink within the /www/profiles directory to the /hedley
   directory.
7. Run the Drupal installer (Drush) using the Hedley profile.

***Warning!***

* The process above will not preserve the data located in the
  sites/default/files directory.
* The database is dropped during the installation.


#### Deploy

The default method assumes Pantheon as the hosting service provider.

##### Prerequisites

Prepare `Config.Deploy.elm` based on `Config.elm` that holds the
infrastructure-related data for Elm. This file is gitignored, and it is
used during Elm compilation, before the final artifact is pushed to Pantheon.
The source of truth for this deployment config is in the Pantheon artifact repository,
can be found under the repository root, so most of the time, you can copy it from there.

#### Steps

To propagate a new release, you can do the following:
```
ddev start
ddev gulp publish
ddev robo deploy:pantheon
```

To generate the release notes, use `ddev robo generate:release-notes prev-tag`.

#### Infrastructure-related steps

When the site is initially installed in an environment, there are recurring
jobs that need to be configured.

 - Advancedqueue processing
 - Weekly report processing

There are two main alternatives to achieve this. Either the hosting platform
provides customizable cron-jobs or we can invoke it from an external place,
like a Jenkins server.
Check the scripts in `infrastructure_setup` directory, create either a Jenkins
job from those or you can invoke them via `cron` or
[`supervisord`](http://supervisord.org/), edit the Bash variables at the
top of the scripts and study the file head comment that contains more
information of the dependencies of the scripts.

Example crontabs:
```
*/5 * * * *   /path/to/app/infrastructure_setup/advancedqueue.sh
1 1 * * *     /path/to/app/infrastructure_setup/reporting.sh
```

We recommend an external source, like Jenkins to trigger these, it's
a comfortable, high-level tool with easily configurable logging / history.
Inside Jenkins, these scripts can be "Freestyle project"s with
"Build periodically" trigger and a "Shell" build part.

If that's a no-go, for the advancedqueue, `supervisord` is a better choice,
as that queue needs to be processed all the time. For the reporting, a simple
cron job might be sufficient.

### Frontend

#### Prerequisites

Make sure the following is installed:

1. Elm Format (`npm install -g elm-format@0.8.1`), not strictly required for the development, but the standard must be followed, as Travis checks that. Therefore it's highly suggested to run Elm Format upon save at your IDE (https://github.com/avh4/elm-format#editor-integration).

#### Installation

* Install backend first.
* `cp src/elm/LocalConfig.Example.elm src/elm/LocalConfig.elm`

You may need to update `src/elm/LocalConfig.elm` if your local URLs are different from the default setting.

#### Usage

1. Serve locally, and watch file changes: `ddev gulp`
2. Prepare file for publishing (e.g. minify, and rev file names): `ddev gulp publish`
3. Deploy to GitHub's pages (`gh-pages` branch of your repository): `ddev gulp deploy`

#### Getting started

Frontend: http://localhost:3000 (that comes from inside DDEV after `ddev gulp`)

The Drupal migration creates Devices, Nurses out of the box, so you can
1. Use `12345678` as the pairing code (tied to Device nodes at the Drupal side).
1. Use `1234` as the PIN code (tied to the Nurse nodes at the Drupal side).

If you have a dump from another source, to be able to work locally, first of all, you need to create a Device and a Nurse.
1. `ddev drush uli` to login as `admin`
1. Fulfill https://eheza-app.ddev.site:4443/node/add/device , note the Pairing code.
1. Fulfill https://eheza-app.ddev.site:4443/node/add/nurse , note the PIN code, assign it to group(s) and health center(s).
1. Visit http://localhost:3000 (that comes from inside DDEV), supply the Pairing code and the PIN.

#### Z-Scores

Our `gulpfile.js` has a task `ddev gulp zscore` which converts the raw Z-Score tables we
downloaded from the WHO web site into three formats:

- A JSON representation the client can download via an HTTP request (and
  cache).
- A JSON representation that the backend can load (to calculate Z-Scores on the
  backend.
- An Elm module which contains the JSON representation as a string, so we can
  unit-test the Elm code.

This should all happen automatically when you run `ddev gulp`.

#### Develop how-to

After you edited an Elm file, and the compilation process is executed, the changes are not visible in the browser.
To activate the new version you've just created, click on the "Version" indication in the top-right corner of the app.
That will take you to a page which allows you to check for updates and activate updates.
