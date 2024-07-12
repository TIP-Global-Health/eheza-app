# Eheza App

<a href="https://gitpod.io/#https://github.com/TIP-Global-Health/eheza-app"><img src="https://gitpod.io/button/open-in-gitpod.svg"/></a>

## Introduction

TIP’s E-Heza Data Solutions is a digital app designed by and for the frontline health worker that can be used on a smartphone or tablet.
E-Heza captures essential data at the point of care to improve the health of mother and child by providing frontline health workers with immediate insights to help personalize information and advice, and help the mother track the well-being of her child.

E-Heza has been added to the Digital Public Goods Alliance [DPG Registry](http://digitalpublicgoods.net/registry/). The goal of the DPGA and its registry is to promote digital public goods in order to create a more equitable world. Being recognised as a DPG increases the visibility, support for, and prominence of open projects that have the potential to tackle global challenges. To become a digital public good, all projects are required to meet the [DPG Standard](http://digitalpublicgoods.net/standard/) to ensure that projects truly encapsulate open source principles.

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
1. Login with `admin` / `admin` into the Drupal backend.
1. Choose "Remote Explorer" on the left and open port 3000 too, either in new browser window or in preview.
1. Use `12345678` as the pairing code (tied to Device nodes at the Drupal side).
1. Use `1234` as the PIN code (tied to the Nurse nodes at the Drupal side).
1. Initiate a sync process. The device status page allows you to initiate a manual sync with the backend.
   You can also choose which health centers to sync, for instance "Nyange Health Center" for the tests.
1. Choose the synced health center.
1. Explore the system.

## Try out locally with DDEV

1. https://ddev.readthedocs.io/en/latest/#installation . Minimum version: [v1.21.1](https://github.com/drud/ddev/releases/tag/v1.21.1)
1. On Mac, for the sake of Elm compilation, please make sure that the VM/container has at least 16GB of available RAM, otherwise `elm make` might get killed by the OOM killer.
1. `cp .ddev/config.local.yaml.example .ddev/config.local.yaml`. Note that by default, installation is performed for Rwanda site. If you wish to install for Burundi site, set `EHEZA_SITE=burundi` at `.ddev/local.config.yaml`.
1. ddev restart
1. `cp client/src/elm/LocalConfig.Example.elm client/src/elm/LocalConfig.elm`
1. * In new file, change `module LocalConfig.Example exposing (localConfigs)` to `module LocalConfig exposing (localConfigs)` 
1. `ddev gulp`
1. Open the [app](http://localhost:3000) in the browser, typically it listens on port 3000.
1. Use `12345678` as the pairing code (tied to Device nodes at the Drupal side).
1. Use `1234` as the PIN code (tied to the Nurse nodes at the Drupal side).
1. Initiate a sync process. The device status page allows you to initiate a manual sync with the backend.
   You can also choose which health centers to sync, for instance "Nyange Health Center" for the tests.
1. Choose the synced health center.
1. Explore the system.

### Frontend

#### Prerequisites

Make sure the following is installed:

1. Elm Format (`npm install -g elm-format@0.8.1`), not strictly required for the development, but the standard must be followed, as Travis checks that. Therefore it's highly suggested to run Elm Format upon save at your IDE (https://github.com/avh4/elm-format#editor-integration).

#### Installation

* Install backend first.
* `cp src/elm/LocalConfig.Example.elm src/elm/LocalConfig.elm`
* In new file, change `module LocalConfig.Example exposing (localConfigs)` to `module LocalConfig exposing (localConfigs)`

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

#### Deployment

The default method assumes Pantheon as the hosting service provider.

##### Prerequisites

Prepare `Config.Deploy.elm` based on `Config.elm` that holds the
infrastructure-related data for Elm. This file is gitignored, and it is
used during Elm compilation, before the final artifact is pushed to Pantheon.
Put it in a private repository, and link that repository using
`EHEZA_INFRA_REPO_REMOTE`, defined in `.ddev/config.local.yaml.example`.
Also, set `EHEZA_SITE`, to indicate the site installation is done for.
The repository does not have a strict structure, but the file should be
present at the `elm/[EHEZA_SITE]/Config.Deploy.elm` path.

#### Steps

To propagate a new release, you can do the following:
```
ddev start
ddev auth ssh
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
