# Eheza App

<a href="https://gitpod.io/#https://github.com/Gizra/eheza-app"><img src="https://gitpod.io/button/open-in-gitpod.svg"/></a>

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
5. Use `1234` as the PIN code (tied to the user accounts at the Drupal side).
6. Explore the system.

Known issues:
 - Sometimes `gulp` fails to download all the Elm packages. Then locate the running process: `ps aux | grep gulp`, kill it, and launch it again: `ddev gulp`.

## Develop locally with DDEV

### Requirements

- https://ddev.readthedocs.io/en/latest/#installation

### Backend

#### Installation

        cp default.config.sh config.sh
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

#### Deploy to Pantheon

##### Prerequisites

Prepare `Config.Deploy.elm` based on `Config.elm` that holds the
infrastructure-related data for Elm. This file is gitignored, and it is
used during Elm compilation, before the final artifact is pushed to Pantheon.

#### Steps

```
ddev start
ddev gulp publish
ddev robo deploy:pantheon
```

To generate the release notes, use `ddev robo generate:release-notes prev-tag`.

### Frontend

#### Prerequisites

Make sure the following is installed:

1. Elm Format (`npm install -g elm-format@0.8.1`), not strictly required for the development, but the standard must be followed, as Travis checks that. Therefore it's highly suggested to run Elm Format upon save at your IDE (https://github.com/avh4/elm-format#editor-integration).

#### Installation

* ddev client-install
* `cp src/elm/LocalConfig.Example.elm src/elm/LocalConfig.elm`

You may need to update `src/elm/LocalConfig.elm` if your local URLs are different from the default setting.

#### Usage

1. Serve locally, and watch file changes: `ddev gulp`
2. Prepare file for publishing (e.g. minify, and rev file names): `ddev gulp publish`
3. Deploy to GitHub's pages (`gh-pages` branch of your repository): `ddev gulp deploy`

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
