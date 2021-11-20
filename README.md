# ihangane

[![Build Status](https://travis-ci.com/Gizra/ihangane.svg?token=p2M1EeCrd3dY32WxWj3X&branch=master)](https://travis-ci.com/Gizra/ihangane)

<a href="https://gitpod.io/#https://github.com/Gizra/ihangane"><img src="https://gitpod.io/button/open-in-gitpod.svg"/></a>

## GitPod

The project is integrated with [GitPod](https://www.gitpod.io/docs/).
Click on the badge above to try it out the project in action and start editing
the source code! By default Drupal and Elm client is accessible publicly and you
can access other DDEV services like Mailhog using the non-HTTPS port, for instance
`8026-` should work for checking the outgoing mails.
Primary ports:
 - `8888` for Drupal
 - `3000` for Elm frontend

### GitPod first steps
1. Wait until the Drupal login page shows up
1. Login with `admin` / `admin` into the Drupal backend.
1. Choose "Remote Explorer" on the left and open port 3000 too, either in new browser window or in preview.
1. Use `12345678` as the pairing code (tied to Device nodes at the Drupal side).
1. Use `1234` as the PIN code (tied to the user accounts at the Drupal side).
1. Explore the system.

## Introduction

This app is now using Progressive Web App techniques in order to load static
assets (Javascript, images, CSS, etc.) while offline. This has several
implications.

- We have some fairly strict requirements for browser compatibility at the
  moment, for the sake of simplifying the implementation of offline support and
  photo uploading. Basically, you need a recent version of Chrome (Chrome 60 or
  later) -- otherwise, various things will fail, without helpful error messages.

- The app will "take over" the URL you use (e.g. ***REMOVED***)
  and serve itself from the cache. This isn't a big deal for
  ***REMOVED***, but is more of an issue for
  http://localhost:3000/. To re-use http://localhost:3000/ for something else,
  you can un-register the app in Chrome by opening the Developer Tools, and
  looking at "Service Workers" in the "Application" tab.

There are several other things which differ from the normal development cycle.
Here is a checklist.

- You can start fresh as usual with an `./install -dy`

- If you have used the app before, you'll need to delete a bunch of things that
  the browser caches. The easiest way is to use Chrome's developer tools to do
  this.
  
  - Go to Chrome's dev tools (View -> Developer -> Developer Tools).
  - Go to the `Application` tab.
  - In the sidebar, there will be a "Clear Storage" heading ... click on that.
  - You'll see a "Clear site data" button. Clicking it will clear everything
    out and start fresh.

  We could automate this ... that is, we could have a button in the app itself
  to delete everything and start over. Or, we could detect the situation where
  the backend is actually a fresh `install -dy`. However, we wouldn't
  necessarily want to make that very easy in production! In any event, they are
  features we could add if it makes development and testing easier.

- If you reload the browser after "Clear Site Data", you'll get to the "Device
  Status" page. You'll need to enter a pairing code to authorize this device to
  sync with the backend. The migrations that `install -dy` performs will setup
  a pairing code of "12345678". So, you can use that to pair your device.  (In
  production, there is a "device" entity on the backend with a pairing code
  field to set).

- Once you've entered your pairing code, the app will ask you to log in with a
  PIN code. The `install -dy` process sets up a PIN code of "1234" that you can
  use (you'll be nurse Maya). In production, there is a `nurse` entity that has
  a PIN code field.

- Once you've logged in with your PIN code, you'll be offered a couple of
  buttons.  You can check "Device Status" or "Select Your Clinic". At this
  point, "Select Your Clinic" isn't interesting yet, because we first need to
  select which health centers this device will work with. So, start with
  "Device Status".

- The device status page allows you to initiate a manual sync with the backend.
  You can also choose which health centers to sync. I have been choosing
  "Nyange Health Center" to test with. (Automatic syncs will also happen
  periodically).  You can also reload the browser to make a sync happen.

- Once you've chosen some health centers, you can go back and select your
  clinic.  For instance, having chosen "Nyange Health Center", I now get
  offered "Nyange II" on the "Select your Clinic" page.

- Clicking on that will take you to the clinic page, showing you recent and
  upcoming group sessions. The `install -dy` process will automatically create
  a session for every clinic starting today. If you need to do that again (for
  instance, tomorrow), then there is a drush command which will do that ....
  `drush cst`). (You'll need to sync for the browser to then see it -- either
  on the device status page, or just reload the browser).

- You can then click on the "attendance" button to get into the familiar UI for
  sessions.

So, that should get you started from an `install -dy`.

Another thing you should be aware of is that when you make changes to the
frontend, you actually need to "activate" those changes within the app.
Refreshing the browser by itself is not enough, because it simply accesses
the old version from its cache.

To activate the new version you've just created, click on the "Version"
indication at the top-right corner of the app. That will take you to a page
which allows you to check for updates and activate updates.


## Backend


## Installation

### Install with DDEV

#### Requirements

 - https://ddev.readthedocs.io/en/latest/#installation
 - Drush 8 or earlier: https://docs.drush.org/en/master/install/#drupal-compatibility - that supports Drupal 7.

#### Steps

        cp default.config.sh config.sh
        cp .ddev/config.local.yaml.example .ddev/config.local.yaml
        ddev restart

Migrate content with either `ddev migrate default` or `ddev migrate sample`
depending on whether you want minimal development content or a full set of
sample content (takes much longer).

#### The install script will perform following steps:

1. Delete the /www folder.
2. Recreate the /www folder.
3. Download and extract all contrib modules, themes & libraries to the proper
   subfolders of the profile.
4. Download and extract Drupal 7 core in the /www folder
5. Create an empty sites/default/files directory
6. Makes a symlink within the /www/profiles directory to the /hedley
   directory.
7. Run the Drupal installer (Drush) using the Hedley profile.

#### Warning!

* The install script will not preserve the data located in the
  sites/default/files directory.
* The install script will clear the database during the installation.

## Deploy to Pantheon

### Prerequisites

Prepare `Config.Deploy.elm` based on `Config.elm` that holds the
infrastructure-related data for Elm. This file is gitignored, and it is
used during Elm compilation, before the final artifact is pushed to Pantheon.

### Steps

```
ddev start
ddev gulp publish
ddev robo deploy:pantheon
```

To generate the release notes, use `ddev robo generate:release-notes prev-tag`.
