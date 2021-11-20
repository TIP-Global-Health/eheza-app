# Drupal 7 - Install Profile Hedley

This is a starting base to create Drupal 7 websites using an install profile.


## Installation

### Install with DDEV

#### Requirements

 - https://ddev.readthedocs.io/en/latest/#installation
 - Drush 8 or earlier: https://docs.drush.org/en/master/install/#drupal-compatibility - that supports Drupal 7.

#### Steps

(from repository root)

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
