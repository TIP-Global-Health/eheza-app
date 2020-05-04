# Drupal 7 - Install Profile Hedley

This is a starting base to create Drupal 7 websites using an install profile.


## Installation

### Install with DDEV

  cp default.config.sh config.sh
  cp .ddev/config.local.yaml.example .ddev/config.local.yaml
  ddev restart

Migrate content with either `ddev migrate default` or `ddev migrate sample`
depending on whether you want minimal development content or a full set of
sample content (takes much longer).

### Native Install

**Warning:** you need to setup [Drush](https://github.com/drush-ops/drush)
first or the installation and update scripts will not work.

#### Create config file

Copy the example configuration file to config.sh:

	$ cp default.config.sh config.sh

Edit the configuration file, fill in the blanks.


#### Run the install script

Run the install script from within the root of the repository:

	$ ./install

You can login automatically when the installation is done. Add the -l argument
when you run the install script.

  $ ./install -l


#### Configure web server

Create a vhost for your webserver, point it to the `REPOSITORY/ROOT/www` folder.
(Restart/reload your webserver).

Add the local domain to your ```/etc/hosts``` file.

Open the URL in your favorite browser.



## Reinstall

You can Reinstall the platform any type by running the install script.

	$ ./install


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

**You need to take backups before you run the install script!**



## Upgrade

It is also possible to upgrade Drupal core and contributed modules and themes
without destroying the data in tha database and the sites/default directory.

Run the upgrade script:

	$ ./upgrade

You can login automatically when the upgrade is finished. Add the -l argument
when you run the upgrade script.

  $ ./upgrade -l


#### The upgrade script will perform following steps:

1. Create a backup of the sites/default folder.
2. Delete the /www folder.
3. Recreate the /www folder.
4. Download and extract all contrib modules, themes & libraries to the proper
   subfolders of the profile.
5. Download and extract Drupal 7 core in the /www folder.
6. Makes a symlink within the /www/profiles directory to the
   /hedley 7. directory.
7. Restore the backup of the sites/default folder.


## Deploy to Pantheon

1. There are two main things that should be checked before running the custom script:
    1. `config.sh`: `PANTHEON_DIR`, please make sure it matches the absolute path of your Pantheon repository.
    2. `config.sh`: `PANTHEON_ALIAS`, please make sure it matches the alias of the development site and this alias points to the DEV site and it works on your local Drush. Only needed for the one-time login link.
2. Alternatively you can configure the following:
    1. `config.sh`: `PANTHEON_BRANCH`, you can use this to deploy the current branch of your GitHub working copy to a multidev environment on Pantheon.
3. Run the script: `bash pantheon-update.sh`. During the process, you have the ability to interrupt the process by pressing Cntrl-C, if the output of `git show` shows abnormal changes. The script has the following command-line options:
   - `-l` : shows a one-time login link for the admin user to the site after the finished deployment and opens in in the browser.
   - `-h` : shows a summary of the command line options.
