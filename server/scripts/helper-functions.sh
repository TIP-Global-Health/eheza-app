#!/bin/bash

################################################################################
#
# Helper functions so we can reuse code in different scripts!
#
################################################################################

##
# Before doing anything, make sure that new Drush commands will be recognized.
##
if [[ -d "$ROOT"/www ]]; then
  drush -q -r "$ROOT"/www cc drush
fi

##
# Load the configuration file.
# Will exit with an error message if the configuration file does not exists!
##
function load_config_file {
  # Check if the config file exists.
  if [ ! -f "$ROOT"/config.sh ]; then
    echo
    echo -e  "${BGRED}                                                                 ${RESTORE}"
    echo -e "${BGLRED}  ERROR: No configuration file found!                            ${RESTORE}"
    echo -e  "${BGRED}  > Check if the ${BGLRED}config.sh${BGRED} file exists in the same               ${RESTORE}"
    echo -e  "${BGRED}    directory of the ${BGLRED}install${BGRED} script.                             ${RESTORE}"
    echo -e  "${BGRED}  > If not create one by creating a copy of ${BGLRED}default.config.sh${BGRED}.   ${RESTORE}"
    echo -e  "${BGRED}                                                                 ${RESTORE}"
    echo
    exit 1
  fi

  # Include the configuration file.
  source "$ROOT"/config.sh
}


##
# Check the configuration from the config file.
#
# This checks:
# - If the configured profile exists.
##
function check_config_file {
  # Check if the $PROFILE_NAME is defined.
  if [ ! "$PROFILE_NAME" ]; then
    echo
    echo -e  "${BGRED}                                                                 ${RESTORE}"
    echo -e "${BGLRED}  ERROR: No profile in the config file!                          ${RESTORE}"
    echo -e  "${BGRED}  > Check and add the profile name in the ${BGLRED}config.sh${BGRED} file.        ${RESTORE}"
    echo -e  "${BGRED}                                                                 ${RESTORE}"
    echo
    exit 1
  fi

  # Check if there is a folder with the $PROFILE_NAME.
  if [ ! -d "$ROOT"/"$PROFILE_NAME" ]; then
    TITLE=$(fill_string_spaces "ERROR: No profile with the name $PROFILE_NAME" 61)
    echo
    echo -e  "${BGRED}                                                                 ${RESTORE}"
    echo -e "${BGLRED}  $TITLE  ${RESTORE}"
    echo -e  "${BGRED}  > Check the profile name in the ${BGLRED}config.sh${BGRED} file.                ${RESTORE}"
    echo -e  "${BGRED}                                                                 ${RESTORE}"
    echo
    exit 1
  fi
}



##
# Cleanup the sites/default/ directory:
# - Removes the files directory.
# - Removes the settings.php file.
#
# Uses (requests) sudo powers if needed!
##
function delete_sites_default_content {
  # Cleanup the www/sites/default content.
  if [ -d "$ROOT"/www/sites ]; then
    echo -e "${LBLUE}> Cleaning up the sites/default directory${RESTORE}"
    chmod 777 "$ROOT"/www/sites/default
    rm -rf "$ROOT"/www/sites/default/files
    rm -f "$ROOT"/www/sites/default/settings.php
    echo
  fi

  # Backup in case of we need sudo powers to get rid of the files directory.
  if [ -d "$ROOT"/www/sites/default/files ]; then
    echo -e "${LBLUE}> Cleaning up the sites/default/files directory with sudo power!${RESTORE}"
    sudo rm -rf "$ROOT"/www/sites/default/files
    echo
  fi

  # Backup in case of we need sudo powers to get rid of the settings.php directory.
  if [ -f "$ROOT"/www/sites/default/settings.php ]; then
    echo -e "${LBLUE}> Cleaning up the sites/default/settings.php file with sudo power!${RESTORE}"
    sudo rm -rf "$ROOT"/www/sites/default/settings.php
    echo
  fi
}


##
# Cleanup the profile/ directory:
# - Remove contributed modules (modules/contrib).
# - Remove development modules (modules/development).
# - Remove contributed themes (themes/contrib).
# - Remove libraries (libraries).
##
function delete_profile_contrib {
  # Cleanup the contrib modules
  if [ -d "$ROOT"/"$PROFILE_NAME"/modules/contrib ]; then
    echo -e "${LBLUE}> Cleaning up the $PROFILE_NAME/modules/contrib directory${RESTORE}"
    rm -rf "$ROOT"/"$PROFILE_NAME"/modules/contrib
    echo
  fi

  # Cleanup the development modules
  if [ -d "$ROOT"/"$PROFILE_NAME"/modules/development ]; then
    echo -e "${LBLUE}> Cleaning up the $PROFILE_NAME/modules/development directory${RESTORE}"
    rm -rf "$ROOT"/"$PROFILE_NAME"/modules/development
    echo
  fi

  # Cleanup the contrib themes
  if [ -d "$ROOT"/"$PROFILE_NAME"/themes/contrib ]; then
    echo -e "${LBLUE}> Cleaning up the $PROFILE_NAME/themes/contrib directory${RESTORE}"
    rm -rf "$ROOT"/"$PROFILE_NAME"/themes/contrib
    echo
  fi

  # Cleanup the libraries folder
  if [ -d "$ROOT"/"$PROFILE_NAME"/libraries ]; then
    echo -e "${LBLUE}> Cleaning up the $PROFILE_NAME/libraries directory${RESTORE}"
    rm -rf "$ROOT"/"$PROFILE_NAME"/libraries
    echo
  fi
}


##
# Delete all the content within the /www folder.
##
function delete_www_content {
  if [ -d "$ROOT"/www/sites/default ]; then
    chmod 777 "$ROOT"/www/sites/default
  fi

  if [ -d "$ROOT"/www/sites ]; then
    echo -e "${LBLUE}> Cleaning up the www directory${RESTORE}"
    rm -rf "$ROOT"/www/
    echo
  fi

  # Create the www directory if necessary.
  if [ ! -d "$ROOT"/www ]; then
    echo -e "${LBLUE}> Creating an empty www directory${RESTORE}"
    mkdir "$ROOT"/www
    echo
  fi
}


##
# Download & extract Drupal core + contrib based on the make files.
##
function drupal_make {
  echo -e "${LBLUE}> Run the build script (scripts/build)${RESTORE}"
  if [ ! "$NATIVE_INSTALL" ]; then
    ddev . "cd .. && scripts/build"
  else
    bash "$ROOT"/scripts/build
  fi
  echo
}


##
# Install the profile as configured in the config.sh file.
##
function install_drupal_profile {
  echo -e "${LBLUE}> Install Drupal with the $PROFILE_NAME install profile${RESTORE}"

  cd "$ROOT"/www
  drush si -y "$PROFILE_NAME" \
    --locale=en \
    --account-name="$ADMIN_USERNAME" \
    --account-pass="$ADMIN_PASSWORD" \
    --account-mail="$ADMIN_EMAIL" \
    --db-url="mysql://$MYSQL_USERNAME:$MYSQL_PASSWORD@$MYSQL_HOSTNAME/$MYSQL_DB_NAME" \
    --uri="$BASE_DOMAIN_URL"
  echo

  cd "$ROOT"
}


##
# Composer install.
##
function composer_install {
  echo -e "${LBLUE}> Composer install${RESTORE}"

  cd "$ROOT"/www/sites/default/files/composer
  composer install
  echo

  cd "$ROOT"
}

##
# Create (if not exists) and set the proper file permissions
# on the sites/default/files directory.
##
function create_sites_default_files_directory {
  if [ ! -d "$ROOT"/www/sites/default/files ]; then
    echo -e "${LBLUE}> Create the files directory (sites/default/files directory)${RESTORE}"
    mkdir -p "$ROOT"/www/sites/default/files
    mkdir -p "$ROOT"/www/sites/default/files/private
  fi

  echo -e "${LBLUE}> Set the file permissions on the sites/default/files directory${RESTORE}"
  chmod -R 777 "$ROOT"/www/sites/default/files
  umask 000 "$ROOT"/www/sites/default/files
  chmod -R g+s "$ROOT"/www/sites/default/files
  echo
}


##
# Enable the development modules.
##
function enable_development_modules {
  echo -e "${LBLUE}> Enabling the development modules${RESTORE}"
  cd "$ROOT"/www
  drush en -y devel views_ui field_ui
  cd "$ROOT"
  echo
}

##
# Do dummy content migration.
##
function import_demo_content {
  echo -e "${LBLUE}> Importing demo data${RESTORE}"
  cd "$ROOT"/www

  # Check if migrate module is available
  MIGRATE_UI=$(drush pm-list --pipe --type=module | grep "^migrate_ui$")
  MIGRATE_EXTRAS=$(drush pm-list --pipe --type=module | grep "^migrate_extras$")
  if [ "$MIGRATE_UI" ] && [ "$MIGRATE_EXTRAS" ]; then
    drush en -y hedley_migrate
    drush en -y migrate migrate_ui migrate_extras
    drush mi --group=default --user=1
    drush mi --group=counseling --user=1
    drush mi --group=forms --user=1
  else
    echo -e  "${BGYELLOW}                                                                 ${RESTORE}"
    echo -e "${BGLYELLOW}  Migrate and or Migrate Extras module(s) are not available!     ${RESTORE}"
    echo -e  "${BGYELLOW}  You need to include:                                           ${RESTORE}"
    echo -e  "${BGYELLOW}    - migrate                                                    ${RESTORE}"
    echo -e  "${BGYELLOW}    - migrate_extras                                             ${RESTORE}"
    echo -e  "${BGYELLOW}  modules in the drupal-org.make file                            ${RESTORE}"
    echo -e  "${BGYELLOW}                                                                 ${RESTORE}"
  fi

  # Make sure we have random content for all the existing content-types and
  # random taxonomy terms for all the existing vocabularies.
  # generate_demo_content

  cd "$ROOT"
  echo
}

##
# Generating demo content including users, content types and vocabularies.
#
# For content types we based on the 'node_type' table, and for vocabularies on
# the 'taxonomy_vocabulary' table.
##
function generate_demo_content {
  # @todo: Replace this with CSV generated data.
  echo -e "${LBLUE}> Starting the process of generating demo content using the devel_generate module.${RESTORE}"
  cd "$ROOT"/www

  # Make sure devel-generate is enabled.
  drush en devel_generate -y

  # Generating taxonomy terms for all defined vocabularies.
  # Taxonomy terms has no dependencies, hence we can automate the list.
  VOCABS=$(drush sqlq "SELECT machine_name FROM taxonomy_vocabulary")
  for VOCAB in $(echo "$VOCABS" | tr ";" "\n")
  do
    echo -e "${LBLUE}Generating terms of vocabulary: $VOCAB ${RESTORE}"
    drush generate-terms "$VOCAB"
  done

  # Generating all types of nodes.
  # Hardcoding the list because of the dependencies between them.
  TYPES=(
    height
    muac
    nutrition
    photo
    weight
    family_planning
  )
  for TYPE in "${TYPES[@]}"
  do
    echo -e "${LBLUE}Generating nodes of type: $TYPE ${RESTORE}"
    drush generate-content 20 0 --types="$TYPE" --skip-fields=field_uuid,field_shards,field_bmi,field_zscore_age,field_zscore_bmi,field_zscore_length
  done

  cd "$ROOT"
  echo
}

##
# Fill string with spaces until required length.
#
# @param string The string.
# @param int The requested total length.
##
function fill_string_spaces {
  STRING="$1"
  STRING_LENGTH=${#STRING}
  DESIRED_LENGTH="$2"
  SPACES_LENGTH=$((DESIRED_LENGTH-STRING_LENGTH))

  if [[ 0 -gt "$SPACES_LENGTH" ]]; then
    SPACES_LENGTH=0
  fi

  printf -v SPACES '%*s' $SPACES_LENGTH
  echo "$STRING$SPACES"
}


##
# Login to Drupal as Administrator using the one time login link.
#
# This command does the login for you when the build script is done.
# It will open a new tab in your default browser and login to your project as
# the Administrator.
##
function drupal_login {
  cd www
  drush uli --uri="$BASE_DOMAIN_URL"
  cd ..
}

##
# Symlink external folders into www folder.
#
# This will use the SYMLINKS array from the config.sh file and create
# the symlinks relative to the www folder in the folder structure.
##
function symlink_externals {
  echo -e "${LBLUE}> Symlinking external directories & files${RESTORE}"
  if [ ${#SYMLINKS[@]} -eq 0 ]; then
    echo "No directories or files to symlink."
    echo
    return 0
  fi

  # Loop trough the symlinks configuration.
  for SOURCETARGET in "${SYMLINKS[@]}"; do
    mapfile -t paths < <(echo "$SOURCETARGET" | tr ">" "\n")
    path_source=${paths[0]}
    path_target="$ROOT/www/${paths[1]}"
    basepath_target=${path_target%/*}

    # check if the source exists
    if [ ! -e "$path_source" ] && [ ! -L "$path_source" ]; then
      echo "Source does not exists"
      echo "  ($path_source)"
      continue
    fi

    # Check if the target does not exist.
    if [ -e "$path_target" ] || [ -L "$path_target" ]; then
      echo "Target already exists"
      echo "  ($path_target)"
      continue
    fi

    # create basepath of the target if does not already exists.
    if [ ! -d "$basepath_target" ]; then
      mkdir -p "$basepath_target"
    fi

    # Create the symlink
    ln -s "$path_source" "$path_target"
    echo "Created symlink for $path_source"
    echo "  > as $path_target"

  done
  echo
}

##
# Check if there is a post script and run it.
#
# @param string $1
#   The kind of post script to run.
##
function run_post_script {
  if [ ! "$1" ]; then
    return 1
  fi

  # Define post script name.
  POST_FUNCT_NAME="post_$1"

  # Check if the function is declared.
  declare -Ff "$POST_FUNCT_NAME" >/dev/null;
  if [ $? -eq 1 ]; then
    return 1
  fi

  # Run the post script.
  echo -e "${LBLUE}> Run $POST_FUNCT_NAME script.${RESTORE}"
  $POST_FUNCT_NAME
  echo
}
