#!/bin/bash
set -e

# 1. Make sure you have the alias setup (use `drush sa` too see the aliases).
# 2. Make sure you have Terminus (from Pantheon.io) in your path.

BASE_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source "$BASE_DIR"/config.sh

# Some variable definitions
NORMAL=$(tput sgr0)

source "$BASE_DIR"/scripts/helper-colors.sh
source "$BASE_DIR"/scripts/helper-functions.sh

##
# Function to explain the script arguments.
##
function arguments_usage {
  USAGE=$(fill_string_spaces "Usage: $0 [pantheon-branch-name] [options]" 61)
  TITLE=$(fill_string_spaces "Install $PROFILE on Pantheon" 61)
  DEFAULT=$(fill_string_spaces "Defaults to $PANTHEON_BRANCH." 61)
  echo
  echo -e  "${BGCYAN}                                                                 ${RESTORE}"
  echo -e "${BGLCYAN}  $TITLE  ${RESTORE}"
  echo -e  "${BGCYAN}  $USAGE  ${RESTORE}"
  echo -e  "${BGCYAN}                                                                 ${RESTORE}"
  echo -e  "${BGCYAN}  pantheon-branch-name:                                          ${RESTORE}"
  echo -e  "${BGCYAN}  Optional argument to specify the remote branch at the Pantheon ${RESTORE}"
  echo -e  "${BGCYAN}  repository.                                                    ${RESTORE}"
  echo -e  "${BGCYAN}  $DEFAULT  ${RESTORE}"
  echo -e  "${BGCYAN}                                                                 ${RESTORE}"
  echo -e  "${BGCYAN}  OPTIONS:                                                       ${RESTORE}"
  echo -e  "${BGCYAN}    -h   Show this message.                                      ${RESTORE}"
  echo -e  "${BGCYAN}    -l   Open a new tab in your default browser and login to     ${RESTORE}"
  echo -e  "${BGCYAN}         your project as the Administrator.                      ${RESTORE}"
  echo
}

if [[ $CI != true]];
then

# Check and process arguments.
# See http://rsalveti.wordpress.com/2007/04/03/bash-parsing-arguments-with-getopts/
while getopts "dl" OPTION
do
  case $OPTION in
    l)
      AUTO_LOGIN=1
      ;;
    ?)
      arguments_usage
      exit
      ;;
  esac
done

if [ $# -ge 1 ]
then
    PANTHEON_BRANCH=$1
fi

# Check essential command-line tools.
if ! hash drush 2>/dev/null; then
  echo -e "${RED}Drush executable is not available ${RESTORE}"
  echo "http://docs.drush.org/en/master/install/"
  exit 1
fi

fi

echo -e "${GREEN}Resets Pantheon folder to $PANTHEON_BRANCH at Git.${NORMAL}\n"

cd "$PANTHEON_DIR"
git fetch
git clean -f
git reset --hard origin/"$PANTHEON_BRANCH"
git checkout -B "$PANTHEON_BRANCH"

cd "$MAKE_DIR"
ORIGIN_BRANCH=$(git rev-parse --abbrev-ref HEAD)
if [[ -n $(git status -s) ]]; then
  git status
  echo "$MAKE_DIR is dirty"
  exit 1
fi

echo -e "${GREEN}Sync new code at $ORIGIN_BRANCH branch into the Pantheon folder ($PANTHEON_BRANCH branch).${NORMAL}"
rsync -avzr --delete-after "$MAKE_DIR/$PROFILE/" "$PANTHEON_DIR/profiles/$PROFILE/"
rsync -avzr --delete-after "$MAKE_DIR"/www/sites/all/ "$PANTHEON_DIR"/sites/all/

echo -e "${GREEN}Re-building the app and copy to {$PANTHEON_DIR}/app.${NORMAL}"
cd $MAKE_DIR/../client
bower install
gulp publish
rm -rf $PANTHEON_DIR/app
mkdir $PANTHEON_DIR/app
cp -Ra dist/. $PANTHEON_DIR/app/

cd "$PANTHEON_DIR"
echo -e "${GREEN}Git commit new code.${NORMAL}\n"
git add . --all
git status

echo -e "${YELLOW}Sleeping for 5 seconds, you can abort the process before push by hitting Ctrl-C.${NORMAL}\n"
sleep 5
git commit -am "Site update from $ORIGIN_BRANCH"
git push

cd "$MAKE_DIR"

if [ $AUTO_LOGIN ]; then
  echo -e "${GREEN}Open site in browser.${NORMAL}\n"
  drush "$PANTHEON_ALIAS" uli
fi
