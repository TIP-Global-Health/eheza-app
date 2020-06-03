<?php

use Lurker\Event\FilesystemEvent;
use Robo\Tasks;
use Symfony\Component\EventDispatcher\Event;
use Symfony\Component\Yaml\Yaml;

/**
 * Robo commmands.
 */
class RoboFile extends Tasks {

  /**
   * The Pantheon name.
   *
   * You need to fill this information for Robo to know what's the name of your
   * site.
   */
  const PANTHEON_NAME = 'ihangane';

  /**
   * Deploy to Pantheon.
   *
   * @param string $branchName
   *   The branch name to commit to. Default to master.
   *
   * @throws \Exception
   */
  public function deployPantheon($branchName = 'master') {
    if (empty(self::PANTHEON_NAME)) {
      throw new Exception('You need to fill the "PANTHEON_NAME" const in the Robo file. so it will know what is the name of your site.');
    }

    $pantheonDirectory = '.pantheon';

    $result = $this
      ->taskExec('git status -s')
      ->printOutput(FALSE)
      ->run();

    if ($result->getMessage()) {
      throw new Exception('The working directory is dirty. Please commit any pending changes.');
    }

    $result = $this
      ->taskExec("cd $pantheonDirectory && git status -s")
      ->printOutput(FALSE)
      ->run();

    if ($result->getMessage()) {
      throw new Exception('The Pantheon directory is dirty. Please commit any pending changes.');
    }

    // Validate pantheon.upstream.yml.
    if (!file_exists($pantheonDirectory . '/pantheon.upstream.yml')) {
      throw new Exception("pantheon.upstream.yml is missing from the Pantheon directory ($pantheonDirectory)");
    }

    $yaml = Yaml::parseFile($pantheonDirectory . '/pantheon.yml');
    if (empty($yaml['php_version'])) {
      throw new Exception("'php_version:' directive is missing from pantheon.upstream.yml in Pantheon directory ($pantheonDirectory)");
    }

    $this->_exec("cd $pantheonDirectory && git checkout $branchName");

    $rsyncExclude = [
      '.git',
      '.ddev',
      '.idea',
      '.pantheon',
      'sites/default',
      'pantheon.yml',
      'pantheon.upstream.yml',
    ];

    $rsyncExcludeString = '--exclude=' . join(' --exclude=', $rsyncExclude);

    // Copy all files and folders of the Drupal installation.
    $this->_exec("rsync -az -q --delete $rsyncExcludeString . $pantheonDirectory");

    // Copy all the files and folders of the app.
    $this->_exec("rsync -az -q --delete $rsyncExcludeString ../client/dist $pantheonDirectory/app");

    // We don't want to change Pantheon's git ignore, as we do want to commit
    // vendor and contrib directories.
    // @todo: Ignore it from rsync, but './.gitignore' didn't work.
    $this->_exec("cd $pantheonDirectory && git checkout .gitignore");

    $this->_exec("cd $pantheonDirectory && git status");

    $commitAndDeployConfirm = $this->confirm('Commit changes and deploy?');
    if (!$commitAndDeployConfirm) {
      $this->say('Aborted commit and deploy, you can do it manually');

      // The Pantheon repo is dirty, so check if we want to clean it up before
      // exit.
      $cleanupPantheonDirectoryConfirm = $this->confirm("Revert any changes on $pantheonDirectory directory (i.e. `git checkout .`)?");
      if (!$cleanupPantheonDirectoryConfirm) {
        // Keep folder as is.
        return;
      }

      // We repeat "git clean" twice, as sometimes it seems that a single one
      // doesn't remove all directories.
      $this->_exec("cd $pantheonDirectory && git checkout . && git clean -fd && git clean -fd && git status");

      return;
    }

    $pantheonName = self::PANTHEON_NAME;
    $pantheonTerminusEnvironment = $pantheonName . '.dev';

    $this->_exec("cd $pantheonDirectory && git pull && git add . && git commit -am 'Site update' && git push");
    $this->deployPantheonSync('dev', false);
  }

  /**
   * Deploy site from one env to the other on Pantheon.
   *
   * @param string $env
   *   The environment to update.
   * @param bool $doDeploy
   *   Determine if a deploy should be done by terminus. That is, for example
   *   should TEST environment be updated from DEV.
   *
   * @throws \Robo\Exception\TaskException
   */
  public function deployPantheonSync(string $env = 'test', bool $doDeploy = true) {
    $pantheonName = self::PANTHEON_NAME;
    $pantheonTerminusEnvironment = $pantheonName . '.' . $env;

    $task = $this->taskExecStack();

    if ($doDeploy) {
      $task->exec("terminus env:deploy $pantheonTerminusEnvironment");
    }

    $task
      ->exec("terminus remote:drush $pantheonTerminusEnvironment -- cr")

      // A second cache-clear, because Drupal...
      ->exec("terminus remote:drush $pantheonTerminusEnvironment -- cr")
      ->exec("terminus remote:drush $pantheonTerminusEnvironment -- updb -y")

      // A second config import, because Drupal...
      ->exec("terminus remote:drush $pantheonTerminusEnvironment -- cim -y")
      ->exec("terminus remote:drush $pantheonTerminusEnvironment -- cim -y")
      ->exec("terminus remote:drush $pantheonTerminusEnvironment -- uli")
      ->run();
  }

}
