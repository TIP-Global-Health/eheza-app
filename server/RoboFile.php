<?php

use Robo\Tasks;
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
  const PANTHEON_NAME = 'eheza-app';

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
      throw new Exception('You need to fill the "PANTHEON_NAME" const in the Robo file, so it will know what is the name of your site.');
    }

    $site = getenv('CONFIG_SITE');
    if (!$site) {
      throw new Exception('Please specify CONFIG_SITE in your DDEV local config, so it will be possible to resolve pantheon directory.');
    }

    $pantheonDirectory = '.pantheon-' . $site;

    $result = $this
      ->taskExec('git status -s -uno')
      ->printOutput(FALSE)
      ->run();

    if ($result->getMessage()) {
      $this->yell($result->getMessage());
      // throw new Exception('The GitHub working directory is dirty. Please commit any pending changes or add to .gitignore.');
    }

    $result = $this
      ->taskExec("cd $pantheonDirectory && git status -s -uno")
      ->printOutput(FALSE)
      ->run();

    if ($result->getMessage()) {
      $this->yell($result->getMessage());
      throw new Exception('The Pantheon working directory is dirty. Please commit any pending changes or add to .gitignore.');
    }

    // Validate pantheon.upstream.yml.
    $pantheonConfig = $pantheonDirectory . '/pantheon.upstream.yml';
    if (!file_exists($pantheonConfig)) {
      throw new Exception("pantheon.upstream.yml is missing from the Pantheon directory ($pantheonDirectory)");
    }

    $yaml = Yaml::parseFile($pantheonConfig);
    if (empty($yaml['php_version'])) {
      throw new Exception("'php_version:' directive is missing from pantheon.upstream.yml in Pantheon directory ($pantheonDirectory)");
    }

    $result = $this
      ->taskExec("cd $pantheonDirectory && git checkout $branchName")
      ->printOutput(FALSE)
      ->run();

    if (!$result->wasSuccessful()) {
      throw new Exception("Specified branch $branchName does not exist.");
    }

    $rsyncExclude = [
      '.git',
      '.circleci',
      '.ddev',
      '.idea',
      '.pantheon',
      'sites/default',
      'pantheon.yml',
      'pantheon.upstream.yml',
      'client',
      'scalability-test',
      'infrastructure_setup',
    ];

    $rsyncExcludeString = '--exclude=' . implode(' --exclude=', $rsyncExclude);

    // Copy all files and folders of the Drupal installation.
    $server_sync_result = $this->_exec("rsync -az -q -L -K --delete $rsyncExcludeString www/. $pantheonDirectory")->getExitCode();
    if ($server_sync_result != 0) {
      throw new Exception('Failed to sync the server-side');
    }

    // Copy all the files and folders of the app.
    // Inside Docker, we do mount the client separately.
    $client_source = '/var/client';
    if (!file_exists($client_source)) {
      $client_source = '../client';
    }

    $client_sync_result = $this->_exec("rsync -az -q -L -K --delete $client_source/dist/. $pantheonDirectory/app")->getExitCode();
    if ($client_sync_result != 0) {
      throw new Exception('Failed to sync the client-side');
    }

    // We don't want to change Pantheon's git ignore, as we do want to commit
    // vendor and contrib directories.
    // @todo: Ignore it from rsync, but './.gitignore' didn't work.
    $this->_exec("cd $pantheonDirectory && git checkout .gitignore");

    $this->_exec("cd $pantheonDirectory && git status");

    $commitAndDeployConfirm = $this->confirm('Commit changes and deploy?', TRUE);
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

    $push_status = $this->_exec("cd $pantheonDirectory && git pull && git add . && git commit -am 'Site update' && git push")->getExitCode();
    if ($client_sync_result != 0) {
      throw new Exception('Failed to push to Pantheon');
    }

    $pantheonEnv = $branchName == 'master' ? 'dev' : $branchName;
    $this->deployPantheonSync($pantheonEnv, FALSE);
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
  public function deployPantheonSync(string $env = 'test', bool $doDeploy = TRUE) {
    if (getenv('PANTHEON_NAME')) {
      $pantheonName = getenv('PANTHEON_NAME');
    }
    else {
      $pantheonName = self::PANTHEON_NAME;
    }

    $pantheonTerminusEnvironment = $pantheonName . '.' . $env;

    $task = $this->taskExecStack();

    if ($doDeploy) {
      $task->exec("terminus env:deploy $pantheonTerminusEnvironment");
    }

    $task
      ->exec("terminus remote:drush $pantheonTerminusEnvironment -- cc all")
      // A second cache-clear, because Drupal...
      ->exec("terminus remote:drush $pantheonTerminusEnvironment -- cc all")
      ->exec("terminus remote:drush $pantheonTerminusEnvironment -- updb -y")
      ->exec("terminus remote:drush $pantheonTerminusEnvironment -- uli")
      ->run();
  }

  /**
   * Generates log of changes since the given tag.
   *
   * @param string|null $tag
   *   The git tag to compare since. Usually the tag from the previous release.
   *   If you're releasing for example 1.0.2, then you should get changes since
   *   1.0.1, so $tag = 1.0.1. Omit for detecting the last tag automatically.
   *
   * @throws \Exception
   */
  public function generateReleaseNotes($tag = NULL) {
    // Check if the specified tag exists or not.
    if (!empty($tag)) {
      $result = $this->taskExec("git tag | grep \"$tag\"")
        ->printOutput(FALSE)
        ->run()
        ->getMessage();
      if (empty($result)) {
        $this->say('The specified tag does not exist: ' . $tag);
      }
    }

    if (empty($result)) {
      $latest_tag = $this->taskExec("git tag --sort=version:refname | tail -n1")
        ->printOutput(FALSE)
        ->run()
        ->getMessage();
      if (empty($latest_tag)) {
        throw new Exception('There are no tags in this repository.');
      }
      if (!$this->confirm("Would you like to compare from the latest tag: $latest_tag?")) {
        $this->say("Specify the tag as an argument");
        exit(1);
      }
      $tag = $latest_tag;
    }

    // Detect organization / repository name from git remote.
    $remote = $this->taskExec("git remote get-url origin")
      ->printOutput(FALSE)
      ->run()
      ->getMessage();

    if (!empty($remote)) {
      $origin_parts = preg_split('/[:\/]/', str_replace('.git', '', $remote));
      if (!empty($origin_parts[1]) && !empty($origin_parts[2])) {
        $github_org = $origin_parts[1];
        $github_project = $origin_parts[2];
      }
    }

    if (!isset($github_org) || !isset($github_project)) {
      $this->say('No GitHub project or GitHub organization found, so not trying to fetch details from GitHub API.');
    }

    // This is the heart of the release notes, the git history, we get all the
    // merge commits since the specified last version and later on we parse
    // the output. Optionally we enrich it with metadata from GitHub REST API.
    $log = $this->taskExec("git log --merges --pretty=format:'%s¬¬|¬¬%b' $tag..")->printOutput(FALSE)->run()->getMessage();
    $lines = explode("\n", $log);

    $this->say('Copy release notes below');

    $this->printReleaseNotesTitle('Changelog');

    $pull_requests_per_issue = [];
    $no_issue_lines = [];
    $contributors = [];
    $issue_titles = [];
    $additions = 0;
    $deletions = 0;
    $changed_files = 0;

    foreach ($lines as $line) {
      $log_messages = explode("¬¬|¬¬", $line);
      $pr_matches = [];
      preg_match_all('/Merge pull request #([0-9]+)/', $line, $pr_matches);

      if (count($log_messages) < 2) {
        // No log message at all, not meaningful for changelog.
        continue;
      }

      if (!isset($pr_matches[1][0])) {
        // Could not detect PR number.
        continue;
      }

      $log_messages[1] = trim($log_messages[1]);
      if (empty($log_messages[1])) {
        // Whitespace-only log message, not meaningful for changelog.
        continue;
      }
      $pr_number = $pr_matches[1][0];
      if (isset($github_org)) {
        $pr_details = $this->githubApiGet("repos/$github_org/$github_project/pulls/$pr_number");
        if (!empty($pr_details->user)) {
          $contributors[] = '@' . $pr_details->user->login;
          $additions += $pr_details->additions;
          $deletions += $pr_details->deletions;
          $changed_files += $pr_details->changed_files;
        }
      }

      // The issue number is a required part of the branch name,
      // So usually we can grab it from the log too, but that's optional
      // If we cannot detect it, we still print a less verbose changelog line.
      $issue_matches = [];
      preg_match_all('!from [a-zA-Z-_0-9]+/([0-9]+)!', $line, $issue_matches);

      if (isset($issue_matches[1][0])) {
        $issue_number = $issue_matches[1][0];
        if (!isset($issue_titles[$issue_number]) && isset($github_org)) {
          $issue_details = $this->githubApiGet("repos/$github_org/$github_project/issues/$issue_number");
          if (!empty($issue_details->title)) {
            $issue_titles[$issue_number] = $issue_details->title;
            $contributors[] = '@' . $issue_details->user->login;
          }
        }

        if (isset($issue_titles[$issue_number])) {
          $issue_line = "- {$issue_titles[$issue_number]} (#{$issue_number})";
        }
        else {
          $issue_line = "- Issue #{$issue_number}";
        }
        if (!isset($pull_requests_per_issue[$issue_line])) {
          $pull_requests_per_issue[$issue_line] = [];
        }
        $pull_requests_per_issue[$issue_line][] = "  - {$log_messages[1]} (#{$pr_matches[1][0]})";
      }
      else {
        $no_issue_lines[] = "- {$log_messages[1]} (#$pr_number)";
      }
    }

    foreach ($pull_requests_per_issue as $issue_line => $pr_lines) {
      print $issue_line . "\n";
      foreach ($pr_lines as $pr_line) {
        print $pr_line . "\n";
      }
    }

    $this->printReleaseNotesSection('', $no_issue_lines);

    if (isset($github_org)) {
      $contributors = array_unique($contributors);
      sort($contributors);
      $this->printReleaseNotesSection('Contributors', $contributors);

      $this->printReleaseNotesSection('Code statistics', [
        "Lines added: $additions",
        "Lines deleted: $deletions",
        "Files changed: $changed_files",
      ]);
    }
  }

  /**
   * Print a section for the release notes.
   *
   * @param string $title
   *   Section title.
   * @param array $lines
   *   Bulletpoints.
   */
  protected function printReleaseNotesSection(string $title, array $lines) {
    if (!empty($title)) {
      $this->printReleaseNotesTitle($title);
    }
    foreach ($lines as $line) {
      if (substr($line, 0, 1) == '-') {
        print "$line\n";
      }
      else {
        print "- $line\n";
      }
    }
  }

  /**
   * Print a title for the release notes.
   *
   * @param string $title
   *   Section title.
   */
  protected function printReleaseNotesTitle($title) {
    echo "\n\n## $title\n";
  }

  /**
   * Performs a GET request towards GitHub API using personal access token.
   *
   * @param string $path
   *   Resource/path to GET.
   *
   * @return mixed|null
   *   Decoded response or NULL.
   *
   * @throws \Exception
   */
  protected function githubApiGet($path) {
    $token = getenv('GITHUB_ACCESS_TOKEN');
    $username = getenv('GITHUB_USERNAME');
    if (empty($token)) {
      throw new Exception('Specify the personal access token in GITHUB_ACCESS_TOKEN environment variable before invoking the release notes generator in order to be able to fetch details of issues and pull requests');
    }
    if (empty($username)) {
      throw new Exception('Specify the GitHub username in GITHUB_USERNAME environment variable before invoking the release notes generator in order to be able to fetch details of issues and pull requests');
    }
    // We might not have a sane Drupal instance, let's not rely on Drupal API
    // to generate release notes.
    $ch = curl_init('https://api.github.com/' . $path);
    curl_setopt($ch, CURLOPT_USERAGENT, 'Drupal Starter Release Notes Generator');
    curl_setopt($ch, CURLOPT_USERPWD, $username . ":" . $token);
    curl_setopt($ch, CURLOPT_TIMEOUT, 30);
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, TRUE);
    $result = curl_exec($ch);
    curl_close($ch);
    return empty($result) ? NULL : json_decode($result);
  }

  /**
   * Generates the demographics report.
   */
  public function reportDemographics($limit_date = NULL, $region = NULL) {
    if (empty($limit_date)) {
      $limit_date = date('Y-m-d');
    }
    $this->_exec("cd /var/www/html/server/www && drush scr profiles/hedley/modules/custom/hedley_admin/scripts/generate-demographics-report.php --limit_date=$limit_date --region=$region");
  }

  /**
   * Generates the demographics report.
   */
  public function reportDemographicsHc($limit_date = NULL, $region = NULL) {
    if (empty($limit_date)) {
      $limit_date = date('Y-m-d');
    }
    $this->_exec("cd /var/www/html/server/www && drush scr profiles/hedley/modules/custom/hedley_admin/scripts/generate-demographics-hc-report.php --limit_date=$limit_date --region=$region");
  }

  /**
   * Generates the acute illness report.
   */
  public function reportAcuteIllness($start_date = NULL, $end_date = NULL, $region = NULL) {
    $this->_exec("cd /var/www/html/server/www && drush scr profiles/hedley/modules/custom/hedley_admin/scripts/generate-acute-illness-report.php --start_date=$start_date --end_date=$end_date --region=$region");
  }

  /**
   * Generates the ANC report.
   */
  public function reportAnc($limit_date = NULL) {
    if (empty($limit_date)) {
      $limit_date = date('Y-m-d');
    }
    $this->_exec("cd /var/www/html/server/www && drush scr profiles/hedley/modules/custom/hedley_admin/scripts/generate-anc-report.php --limit_date=$limit_date");
  }

  /**
   * Generates the pregnancy report.
   */
  public function reportPregnancy($limit_date = NULL) {
    if (empty($limit_date)) {
      $limit_date = date('Y-m-d');
    }
    $this->_exec("cd /var/www/html/server/www && drush scr profiles/hedley/modules/custom/hedley_admin/scripts/generate-closed-pregnancies-report.php --limit_date=$limit_date");
  }

  /**
   * Generates the nutrition report.
   */
  public function reportNutrition($region = NULL) {
    $this->_exec("cd /var/www/html/server/www && drush scr profiles/hedley/modules/custom/hedley_admin/scripts/generate-nutrition-report.php --region=$region");
  }

}
