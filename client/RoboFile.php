<?php

use Lurker\Event\FilesystemEvent;
use Symfony\Component\EventDispatcher\Event;

/**
 * This is project's console commands configuration for Robo task runner.
 *
 * @see http://robo.li/
 */
class RoboFile extends \Robo\Tasks {

  /**
   * The source path of the app.
   */
  const SOURCE_PATH = 'src/elm';

  /**
   * The compilation command to run.
   */
  const COMPILE_CMD = 'elm make ./src/elm/Main.elm';

  /**
   * Compile the app; On success ...
   *
   * @param bool $optimize
   *   Indicate whether to optimize during compilation.
   */
  private function compile_($optimize = FALSE) {
    $cmd = self::COMPILE_CMD;

    if ($optimize) {
      // Add optimization to the command.
      $cmd .= ' --optimize';
    }

    $result = $this->_exec($cmd);
    if ($result->getExitCode() === 0) {
      // Do something on success.
    }
  }

  /**
   * Compile the app (optimized).
   */
  function compile() {
    $this->say('Compiling (optimized).');
    $this->compile_(TRUE);
  }

  /**
   * Compile the app.
   *
   * Non-optimized, for `Debug.toString`.
   */
  function compileDebug() {
    $this->say('Compiling (non-optimized).');
    $this->compile_();
  }

  /**
   * Watch the source path and compile on change (optimized).
   */
  function watch() {
    $this->say('Compiling and watching (optimized).');
    $this->compile_(TRUE);

    $this->taskWatch()
         ->monitor(
           self::SOURCE_PATH,
           function (Event $event) {
             $this->compile_(TRUE);
           },
           FilesystemEvent::ALL
         )->run();
  }

  /**
   * Watch the source path and compile on change.
   *
   * Non-optimized, for `Debug.toString`.
   */
  function watchDebug() {
    $this->say('Compiling and watching (non-optimized).');
    $this->compile_();

    $this->taskWatch()
         ->monitor(
           self::SOURCE_PATH,
           function (Event $event) {
             $this->compile_();
           },
           FilesystemEvent::ALL
         )->run();
  }

}
