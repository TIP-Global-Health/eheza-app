<?php

/**
 * @file
 * Assertions shared by the standalone report tests.
 */

/**
 * Compares a result with the expected value and records the outcome.
 */
function assert_same($result, $expected, $case, &$passed, &$failed) {
  if ($result === $expected) {
    $passed++;
    return;
  }
  $failed++;
  echo "FAIL: $case\n  expected: " . var_export($expected, TRUE) . "\n  got: " . var_export($result, TRUE) . "\n";
}

/**
 * Prints the totals, and exits with 1 when any test failed.
 *
 * Example: report_results('Nutrition report', 10, 0) prints
 * "Nutrition report tests: 10 total, 10 passed, 0 failed." and "OK".
 */
function report_results($name, $passed, $failed) {
  $total = $passed + $failed;
  echo "\n";
  echo "$name tests: $total total, $passed passed, $failed failed.\n";

  if ($failed > 0) {
    echo "FAILED\n";
    exit(1);
  }

  echo "OK\n";
  exit(0);
}
