<?php

/**
 * @file
 * Drush script to test the incident recovery mechanism.
 *
 * Usage:
 *   drush scr profiles/hedley/modules/custom/hedley_user/scripts/test_incident_recovery.php
 */

// Configuration constants.
define('PERSON_UUID', '9b062e92-b581-59f0-a04d-60f2a5cff28f');
define('NURSE_UUID', '3375800e-33c7-5b17-a601-9a436d2387b8');
define('HEALTH_CENTER_UUID', '764f6dc1-e6a6-5b4a-92e4-3238b86cb9c4');
define('TEST_USER_ID', 2);
define('WAIT_TIME', 3);

// Load test data definitions.
require_once __DIR__ . '/incident_test_data.php';

/**
 * Run a single test case.
 *
 * @param string $test_name
 *   Name of the test.
 * @param array $test_case
 *   The test case containing 'description' and 'entity'.
 *
 * @return bool
 *   TRUE if test passed, FALSE otherwise.
 */
function run_test($test_name, array $test_case) {
  $uid = TEST_USER_ID;
  $description = $test_case['description'];
  $entity = $test_case['entity'];

  drush_print("Running test: $test_name");
  drush_print("  Description: $description");
  drush_print("  Setting incident_details on user $uid...");

  // Load user and set incident details (wrap single entity in array).
  $wrapper = entity_metadata_wrapper('user', $uid);
  $json_data = json_encode([$entity]);
  $wrapper->field_incident_details->set($json_data);
  $wrapper->save();

  drush_print("  Incident details set. Waiting " . WAIT_TIME . " seconds...");
  sleep(WAIT_TIME);

  // Reload user and check if field is empty.
  entity_get_controller('user')->resetCache([$uid]);
  $wrapper = entity_metadata_wrapper('user', $uid);
  $current_details = $wrapper->field_incident_details->value();

  if (empty($current_details)) {
    drush_print("  ✓ PASSED: field_incident_details is empty (entity created successfully)");
    return TRUE;
  }
  else {
    drush_print("  ✗ FAILED: field_incident_details still contains data:");
    drush_print("    " . $current_details);
    return FALSE;
  }
}

/**
 * Main execution.
 */
function main() {
  drush_print("=== Incident Recovery Mechanism Test ===");
  drush_print("Person UUID: " . PERSON_UUID);
  drush_print("Nurse UUID: " . NURSE_UUID);
  drush_print("Health Center UUID: " . HEALTH_CENTER_UUID);
  drush_print("User ID: " . TEST_USER_ID);
  drush_print("");

  // Get all test cases from data file.
  $test_cases = get_test_cases(PERSON_UUID, NURSE_UUID, HEALTH_CENTER_UUID);

  $passed = 0;
  $failed = 0;

  foreach ($test_cases as $test_name => $test_case) {
    if (run_test($test_name, $test_case)) {
      $passed++;
    }
    else {
      $failed++;
    }
    drush_print("");
  }

  // Summary.
  drush_print("=== Test Summary ===");
  drush_print("Passed: $passed");
  drush_print("Failed: $failed");
  drush_print("Total: " . ($passed + $failed));

  if ($failed > 0) {
    drush_set_error('TEST_FAILED', "$failed test(s) failed.");
  }
}

main();
