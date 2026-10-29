<?php

/**
 * @file
 * Standalone tests of saving completion data on encounters.
 *
 * Runs without Drupal bootstrap: hedley_reports.module is loaded directly,
 * and node_save() only records the node it was given.
 */

define('LANGUAGE_NONE', 'und');

require_once dirname(__DIR__) . '/hedley_reports.module';
require_once __DIR__ . '/assertions.inc';

$passed = 0;
$failed = 0;

/**
 * Counts saves instead of writing the node.
 */
function node_save($node) {
  save_count(1);
}

/**
 * Adds to the save count and returns it; a reset starts it again at 0.
 */
function save_count($add = 0, $reset = FALSE) {
  static $count = 0;
  $count = $reset ? 0 : $count + $add;
  return $count;
}

/**
 * Builds an encounter node, optionally holding stored completion data.
 */
function encounter_node($stored = NULL) {
  $node = new stdClass();
  if (isset($stored)) {
    $node->field_reports_data[LANGUAGE_NONE][0]['value'] = $stored;
  }
  return $node;
}

/**
 * Stores completion data and returns how many saves it made.
 */
function saves_for($node, $completion_data) {
  save_count(0, TRUE);
  hedley_reports_save_completion_data($node, $completion_data);
  return save_count();
}

$data = ['taken_by' => 'nurse', 'completion' => ['a' => 1]];

assert_same(saves_for(encounter_node(), $data), 1, 'an encounter with no completion data is saved', $passed, $failed);

assert_same(saves_for(encounter_node(json_encode($data)), $data), 0, 'unchanged completion data is not saved again', $passed, $failed);

$changed = encounter_node(json_encode(['taken_by' => 'chw', 'completion' => ['a' => 1]]));
assert_same(saves_for($changed, $data), 1, 'changed completion data is saved', $passed, $failed);
assert_same($changed->field_reports_data[LANGUAGE_NONE][0]['value'], json_encode($data), 'the saved node holds the new completion data', $passed, $failed);

assert_same(saves_for(encounter_node(''), $data), 1, 'an encounter with empty completion data is saved', $passed, $failed);

report_results('Completion data save', $passed, $failed);
