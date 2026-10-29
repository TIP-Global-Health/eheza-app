<?php

/**
 * @file
 * Standalone tests of saving completion data on encounters.
 *
 * Runs without Drupal bootstrap: hedley_reports.module is loaded directly.
 * node_load() returns the stored copy set by saves_for(), and node_save()
 * keeps the node it was given.
 */

define('LANGUAGE_NONE', 'und');

require_once dirname(__DIR__) . '/hedley_reports.module';
require_once __DIR__ . '/assertions.inc';

$passed = 0;
$failed = 0;

/**
 * Keeps the stored nodes, and the nodes saved since the last reset.
 */
function storage($key, $value = NULL, $reset = FALSE) {
  static $storage = ['stored' => [], 'saved' => []];
  if ($reset) {
    $storage = ['stored' => [], 'saved' => []];
  }
  elseif ($value !== NULL) {
    $storage[$key][] = $value;
  }
  return $key ? $storage[$key] : NULL;
}

/**
 * Returns the stored copy of a node, or FALSE when there is none.
 */
function node_load($nid, $vid = NULL, $reset = FALSE) {
  foreach (storage('stored') as $node) {
    if ($node->nid == $nid) {
      return clone $node;
    }
  }
  return FALSE;
}

/**
 * Keeps the saved node instead of writing it.
 */
function node_save($node) {
  storage('saved', $node);
}

/**
 * Builds an encounter node, optionally holding completion data.
 */
function encounter_node($stored = NULL) {
  $node = new stdClass();
  $node->nid = 1;
  if (isset($stored)) {
    $node->field_reports_data[LANGUAGE_NONE][0]['value'] = $stored;
  }
  return $node;
}

/**
 * Stores completion data on a node and returns the nodes saved.
 *
 * The stored copy is $node itself unless $stored_copy is given; FALSE means
 * the node is no longer stored.
 */
function saves_for($node, $completion_data, $stored_copy = NULL) {
  storage(NULL, NULL, TRUE);
  if ($stored_copy !== FALSE) {
    storage('stored', $stored_copy ?: clone $node);
  }
  hedley_reports_save_completion_data($node, $completion_data);
  return storage('saved');
}

$data = ['taken_by' => 'nurse', 'completion' => ['a' => 1]];
$encoded = json_encode($data);
$other = json_encode(['taken_by' => 'chw']);

assert_same(count(saves_for(encounter_node(), $data)), 1, 'an encounter with no completion data is saved', $passed, $failed);

assert_same(count(saves_for(encounter_node(''), $data)), 1, 'an encounter with empty completion data is saved', $passed, $failed);

assert_same(count(saves_for(encounter_node($encoded), $data)), 0, 'unchanged completion data is not saved again', $passed, $failed);

$saved = saves_for(encounter_node($other), $data);
assert_same(count($saved), 1, 'changed completion data is saved', $passed, $failed);
assert_same($saved[0]->field_reports_data[LANGUAGE_NONE][0]['value'], $encoded, 'the saved node holds the new completion data', $passed, $failed);

$reordered = json_encode(['completion' => ['a' => 1], 'taken_by' => 'nurse']);
assert_same(count(saves_for(encounter_node($reordered), $data)), 1, 'the same data encoded differently is saved', $passed, $failed);

// The node was loaded before the data was built; the stored copy may differ.
$edited = encounter_node($other);
$edited->field_edited[LANGUAGE_NONE][0]['value'] = 'edited by a device';
$saved = saves_for(encounter_node($other), $data, $edited);
assert_same(count($saved) === 1 && isset($saved[0]->field_edited), TRUE, 'an edit stored since the node was loaded is kept', $passed, $failed);

assert_same(count(saves_for(encounter_node($other), $data, encounter_node($encoded))), 0, 'no save when the stored copy already holds the data', $passed, $failed);

assert_same(count(saves_for(encounter_node(), $data, FALSE)), 0, 'no save when the node was deleted since it was loaded', $passed, $failed);

report_results('Completion data save', $passed, $failed);
