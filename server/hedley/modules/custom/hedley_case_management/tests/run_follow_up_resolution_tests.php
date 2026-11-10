<?php

/**
 * @file
 * Standalone tests of which follow-ups a created node closes.
 *
 * Runs without Drupal bootstrap: hedley_case_management.module is loaded
 * directly, and nodes are held in memory. New nodes get rising node IDs, so
 * the order nodes are created in is the order they reached the server.
 */

define('LANGUAGE_NONE', 'und');
define('NODE_PUBLISHED', 1);

require_once dirname(__DIR__) . '/hedley_case_management.module';
require_once dirname(dirname(__DIR__)) . '/hedley_reports/tests/assertions.inc';

/**
 * The in-memory stand-in for an entity query over the stored nodes.
 */
class HedleyCaseManagementFakeQuery {

  /**
   * The conditions, each a function of a node returning TRUE when it matches.
   *
   * @var array
   */
  protected $conditions = [];

  /**
   * Whether closed follow-ups are left out.
   *
   * @var bool
   */
  protected $excludeResolved = FALSE;

  /**
   * Maximal number of results.
   *
   * @var int
   */
  protected $limit = 0;

  /**
   * Adds a condition on the bundle; the entity type is always node.
   */
  public function entityCondition($name, $value, $operator = NULL) {
    if ($name === 'bundle') {
      $bundles = is_array($value) ? $value : [$value];
      $this->conditions[] = function ($node) use ($bundles) {
        return in_array($node->type, $bundles);
      };
    }
    return $this;
  }

  /**
   * Adds a condition on a node property.
   */
  public function propertyCondition($name, $value, $operator = '=') {
    $this->conditions[] = function ($node) use ($name, $value, $operator) {
      return compare_values($node->{$name}, $value, $operator);
    };
    return $this;
  }

  /**
   * Adds a condition on a field; a node without the field never matches.
   */
  public function fieldCondition($field, $column, $value, $operator = NULL) {
    $this->conditions[] = function ($node) use ($field, $column, $value, $operator) {
      if (empty($node->{$field}[LANGUAGE_NONE][0][$column])) {
        return FALSE;
      }
      $operator = $operator ?: (is_array($value) ? 'IN' : '=');
      return compare_values($node->{$field}[LANGUAGE_NONE][0][$column], $value, $operator);
    };
    return $this;
  }

  /**
   * Honours the query tag that leaves out closed follow-ups.
   */
  public function addTag($tag) {
    if ($tag === 'exclude_resolved') {
      $this->excludeResolved = TRUE;
    }
    return $this;
  }

  /**
   * Limits the number of results.
   */
  public function range($start, $length) {
    $this->limit = $length;
    return $this;
  }

  /**
   * Returns the matching nodes, keyed by node ID under 'node'.
   */
  public function execute() {
    $matching = [];
    foreach ($GLOBALS['nodes'] as $nid => $node) {
      if ($this->excludeResolved && !empty($node->field_date_concluded)) {
        continue;
      }
      foreach ($this->conditions as $condition) {
        if (!$condition($node)) {
          continue 2;
        }
      }
      $matching[$nid] = $node;
    }
    if (empty($matching)) {
      return [];
    }
    return ['node' => array_slice($matching, 0, $this->limit, TRUE)];
  }

}

/**
 * The in-memory stand-in for a field reached through a wrapper.
 */
class HedleyCaseManagementFakeField {

  /**
   * The node holding the field.
   *
   * @var object
   */
  protected $node;

  /**
   * The field's name.
   *
   * @var string
   */
  protected $name;

  /**
   * Holds the node and the field name.
   */
  public function __construct($node, $name) {
    $this->node = $node;
    $this->name = $name;
  }

  /**
   * Returns the referenced node's ID.
   */
  public function getIdentifier() {
    return $this->node->{$this->name}[LANGUAGE_NONE][0]['target_id'] ?? NULL;
  }

  /**
   * Returns the field's value.
   */
  public function value() {
    return $this->node->{$this->name}[LANGUAGE_NONE][0]['value'] ?? NULL;
  }

  /**
   * Sets the field's value.
   */
  public function set($value) {
    $this->node->{$this->name}[LANGUAGE_NONE][0]['value'] = $value;
  }

  /**
   * Follows the reference to a field of the referenced node.
   */
  public function __get($name) {
    return new HedleyCaseManagementFakeField($GLOBALS['nodes'][$this->getIdentifier()], $name);
  }

}

/**
 * The in-memory stand-in for an entity metadata wrapper.
 */
class HedleyCaseManagementFakeWrapper {

  /**
   * The wrapped node.
   *
   * @var object
   */
  protected $node;

  /**
   * Holds the node.
   */
  public function __construct($node) {
    $this->node = $node;
  }

  /**
   * Returns a field of the node.
   */
  public function __get($name) {
    return new HedleyCaseManagementFakeField($this->node, $name);
  }

  /**
   * Nodes are already in memory, so there is nothing to save.
   */
  public function save() {
  }

}

/**
 * Compares a stored value with a query's value.
 */
function compare_values($stored, $value, $operator) {
  switch ($operator) {
    case 'IN':
      return in_array($stored, $value);

    case '<':
      return $stored < $value;

    case '<=':
      return $stored <= $value;

    default:
      return $stored == $value;
  }
}

/**
 * Stands in for the module's query factory.
 */
function hedley_general_create_entity_field_query_excluding_deleted() {
  return new HedleyCaseManagementFakeQuery();
}

/**
 * Stands in for the helper that cuts a stored date down to its day.
 */
function hedley_restful_timestamp_only_date($value) {
  return explode(' ', $value)[0];
}

/**
 * Stands in for the entity API's wrapper factory.
 */
function entity_metadata_wrapper($type, $node) {
  return new HedleyCaseManagementFakeWrapper(is_object($node) ? $node : $GLOBALS['nodes'][$node]);
}

/**
 * Stands in for the lookup of an illness's or pregnancy's encounters.
 */
function hedley_person_load_individual_participant_encounters_ids($participant) {
  $encounters = [];
  foreach ($GLOBALS['nodes'] as $nid => $node) {
    if (($node->field_individual_participant[LANGUAGE_NONE][0]['target_id'] ?? NULL) === $participant) {
      $encounters[] = $nid;
    }
  }
  return $encounters;
}

/**
 * Stores a node with the next node ID.
 *
 * @param string $type
 *   The bundle.
 * @param array $references
 *   The reference fields, field name to node ID.
 * @param array $values
 *   The value fields, field name to value.
 *
 * @return object
 *   The stored node.
 */
function store_node($type, array $references = [], array $values = []) {
  $node = new stdClass();
  $node->nid = ++$GLOBALS['last_nid'];
  $node->type = $type;
  $node->status = NODE_PUBLISHED;
  foreach ($references as $field => $target) {
    $node->{$field}[LANGUAGE_NONE][0]['target_id'] = $target;
  }
  foreach ($values as $field => $value) {
    $node->{$field}[LANGUAGE_NONE][0]['value'] = $value;
  }
  $GLOBALS['nodes'][$node->nid] = $node;
  return $node;
}

/**
 * Stores a node, and runs the module's reaction to its creation.
 */
function upload_node($type, array $references = [], array $values = []) {
  $node = store_node($type, $references, $values);
  hedley_case_management_node_insert($node);
  return $node;
}

/**
 * Whether a follow-up was closed.
 */
function is_closed($follow_up) {
  return !empty($follow_up->field_date_concluded[LANGUAGE_NONE][0]['value']);
}

/**
 * The date stored by the app for a day.
 */
function stored_date($day) {
  return $day . ' 00:00:00';
}

/**
 * Sets up a person with a participant and a first encounter of a list.
 *
 * @param array $list
 *   The list, from follow_up_lists().
 *
 * @return array
 *   Node IDs: person, participant and encounter.
 */
function set_up_patient(array $list) {
  $person = store_node('person');
  $participant = store_node('individual_participant', ['field_person' => $person->nid]);
  $encounter = store_node($list['encounter'], ['field_individual_participant' => $participant->nid], $list['encounter_values'] + ['field_scheduled_date' => stored_date('2026-01-01')]);

  if ($list['encounter'] === 'acute_illness_encounter') {
    // An illness with a single encounter has no follow-ups to close.
    store_node($list['encounter'], ['field_individual_participant' => $participant->nid], ['field_scheduled_date' => stored_date('2026-01-02')]);
  }

  return [
    'person' => $person->nid,
    'participant' => $participant->nid,
    'encounter' => $encounter->nid,
  ];
}

/**
 * Uploads a follow-up, dated on a day.
 *
 * @param string $bundle
 *   The follow-up bundle.
 * @param array $patient
 *   The patient, from set_up_patient().
 * @param string $day
 *   The follow-up's date.
 *
 * @return object
 *   The follow-up node.
 */
function upload_follow_up($bundle, array $patient, $day) {
  $references = ['field_person' => $patient['person']];
  $values = ['field_date_measured' => stored_date($day)];
  if ($bundle === 'acute_illness_follow_up') {
    $references['field_acute_illness_encounter'] = $patient['encounter'];
    $values['field_acute_illness_diagnosis'] = 'pneumonia';
  }
  return upload_node($bundle, $references, $values);
}

/**
 * Uploads an encounter of a list, dated on a day.
 *
 * @param array $list
 *   The list, from follow_up_lists().
 * @param array $patient
 *   The patient, from set_up_patient().
 * @param string $day
 *   The encounter's date.
 *
 * @return object
 *   The encounter node.
 */
function upload_encounter(array $list, array $patient, $day) {
  return upload_node($list['encounter'], ['field_individual_participant' => $patient['participant']], $list['encounter_values'] + ['field_scheduled_date' => stored_date($day)]);
}

/**
 * The six Case Management lists.
 *
 * @return array
 *   Per list: the bundles of its follow-ups, and the encounter that closes
 *   them. The Immunization list has no follow-up that closes others.
 */
function follow_up_lists() {
  return [
    'Child Nutrition' => [
      'bundles' => ['follow_up', 'nutrition_follow_up', 'well_child_follow_up'],
      'encounter' => 'home_visit_encounter',
      'encounter_values' => [],
      'closing_follow_up' => TRUE,
    ],
    'Immunization' => [
      'bundles' => ['well_child_next_visit'],
      'encounter' => 'well_child_encounter',
      'encounter_values' => ['field_well_child_encounter_type' => 'pediatric-care'],
      'closing_follow_up' => FALSE,
    ],
    'Acute Illness' => [
      'bundles' => ['acute_illness_follow_up'],
      'encounter' => 'acute_illness_encounter',
      'encounter_values' => [],
      'closing_follow_up' => TRUE,
    ],
    'Antenatal' => [
      'bundles' => ['prenatal_follow_up'],
      'encounter' => 'prenatal_encounter',
      'encounter_values' => ['field_prenatal_encounter_type' => 'chw-1'],
      'closing_follow_up' => TRUE,
    ],
    'Tuberculosis' => [
      'bundles' => ['tuberculosis_follow_up'],
      'encounter' => 'tuberculosis_encounter',
      'encounter_values' => [],
      'closing_follow_up' => TRUE,
    ],
    'HIV' => [
      'bundles' => ['hiv_follow_up'],
      'encounter' => 'hiv_encounter',
      'encounter_values' => [],
      'closing_follow_up' => TRUE,
    ],
  ];
}

$GLOBALS['nodes'] = [];
$GLOBALS['last_nid'] = 0;

$passed = 0;
$failed = 0;

$newer = '2026-03-10';
$older = '2026-02-01';
$later = '2026-04-20';

$lists = follow_up_lists();
foreach ($lists as $name => $list) {
  foreach ($list['bundles'] as $bundle) {
    $label = "$name ($bundle)";

    // A closing record can be an encounter or, on most lists, a follow-up.
    $closers = [
      'encounter' => function ($patient, $day) use ($list) {
        return upload_encounter($list, $patient, $day);
      },
    ];
    if ($list['closing_follow_up']) {
      $closers['follow-up'] = function ($patient, $day) use ($bundle) {
        return upload_follow_up($bundle, $patient, $day);
      };
    }

    foreach ($closers as $kind => $upload_closer) {
      $patient = set_up_patient($list);
      $follow_up = upload_follow_up($bundle, $patient, $newer);
      $upload_closer($patient, $older);
      assert_same(is_closed($follow_up), FALSE, "$label: a late $kind dated earlier leaves the newer follow-up open", $passed, $failed);

      $patient = set_up_patient($list);
      $follow_up = upload_follow_up($bundle, $patient, $newer);
      $upload_closer($patient, $newer);
      assert_same(is_closed($follow_up), TRUE, "$label: a $kind dated the same day closes the follow-up", $passed, $failed);

      $patient = set_up_patient($list);
      $follow_up = upload_follow_up($bundle, $patient, $newer);
      $upload_closer($patient, $later);
      assert_same(is_closed($follow_up), TRUE, "$label: a $kind dated later closes the follow-up", $passed, $failed);

      $patient = set_up_patient($list);
      $upload_closer($patient, $older);
      $follow_up = upload_follow_up($bundle, $patient, $newer);
      assert_same(is_closed($follow_up), FALSE, "$label: a follow-up reaching the server after the $kind stays open", $passed, $failed);

      $patient = set_up_patient($list);
      $upload_closer($patient, $newer);
      $follow_up = upload_follow_up($bundle, $patient, $older);
      assert_same(is_closed($follow_up), FALSE, "$label: a follow-up dated earlier, reaching the server after the $kind, stays open", $passed, $failed);

      // The late record and its own follow-up arrive after the newer one.
      $patient = set_up_patient($list);
      $newer_follow_up = upload_follow_up($bundle, $patient, $newer);
      $upload_closer($patient, $older);
      $late_follow_up = upload_follow_up($bundle, $patient, $older);
      $upload_closer($patient, $later);
      assert_same(is_closed($newer_follow_up), TRUE, "$label: a later $kind closes the newer follow-up", $passed, $failed);
      assert_same(is_closed($late_follow_up), TRUE, "$label: a later $kind closes the follow-up of the late upload", $passed, $failed);
    }
  }
}

// Devices upload encounters with a date and no time.
$list = $lists['Immunization'];
$patient = set_up_patient($list);
$follow_up = upload_follow_up('well_child_next_visit', $patient, $newer);
upload_node($list['encounter'], ['field_individual_participant' => $patient['participant']], $list['encounter_values'] + ['field_scheduled_date' => $older]);
assert_same(is_closed($follow_up), FALSE, 'A late encounter uploaded with no time leaves the newer follow-up open', $passed, $failed);
upload_node($list['encounter'], ['field_individual_participant' => $patient['participant']], $list['encounter_values'] + ['field_scheduled_date' => $newer]);
assert_same(is_closed($follow_up), TRUE, 'An encounter uploaded with no time, dated the same day, closes the follow-up', $passed, $failed);

// Another person's follow-up is never closed.
$list = $lists['Immunization'];
$patient = set_up_patient($list);
$other = set_up_patient($list);
$follow_up = upload_follow_up('well_child_next_visit', $other, $newer);
upload_encounter($list, $patient, $later);
assert_same(is_closed($follow_up), FALSE, "Another person's follow-up stays open", $passed, $failed);

// Acute Illness: only the same illness's follow-ups close.
$list = $lists['Acute Illness'];
$patient = set_up_patient($list);
$other_illness = store_node('individual_participant', ['field_person' => $patient['person']]);
$other_encounter = store_node('acute_illness_encounter', ['field_individual_participant' => $other_illness->nid], ['field_scheduled_date' => stored_date('2026-01-01')]);
$follow_up = upload_follow_up('acute_illness_follow_up', ['encounter' => $other_encounter->nid] + $patient, $newer);
upload_encounter($list, $patient, $later);
assert_same(is_closed($follow_up), FALSE, 'A later encounter of another illness leaves the follow-up open', $passed, $failed);

// An Acute Illness follow-up of suspected Tuberculosis is on the TB list.
$tb_list = $lists['Tuberculosis'];
$tb_cases = [
  [$older, FALSE, 'dated earlier leaves it open'],
  [$later, TRUE, 'dated later closes it'],
];
foreach ($tb_cases as [$day, $closed, $outcome]) {
  $patient = set_up_patient($tb_list);
  $illness = set_up_patient($list);
  $follow_up = upload_node('acute_illness_follow_up', [
    'field_person' => $patient['person'],
    'field_acute_illness_encounter' => $illness['encounter'],
  ], [
    'field_date_measured' => stored_date($newer),
    'field_acute_illness_diagnosis' => 'tuberculosis-suspect',
  ]);
  upload_encounter($tb_list, $patient, $day);
  assert_same(is_closed($follow_up), $closed, "A Tuberculosis encounter $outcome: suspected Tuberculosis follow-up", $passed, $failed);
}

// Encounters that close nothing.
$list = $lists['Antenatal'];
$patient = set_up_patient($list);
$follow_up = upload_follow_up('prenatal_follow_up', $patient, $newer);
upload_node('prenatal_encounter', ['field_individual_participant' => $patient['participant']], [
  'field_prenatal_encounter_type' => 'nurse',
  'field_scheduled_date' => stored_date($later),
]);
assert_same(is_closed($follow_up), FALSE, 'A nurse antenatal encounter closes nothing', $passed, $failed);

$list = $lists['Immunization'];
$patient = set_up_patient($list);
$follow_up = upload_follow_up('well_child_next_visit', $patient, $newer);
upload_node('well_child_encounter', ['field_individual_participant' => $patient['participant']], [
  'field_well_child_encounter_type' => 'newborn-exam',
  'field_scheduled_date' => stored_date($later),
]);
assert_same(is_closed($follow_up), FALSE, 'A newborn exam closes nothing', $passed, $failed);

// A follow-up already closed stays closed, with its closing date.
$patient = set_up_patient($list);
$follow_up = upload_follow_up('well_child_next_visit', $patient, $newer);
$follow_up->field_date_concluded[LANGUAGE_NONE][0]['value'] = 'closed earlier';
upload_encounter($list, $patient, $older);
upload_encounter($list, $patient, $later);
assert_same($follow_up->field_date_concluded[LANGUAGE_NONE][0]['value'], 'closed earlier', 'A follow-up already closed stays closed, with its closing date', $passed, $failed);

// Recording an outcome closes all follow-ups, whatever their dates.
$outcomes = [
  'acute-illness' => ['Acute Illness', 'acute_illness_follow_up'],
  'antenatal' => ['Antenatal', 'prenatal_follow_up'],
  'tuberculosis' => ['Tuberculosis', 'tuberculosis_follow_up'],
  'hiv' => ['HIV', 'hiv_follow_up'],
];
foreach ($outcomes as $encounter_type => [$name, $bundle]) {
  $list = $lists[$name];
  $patient = set_up_patient($list);
  $follow_up = upload_follow_up($bundle, $patient, '2027-01-01');

  $participant = $GLOBALS['nodes'][$patient['participant']];
  $participant->field_encounter_type[LANGUAGE_NONE][0]['value'] = $encounter_type;
  $participant->original = clone $participant;
  $participant->field_outcome[LANGUAGE_NONE][0]['value'] = 'concluded';
  hedley_case_management_node_update($participant);

  assert_same(is_closed($follow_up), TRUE, "$name: recording an outcome closes the follow-up, whatever its date", $passed, $failed);
}

report_results('Follow-up resolution', $passed, $failed);
