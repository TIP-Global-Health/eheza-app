<?php

/**
 * @file
 * hedley_schedule.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function hedley_schedule_ctools_plugin_api($module = NULL, $api = NULL) {
  if ($module == "strongarm" && $api == "strongarm") {
    return array("version" => "1");
  }
}

/**
 * Implements hook_node_info().
 */
function hedley_schedule_node_info() {
  $items = array(
    'acute_illness_encounter' => array(
      'name' => t('Acute Illness Encounter'),
      'base' => 'node_content',
      'description' => t('Tracks a visit by a person in which acute illness activities are performed.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'clinic' => array(
      'name' => t('Group'),
      'base' => 'node_content',
      'description' => t('A Group is a group of mothers and babies who are seen together for group health assessments.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'counseling_schedule' => array(
      'name' => t('Counseling Schedule'),
      'base' => 'node_content',
      'description' => t('Groups together, in order, several "Counseling Topics" which ought to be addressed at a certain point.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'counseling_session' => array(
      'name' => t('Counseling Session'),
      'base' => 'node_content',
      'description' => t('This new entity will analogue to the other "measurement"/examination entities (muac, photo, height, weight etc.)'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'counseling_topic' => array(
      'name' => t('Counseling Topic'),
      'base' => 'node_content',
      'description' => t('Counseling topics for the Counseling session'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'home_visit_encounter' => array(
      'name' => t('Home Visit Encounter'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'individual_participant' => array(
      'name' => t('Individual Participant'),
      'base' => 'node_content',
      'description' => t('Identifies a person as a participant in individual encounter activities, according to type of individual encounter.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'ncd_encounter' => array(
      'name' => t('NCD Encounter'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'nutrition_encounter' => array(
      'name' => t('Nutrition Encounter'),
      'base' => 'node_content',
      'description' => t('Tracks a visit by a child in which nutrition activities are performed.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'pmtct_participant' => array(
      'name' => t('Group Participant'),
      'base' => 'node_content',
      'description' => t('Identifies a child participating in a Group, along with an adult participating in mother activities or caregiver activities.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'prenatal_encounter' => array(
      'name' => t('Prenatal Encounter'),
      'base' => 'node_content',
      'description' => t('Tracks a visit by a mother in which prenatal activities are performed.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'session' => array(
      'name' => t('Group Encounter'),
      'base' => 'node_content',
      'description' => t('A Group Encounter represents a date on which a health assessment is scheduled for a Group. It ties together the various measurements taken at that group encounter.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => t('In order to schedule a group encounter,  create a new <em>Group Encounter</em>, assign the relevant <em>Group</em>, and specify the <em>Scheduled Date</em> for the measurements. This needs to be done in advance, using the Drupal admin UI. It will then show up in the client app as being available for data entry, until the <em>Close On</em> date.'),
    ),
  );
  drupal_alter('node_info', $items);
  return $items;
}
