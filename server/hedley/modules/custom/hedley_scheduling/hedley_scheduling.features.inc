<?php
/**
 * @file
 * hedley_scheduling.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function hedley_scheduling_ctools_plugin_api($module = NULL, $api = NULL) {
  if ($module == "strongarm" && $api == "strongarm") {
    return array("version" => "1");
  }
}

/**
 * Implements hook_node_info().
 */
function hedley_scheduling_node_info() {
  $items = array(
    'clinic' => array(
      'name' => t('Clinic'),
      'base' => 'node_content',
      'description' => t('A Clinic represents a location at which data-entry sessions can be scheduled.

Each mother is assigned to a clinic.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'session' => array(
      'name' => t('Session'),
      'base' => 'node_content',
      'description' => t('A Session represents a date on which data-entry is scheduled for a clinic.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => t('In order to schedule data entry for a clinic,  create a new `Session`, assign the relevant `Clinic`, and specify the `Scheduled Date` for the measurements. This needs to be done in advance, using the Drupal admin UI. It will then show up in the client app as being available for data entry, until the `Close On` date.'),
    ),
  );
  drupal_alter('node_info', $items);
  return $items;
}
