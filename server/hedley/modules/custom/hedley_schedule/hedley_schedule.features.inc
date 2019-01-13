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
    'catchment_area' => array(
      'name' => t('Catchment Area'),
      'base' => 'node_content',
      'description' => t('A catchment area represents a group of health centers.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'clinic' => array(
      'name' => t('Clinic'),
      'base' => 'node_content',
      'description' => t('A Clinic represents a group of mothers and babies for which group post-natal sessions are scheduled.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'health_center' => array(
      'name' => t('Health Center'),
      'base' => 'node_content',
      'description' => t('A location at which health services are provided. Each health centre has multiple "clinics" (groups of mothers and babies for post-natal care). Each health centre falls within a "catchment area" (a group of health centers).'),
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
      'help' => t('In order to schedule data entry for a clinic,  create a new <em>Session</em>, assign the relevant <em>Clinic</em>, and specify the <em>Scheduled Date</em> for the measurements. This needs to be done in advance, using the Drupal admin UI. It will then show up in the client app as being available for data entry, until the <em>Close On</em> date.'),
    ),
  );
  drupal_alter('node_info', $items);
  return $items;
}
