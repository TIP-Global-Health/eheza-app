<?php

/**
 * @file
 * hedley_person.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function hedley_person_ctools_plugin_api($module = NULL, $api = NULL) {
  if ($module == "strongarm" && $api == "strongarm") {
    return array("version" => "1");
  }
}

/**
 * Implements hook_image_default_styles().
 */
function hedley_person_image_default_styles() {
  $styles = array();

  // Exported image style: patient-photo.
  $styles['patient-photo'] = array(
    'label' => 'Patient photo',
    'effects' => array(
      1 => array(
        'name' => 'image_scale_and_crop',
        'data' => array(
          'width' => 200,
          'height' => 200,
        ),
        'weight' => 1,
      ),
    ),
  );

  // Exported image style: person-photo.
  $styles['person-photo'] = array(
    'label' => 'Person photo',
    'effects' => array(
      1 => array(
        'name' => 'image_scale_and_crop',
        'data' => array(
          'width' => 200,
          'height' => 200,
        ),
        'weight' => 1,
      ),
    ),
  );

  return $styles;
}

/**
 * Implements hook_node_info().
 */
function hedley_person_node_info() {
  $items = array(
    'person' => array(
      'name' => t('Person'),
      'base' => 'node_content',
      'description' => t('Basic information about participants and family members.'),
      'has_title' => '1',
      'title_label' => t('Name'),
      'help' => '',
    ),
    'relationship' => array(
      'name' => t('Relationship'),
      'base' => 'node_content',
      'description' => t('Records a relationship between two persons.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
  );
  drupal_alter('node_info', $items);
  return $items;
}
