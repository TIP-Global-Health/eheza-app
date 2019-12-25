<?php

/**
 * @file
 * Populate the type for all clinics.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_migrate/scripts/export-content.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

drush_print('Starting export!');


$health_centers_ids = [
//    7091, // Rukura
//  7092, // Rwankuba
    28589, // Test
];

$catchment_areas = [[
  'id',
  'title',
]];
$health_centers = [[
  'id',
  'title',
  'field_catchment_area',
]];
$groups = [[
  'id',
  'title',
  'field_group_type',
  'field_health_center',
]];
$nurses = [[
  'id',
  'title',
  'field_role',
  'field_health_centers',
  'field_pin_code',
]];
$group_encounters = [[
  'id',
  'field_clinic',
  'field_scheduled_date',
]];
$participants = [[
  'id',
  'field_person',
  'field_adult',
  'field_adult_activities',
  'field_expected',
  'field_clinic',
  ]];
$people = [[
  'id',
  'title',
  'field_first_name',
  'field_second_name',
  'field_gender',
  'field_birth_date',
  'field_health_center',
  ]];
$relationships = [[
  'id',
  'field_person',
  'field_related_by',
  'field_related_to',
  ]];
$measurements_fields = [
  'id',
  'field_person',
  'field_date_measured',
  'field_nurse',
  'field_session',
  'field_shards',
];
$measurements = [
  'attendance' => [array_merge(
    $measurements_fields, ['field_attended']
  )],
  'family_planning' => [array_merge(
    $measurements_fields, ['field_family_planning_signs']
  )],
  'height' => [array_merge(
    $measurements_fields, ['field_height', 'field_zscore_age']
  )],
  'weight' => [array_merge(
    $measurements_fields, [
      'field_weight',
      'field_bmi',
      'field_zscore_age',
      'field_zscore_length',
      'field_zscore_bmi',
    ]
  )],
  'muac' => [array_merge(
    $measurements_fields, ['field_muac']
  )],
  'nutrition' => [array_merge(
    $measurements_fields, ['field_nutrition_signs']
  )],
  //Todo: 'photo',
];

$catchment_area_ids = [];
foreach ($health_centers_ids as $health_center_id) {
  $wrapper = entity_metadata_wrapper('node', $health_center_id);
  $health_centers[] = [
    $wrapper->getIdentifier(),
    $wrapper->label(),
    $wrapper->field_catchment_area->getIdentifier(),
  ];

  $catchment_area_id = $wrapper->	field_catchment_area->value(['identifier' => TRUE]);
  if (!in_array($catchment_area_id, $catchment_area_ids)) {
    $wrapper = entity_metadata_wrapper('node', $catchment_area_id);
    $catchment_areas[] = [
      $wrapper->getIdentifier(),
      $wrapper->label(),
    ];
    $catchment_area_ids[] = $catchment_area_id;
  }

  $groups_ids = hedley_migrate_get_xx_for_yy('clinic', 'field_health_center', [$health_center_id]);
  foreach ($groups_ids as $group_id) {
    $wrapper = entity_metadata_wrapper('node', $group_id);
    $groups[] = [
      $wrapper->getIdentifier(),
      $wrapper->label(),
      $wrapper->field_group_type->value(),
      $wrapper->field_health_center->getIdentifier(),
    ];
  }

  $nurses_ids = hedley_migrate_get_xx_for_yy('nurse', 'field_health_centers', [$health_center_id]);
  foreach ($nurses_ids as $nurse_id) {
    $wrapper = entity_metadata_wrapper('node', $nurse_id);
    $nurses[] = [
      $wrapper->getIdentifier(),
      $wrapper->label(),
      implode('|', $wrapper->field_role->value()),
      implode('|', $wrapper->field_health_centers->value(['identifier' => TRUE])),
      $wrapper->field_pin_code->value(),
    ];
  }

  $group_encounters_ids = hedley_migrate_get_xx_for_yy('session', 'field_clinic', $groups_ids);
  foreach ($group_encounters_ids as $group_encounter_id) {
    $wrapper = entity_metadata_wrapper('node', $group_encounter_id);
    $group_encounters[] = [
      $wrapper->getIdentifier(),
      $wrapper->field_clinic->getIdentifier(),
      hedley_migrate_export_date_field($wrapper->field_scheduled_date->value()),
    ];
  }

  $participants_ids = hedley_migrate_get_xx_for_yy('pmtct_participant', 'field_clinic', $groups_ids);
  $mothers_ids = $children_ids = [];
  foreach ($participants_ids as $participant_id) {
    $wrapper = entity_metadata_wrapper('node', $participant_id);
    $mothers_ids[] = $wrapper->field_adult->getIdentifier();
    $children_ids[] = $wrapper->field_person->getIdentifier();

    $participants[] = [
      $wrapper->getIdentifier(),
      $wrapper->field_person->getIdentifier(),
      $wrapper->field_adult->getIdentifier(),
      $wrapper->field_adult_activities->value(),
      hedley_migrate_export_date_field($wrapper->field_expected->value()),
      $wrapper->field_clinic->getIdentifier(),
    ];
  }

  $unique_mothers_ids = array_unique($mothers_ids);
  $people_ids = array_merge($unique_mothers_ids, array_unique($children_ids));
  foreach ($people_ids as $person_id) {
    $wrapper = entity_metadata_wrapper('node', $person_id);
    $people[] = [
      $wrapper->getIdentifier(),
      $wrapper->label(),
      $wrapper->field_first_name->value(),
      $wrapper->field_second_name->value(),
      $wrapper->field_gender->value(),
      date('Y-m-d', $wrapper->field_birth_date->value()),
      $wrapper->field_health_center->getIdentifier(),
    ];
  }

  $relationships_ids = hedley_migrate_get_xx_for_yy('relationship', 'field_person', $unique_mothers_ids);
  foreach ($relationships_ids as $relationship_id) {
    $wrapper = entity_metadata_wrapper('node', $relationship_id);
    $relationships[] = [
      $wrapper->getIdentifier(),
      $wrapper->field_person->getIdentifier(),
      $wrapper->field_related_by->value(),
      $wrapper->field_related_to->getIdentifier(),
    ];
  }

  foreach ($measurements as $type => $values) {
    $ids = hedley_migrate_get_xx_for_yy($type, 'field_session', $group_encounters_ids);

    foreach ($ids as $id) {
      $wrapper = entity_metadata_wrapper('node', $id);
      $common_values = [
        $wrapper->getIdentifier(),
        $wrapper->field_person->getIdentifier(),
        date('Y-m-d', $wrapper->field_date_measured->value()),
        $wrapper->field_nurse->getIdentifier(),
        $wrapper->field_session->getIdentifier(),
        implode('|', $wrapper->field_shards->value(['identifier' => TRUE])),
      ];

      switch ($type) {
        case 'attendance':
          $type_based_values = [$wrapper->field_attended->value()];
          break;

        case 'family_planning':
          $type_based_values = [
            implode('|', $wrapper->field_family_planning_signs->value()),
          ];
          break;

        case 'height':
          $type_based_values = [
            $wrapper->field_height->value(),
            $wrapper->field_zscore_age->value(),
          ];
          break;

        case 'weight':
          $type_based_values = [
            $wrapper->field_weight->value(),
            $wrapper->field_bmi->value(),
            $wrapper->field_zscore_age->value(),
            $wrapper->field_zscore_length->value(),
            $wrapper->field_zscore_bmi->value(),
          ];
          break;

        case 'muac':
          $type_based_values = [$wrapper->field_muac->value()];
          break;

        case 'field_nutrition_signs':
          $type_based_values = [
            implode('|', $wrapper->field_nutrition_signs->value()),
          ];
          break;

        default:
          $type_based_values = [];
      }

      $measurements[$type][] = array_merge($common_values, $type_based_values);
    }
  }
}

$mapping = [
  'catchment_area' => $catchment_areas,
  'health_center' => $health_centers,
  'clinic' => $groups,
  'nurse' => $nurses,
  'session' => $group_encounters,
  'pmtct_participant' => $participants,
  'person' => $people,
  'relationship' => $relationships,
  'attendance' => $measurements['attendance'],
  'family_planning' => $measurements['family_planning'],
  'height' => $measurements['height'],
  'weight' => $measurements['weight'],
  'muac' => $measurements['muac'],
  'nutrition' => $measurements['nutrition'],
  //Todo: 'photo',
];

foreach ($mapping as $name => $rows) {
  $content = [];
  foreach ($rows as $row) {
    $content[] = implode(',', $row);
  }

  $path = drupal_get_path('module', 'hedley_migrate') . '/csv';
  $fp = fopen("$path/$name.csv", 'w');
  fwrite($fp, implode(PHP_EOL, $content));

  fclose($fp);
}


drush_print('Done!');
