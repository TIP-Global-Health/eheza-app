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

$faker = hedley_faker_create();

// For sample db: Rukura, Rwankuba, Test.
$health_centers_ids = [7091, 7092, 28589];

$catchment_areas = [
  [
    'id',
    'title',
  ],
];
$health_centers = [
  [
    'id',
    'title',
    'field_catchment_area',
  ],
];
$villages = [
  [
    'id',
    'field_province',
    'field_district',
    'field_sector',
    'field_cell',
    'field_village',
    'field_health_center',
  ],
];
$groups = [
  [
    'id',
    'title',
    'field_group_type',
    'field_health_center',
  ],
];
$nurses = [
  [
    'id',
    'title',
    'field_role',
    'field_health_centers',
    'field_pin_code',
  ],
];
$group_encounters = [
  [
    'id',
    'field_clinic',
    'field_scheduled_date',
  ],
];
$participants = [
  [
    'id',
    'field_person',
    'field_adult',
    'field_adult_activities',
    'field_expected',
    'field_clinic',
  ],
];
$people = [
  [
    'id',
    'title',
    'field_first_name',
    'field_second_name',
    'field_gender',
    'field_birth_date',
    'field_health_center',
    'field_birth_date_estimated',
    'field_hmis_number',
    'field_marital_status',
    'field_education_level',
    'field_ubudehe',
    'field_province',
    'field_district',
    'field_sector',
    'field_cell',
    'field_village',
    'field_hiv_status',
    'field_mode_of_delivery',
    'field_number_of_children',
    'field_photo',
    'field_national_id_number',
    'field_phone_number',
  ],
];
$relationships = [
  [
    'id',
    'field_person',
    'field_related_by',
    'field_related_to',
  ],
];
$measurements_fields = [
  'id',
  'field_person',
  'field_date_measured',
  'field_nurse',
  'field_session',
];
$measurements = [
  'attendance' => [array_merge($measurements_fields, ['field_attended'])],
  'family_planning' => [
    array_merge(
      $measurements_fields,
      ['field_family_planning_signs']
    ),
  ],
  'height' => [
    array_merge(
      $measurements_fields,
      ['field_height', 'field_zscore_age']
    ),
  ],
  'weight' => [
    array_merge(
      $measurements_fields,
      [
        'field_weight',
        'field_bmi',
        'field_zscore_age',
        'field_zscore_length',
        'field_zscore_bmi',
      ]
    ),
  ],
  'muac' => [array_merge($measurements_fields, ['field_muac'])],
  'nutrition' => [array_merge($measurements_fields, ['field_nutrition_signs'])],
  'photo' => [array_merge($measurements_fields, ['field_photo'])],
];

$catchment_area_ids = [];
foreach ($health_centers_ids as $health_center_id) {
  $node = node_load($health_center_id);

  if (!$node) {
    drush_print("Error: can't find health center with ID $health_center_id!");
    exit;
  }

  $wrapper = entity_metadata_wrapper('node', $node);

  $catchment_area_id = $wrapper->field_catchment_area->getIdentifier();

  $health_centers[] = [
    $wrapper->getIdentifier(),
    str_replace(',', ' ', $wrapper->label()),
    $catchment_area_id,
  ];

  if (!in_array($catchment_area_id, $catchment_area_ids)) {
    $wrapper = entity_metadata_wrapper('node', $catchment_area_id);
    $catchment_areas[] = [
      $wrapper->getIdentifier(),
      str_replace(',', ' ', $wrapper->label()),
    ];
    $catchment_area_ids[] = $catchment_area_id;
  }

  $villages_ids = hedley_migrate_resolve_for_export('village', 'field_health_center', [$health_center_id]);
  foreach ($villages_ids as $village_id) {
    $wrapper = entity_metadata_wrapper('node', $village_id);
    $villages[] = [
      $wrapper->getIdentifier(),
      $wrapper->field_province->value(),
      $wrapper->field_district->value(),
      $wrapper->field_sector->value(),
      $wrapper->field_cell->value(),
      $wrapper->field_village->value(),
      $wrapper->field_health_center->getIdentifier(),
    ];
  }

  $groups_ids = hedley_migrate_resolve_for_export('clinic', 'field_health_center', [$health_center_id]);
  foreach ($groups_ids as $group_id) {
    $wrapper = entity_metadata_wrapper('node', $group_id);
    $groups[] = [
      $wrapper->getIdentifier(),
      str_replace(',', ' ', $wrapper->label()),
      $wrapper->field_group_type->value(),
      $wrapper->field_health_center->getIdentifier(),
    ];
  }

  $nurses_ids = hedley_migrate_resolve_for_export('nurse', 'field_health_centers', [$health_center_id]);
  foreach ($nurses_ids as $nurse_id) {
    $wrapper = entity_metadata_wrapper('node', $nurse_id);

    $hc_ids = $wrapper->field_health_centers->value(['identifier' => TRUE]);
    foreach ($hc_ids as $key => $hc_id) {
      if (!in_array($hc_id, $health_centers_ids)) {
        unset($hc_ids[$key]);
      }
    }

    $nurses[$nurse_id] = [
      $nurse_id,
      str_replace(',', ' ', $wrapper->label()),
      implode('|', $wrapper->field_role->value()),
      implode('|', array_values($hc_ids)),
      $wrapper->field_pin_code->value(),
    ];
  }

  $group_encounters_ids = hedley_migrate_resolve_for_export('session', 'field_clinic', $groups_ids);
  foreach ($group_encounters_ids as $group_encounter_id) {
    $wrapper = entity_metadata_wrapper('node', $group_encounter_id);
    $group_encounters[] = [
      $wrapper->getIdentifier(),
      $wrapper->field_clinic->getIdentifier(),
      hedley_migrate_export_date_field($wrapper->field_scheduled_date->value(), TRUE),
    ];
  }

  $participants_ids = hedley_migrate_resolve_for_export('pmtct_participant', 'field_clinic', $groups_ids);
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
    $gender = $wrapper->field_gender->value();
    $first_name = $gender == 'male' ? $faker->firstNameMale : $faker->firstNameFemale;
    $second_name = $faker->lastName;
    $photo = rand(1, 5) . ".jpg";
    $national_id = '1199270' . $faker->numberBetween(100000000, 199999999);
    $phone_number = '0' . $faker->numberBetween(700000000, 799999999);

    $people[$person_id] = [
      $person_id,
      "$second_name $first_name",
      $first_name,
      $second_name,
      $gender,
      date('Y-m-d', $wrapper->field_birth_date->value()),
      $wrapper->field_health_center->getIdentifier(),
      $wrapper->field_birth_date_estimated->value(),
      $wrapper->field_hmis_number->value(),
      $wrapper->field_marital_status->value(),
      $wrapper->field_education_level->value(),
      $wrapper->field_ubudehe->value(),
      $wrapper->field_province->value(),
      $wrapper->field_district->value(),
      $wrapper->field_sector->value(),
      $wrapper->field_cell->value(),
      $wrapper->field_village->value(),
      $wrapper->field_hiv_status->value(),
      $wrapper->field_mode_of_delivery->value(),
      $wrapper->field_number_of_children->value(),
      $photo,
      $national_id,
      $phone_number,
    ];
  }

  $relationships_ids = hedley_migrate_resolve_for_export('relationship', 'field_person', $unique_mothers_ids);
  foreach ($relationships_ids as $relationship_id) {
    $wrapper = entity_metadata_wrapper('node', $relationship_id);

    $relationships[$relationship_id] = [
      $relationship_id,
      $wrapper->field_person->getIdentifier(),
      $wrapper->field_related_by->value(),
      $wrapper->field_related_to->getIdentifier(),
    ];
  }

  foreach ($measurements as $type => $values) {
    $ids = hedley_migrate_resolve_for_export($type, 'field_session', $group_encounters_ids);

    foreach ($ids as $id) {
      $wrapper = entity_metadata_wrapper('node', $id);
      $common_values = [
        $wrapper->getIdentifier(),
        $wrapper->field_person->getIdentifier(),
        date('Y-m-d', $wrapper->field_date_measured->value()),
        $wrapper->field_nurse->getIdentifier(),
        $wrapper->field_session->getIdentifier(),
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

        case 'nutrition':
          $type_based_values = [
            implode('|', $wrapper->field_nutrition_signs->value()),
          ];
          break;

        case 'photo':
          $id = rand(1, 5);
          $type_based_values = ["$id.jpg"];
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
  'village' => $villages,
  'clinic' => $groups,
  'nurse' => array_values($nurses),
  'session' => $group_encounters,
  'pmtct_participant' => $participants,
  'person' => array_values($people),
  'relationship' => array_values($relationships),
  'attendance' => $measurements['attendance'],
  'family_planning' => $measurements['family_planning'],
  'height' => $measurements['height'],
  'weight' => $measurements['weight'],
  'muac' => $measurements['muac'],
  'nutrition' => $measurements['nutrition'],
  'photo' => $measurements['photo'],
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
