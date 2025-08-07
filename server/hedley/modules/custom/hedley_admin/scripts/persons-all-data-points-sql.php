<?php

/**
 * @file
 * Records data points of Person.
 *
 * Execution:  drush scr
 *   profiles/hedley/modules/custom/hedley_admin/scripts/persons-all-data-points-sql.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Get the last node id.
$nid = drush_get_option('nid', 0);

// Get the number of nodes to be processed.
$batch = drush_get_option('batch', 50);

// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 800);

$type = 'person';

$labels = [
  'ID' => 'INTEGER PRIMARY KEY',
  'Birth Date' => 'TIMESTAMP NOT NULL',
  'Gender' => 'TEXT NOT NULL',
  'Education Level' => 'TEXT NOT NULL',
  'Province' => 'TEXT NOT NULL',
  'District' => 'TEXT NOT NULL',
  'Sector' => 'TEXT NOT NULL',
  'Cell' => 'TEXT NOT NULL',
  'Village' => 'TEXT NOT NULL',
];

drush_print("CREATE TABLE public.persons (");
$columns = [];
$last_key = array_key_last($labels);
foreach ($labels as $label => $column_type) {
  $column_name = strtolower(str_replace(' ', '_', $label));
  $columns[] = $column_name;
  $columns_definition = "  $column_name $column_type";
  if ($last_key !== $label) {
    $columns_definition .= ',';
  }
  drush_print($columns_definition);
}
drush_print(");");

$base_query = new EntityFieldQuery();
$base_query
  ->entityCondition('entity_type', 'node')
  ->propertyCondition('type', $type)
  ->propertyOrderBy('nid', 'DESC')
  ->addTag('exclude_deleted');

$count_query = clone $base_query;
$count_query->propertyCondition('nid', $nid, '>');
$count = $count_query->count()->execute();

if ($count == 0) {
  drush_print("There are no nodes of type $type in DB.");
  exit;
}

while (TRUE) {
  $query = clone $base_query;
  if ($nid) {
    $query->propertyCondition('nid', $nid, '<');
  }

  $result = $query
    ->range(0, $batch)
    ->execute();

  if (empty($result['node'])) {
    // No more items left.
    break;
  }

  $ids = array_keys($result['node']);
  $nid = end($ids);
  $persons = node_load_multiple($ids);
  foreach ($persons as $person) {
    $person_id = $person->nid;
    $birth_date = $person->field_birth_date[LANGUAGE_NONE][0]['value'];
    $gender = $person->field_gender[LANGUAGE_NONE][0]['value'];
    $education_level = $person->field_education_level[LANGUAGE_NONE][0]['value'];
    $province = $person->field_province[LANGUAGE_NONE][0]['value'];
    $district = $person->field_district[LANGUAGE_NONE][0]['value'];
    $sector = $person->field_sector[LANGUAGE_NONE][0]['value'];
    $cell = $person->field_cell[LANGUAGE_NONE][0]['value'];
    $village = $person->field_village[LANGUAGE_NONE][0]['value'];

    $values = [
      $person->nid,
      '\'' . $birth_date . '\'',
      '\'' . $gender . '\'',
      '\'' . $education_level . '\'',
      '\'' . $province . '\'',
      '\'' . $district . '\'',
      '\'' . $sector . '\'',
      '\'' . $cell . '\'',
      '\'' . $village . '\'',
    ];

    $columns_string = implode(', ', $columns);
    $values_string = implode(', ', $values);
    drush_print("INSERT INTO public.persons ($columns_string) VALUES ($values_string);");

    // Free up memory.
    drupal_static_reset();
  }
  // Explicitly unset large variables after use, for memory optimization.
  unset($persons);
}

