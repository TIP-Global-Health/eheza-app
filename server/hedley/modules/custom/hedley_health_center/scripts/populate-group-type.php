<?php

/**
 * @file
 * Populate the type for all clinics.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_health_center/scripts/populate-group-type.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

$mapping = [
  'Clinic B' => 'pmtct',
  'Clinic B Test- Wendy' => 'pmtct',
  'Coko Health Center' => 'pmtct',
  'CYOHOHA PLANTATION' => 'sorwathe',
  'Deleted Clinic 7553' => 'pmtct',
  'FBF Rukura' => 'fbf',
  'FBF Ubudehe Coko' => 'fbf',
  'FBF Ubudehe Minazi' => 'fbf',
  'FBF Ubudehe Muhondo HC' => 'fbf',
  'FBF Ubudehe Muyongwe' => 'fbf',
  'FBF Ubudehe Nyange HC' => 'fbf',
  'FBF Ubudehe Rukura, Group 1 Rukura' => 'fbf',
  'FBF Ubudehe Rukura, Group 2 Mbogo' => 'fbf',
  'FBF Ubudehe Rukura, Group 3 Shyombwe' => 'fbf',
  'FBF Ubudehe Ruli HC' => 'fbf',
  'FBF Ubudehe Rushashi' => 'fbf',
  'FBF Ubudehe Rwankuba, Group 1 Bumba' => 'fbf',
  'FBF Ubudehe Rwankuba, Group 2 Rwankuba' => 'fbf',
  'FBF Ubudehe Rwankuba, Group 3, Va' => 'fbf',
  'Kigali Clinic 2- PRETEST' => 'sorwathe',
  'KIGALI PRESCHOOL' => 'sorwathe',
  'KIMIRYI II PRESCHOOL' => 'sorwathe',
  'KIMIRYI I PRESCHOOL' => 'sorwathe',
  'Minazi Health Center' => 'pmtct',
  'Muhondo Health Center' => 'pmtct',
  'MURAMBO II PRESCHOOL' => 'sorwathe',
  'MURAMBO I PRESCHOOL' => 'sorwathe',
  'MURWA II PRESCHOOL' => 'sorwathe',
  'MURWA I PRESCHOOL' => 'sorwathe',
  'Muyongwe Health Center' => 'pmtct',
  'Nyange Health Center' => 'pmtct',
  'RUKERI CRECHE' => 'sorwathe',
  'Rukura Health Center' => 'pmtct',
  'Ruli Hc' => 'pmtct',
  'Ruli Health Center' => 'pmtct',
  'RUNOGA II PRESCHOOL' => 'sorwathe',
  'RUNOGA I PRESCHOOL' => 'sorwathe',
  'Rushashi Health Center' => 'pmtct',
  'Rwankuba Health Center' => 'pmtct',
  'ST JOSEPH KINIHIRA I' => 'sorwathe',
  'ST JOSEPH KINIHIRA II' => 'sorwathe',
  'ST JOSEPH KINIHIRA III' => 'sorwathe',
  'Training Development Clinic' => 'pmtct',
];

$query = hedley_general_create_entity_field_query_excluding_deleted();
$query
  ->entityCondition('entity_type', 'node')
  ->propertyCondition('type', 'clinic')
  ->propertyCondition('status', NODE_PUBLISHED)
  ->propertyOrderBy('nid', 'ASC');

$result = $query->execute();

if (empty($result['node'])) {
  drush_print('No clinics found!');

  return;
}

$ids = array_keys($result['node']);

drush_print(format_string('Found @count clinics!', ['@count' => count($ids)]));

$clinics = node_load_multiple($ids);

foreach ($clinics as $clinic) {
  $wrapper = entity_metadata_wrapper('node', $clinic);
  $clinic_name = $wrapper->label();

  $clinic_type = $mapping[$clinic_name] ? $mapping[$clinic_name] : 'pmtct';

  drush_print(format_string('Assigning @type type to clinic @name', ['@name' => $clinic_name, '@type' => strtoupper($clinic_type)]));

  $wrapper->field_group_type->set($clinic_type);
  $wrapper->save();
}


drush_print('Done!');
