<?php

/**
 * @file
 * Validates villages migration. Needs adjustments for different migrations.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_migrate/scripts/validate-villages-migration.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

$from_nid = drush_get_option('from_nid', 1300000);

$query = base_query_for_bundle('clinic');
$query->propertyCondition('nid', $from_nid, '>');
$query->range(0, 1000);
$result = $query->execute();

$clinics = array_keys($result['node']);
foreach ($clinics as $clinic) {
  $participants = hedley_stats_get_pmtct_participants_by_clinic($clinic, 5000);
  $total = count($participants);

  $wrapper = entity_metadata_wrapper('node', $clinic);
  $clinic_name = $wrapper->label();
  $wrapper = entity_metadata_wrapper('node', $wrapper->field_health_center->getIdentifier());
  $hc_name = $wrapper->label();

  drush_print("$clinic_name at $hc_name got $total participants");
}

/**
 * Generate base query.
 */
function base_query_for_bundle($bundle): EntityFieldQuery {
  $query = hedley_general_create_entity_field_query_excluding_deleted();
  $query
    ->entityCondition('entity_type', 'node')
    ->propertyCondition('type', $bundle)
    ->propertyCondition('status', NODE_PUBLISHED);

  return $query;
}
