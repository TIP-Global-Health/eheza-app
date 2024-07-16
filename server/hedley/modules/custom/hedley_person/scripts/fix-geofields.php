<?php

/**
 * @file
 * Sets correct values at Geo fields.
 *
 * During import process, some patients were given incorrect
 * Geo fields values. This script fixes that.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_person/scripts/fix-geofields.php.
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
$memory_limit = drush_get_option('memory_limit', 500);

$mapping = [
  'province' => [
    'AMAJYARUGU' => 'Amajyaruguru',
    'North' => 'Amajyaruguru',
    'North Province' => 'Amajyaruguru',
    'Northern Province' => 'Amajyaruguru',
    'UBURASIRAZUBA' => 'Iburasirazuba',
    'Kigali City' => 'Umujyi wa kigali',
  ],
  'district' => [
    'NYAMATA' => 'Bugesera',
  ],
];

drush_print('Handling wrong values at province and district fields...');

foreach ($mapping as $geofield => $updates) {
  $geofield_name = "field_$geofield";
  $base_query = db_select("field_data_$geofield_name", 'gf')
    ->fields('gf', ['entity_id']);
  $base_query->condition('bundle', 'person');

  foreach ($updates as $current => $correct) {
    $query = clone $base_query;
    $query->condition("{$geofield_name}_value", $current);
    $ids = $query->execute()->fetchCol();
    $total = count($ids);

    if ($total == 0) {
      drush_print("There are no $geofield values set to $current.");
      continue;
    }

    drush_print("There are $total $geofield values set to $current.");

    $chunks = array_chunk($ids, $batch);
    $precessed = 0;
    foreach ($chunks as $chunk) {
      $nodes = node_load_multiple($chunk);
      foreach ($nodes as $node) {
        $node->{$geofield_name}[LANGUAGE_NONE][0]['value'] = $correct;
        node_save($node);
      }

      if (round(memory_get_usage() / 1048576) >= $memory_limit) {
        drush_print(dt('Stopped before out of memory.'));
        return;
      }

      $precessed += count($chunk);

      drush_print("Processed: $precessed/$total.");
    }
  }
}

drush_print('Handling empty cell fields...');

// Patients with empty cell field mapping.
$mapping = [
  // Patients with Akabenejuru village => cell should be set to Nkuzuzu.
  'Nkuzuzu' => [
    1283030,
    1283063,
    1283227,
  ],
  // Patients with Nyakabingo village => cell should be set to Mvuzo.
  'Mvuzo' => [
    1286046,
    1286047,
    1286048,
    1286049,
    1286050,
    1286051,
    1286052,
    1286053,
    1286054,
    1286055,
    1286056,
    1286057,
    1286058,
    1286059,
    1286060,
    1286061,
    1286062,
    1286063,
    1286064,
    1286065,
    1286066,
    1286067,
    1286068,
    1286069,
    1286070,
    1286071,
    1286072,
    1286073,
    1286074,
    1286075,
    1286076,
    1286077,
    1286078,
    1286079,
    1286080,
    1286081,
    1286082,
    1286083,
    1286084,
    1286085,
    1286086,
    1286087,
    1286088,
    1286089,
    1286090,
    1286091,
    1286092,
    1286093,
    1286094,
    1286095,
    1286096,
    1286097,
    1286098,
    1286099,
    1286100,
    1286101,
    1286102,
    1286103,
    1286104,
    1286105,
    1286106,
    1286107,
    1286108,
    1286110,
    1286112,
    1286114,
    1286116,
    1286118,
    1286120,
    1286122,
    1286124,
    1286126,
    1286128,
    1286130,
    1286132,
    1286134,
    1286136,
    1286138,
    1286140,
    1286142,
    1286144,
    1286146,
    1286148,
    1286150,
    1286152,
    1286154,
    1286156,
    1286158,
    1286160,
    1286162,
    1286164,
    1286166,
    1286168,
    1286170,
    1286172,
    1286174,
    1286176,
    1286178,
    1286180,
    1286182,
    1286184,
    1286186,
    1286188,
    1286190,
    1286192,
    1286194,
    1286196,
    1286198,
    1286200,
    1286202,
    1286204,
    1286206,
    1286208,
    1286210,
    1286212,
    1286214,
    1286216,
    1286218,
    1286220,
    1286222,
    1286224,
    1286226,
    1286228,
    1286230,
    1286232,
    1286234,
    1286236,
    1286238,
    1286240,
    1286242,
    1286244,
    1286246,
    1286248,
    1286250,
    1286252,
    1286254,
    1286256,
    1286258,
    1286260,
    1286262,
    1286264,
  ],
];

foreach ($mapping as $correct => $ids) {
  $total = count($ids);
  $precessed = 0;
  $chunks = array_chunk($ids, $batch);
  foreach ($chunks as $chunk) {
    $nodes = node_load_multiple($chunk);
    foreach ($nodes as $node) {
      $node->field_cell[LANGUAGE_NONE][0]['value'] = $correct;
      node_save($node);
    }

    if (round(memory_get_usage() / 1048576) >= $memory_limit) {
      drush_print(dt('Stopped before out of memory.'));
      return;
    }

    $precessed += count($chunk);

    drush_print("Processed: $precessed/$total.");
  }
}

drush_print('Done!');
