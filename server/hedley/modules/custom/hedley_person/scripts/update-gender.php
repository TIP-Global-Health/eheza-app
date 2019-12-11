<?php

/**
 * @file
 * Changing gender from female to male for predefined list of persons.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_person/scripts/update-gender.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

$ids = [
  28598,
  28632,
  28635,
  28637,
  28638,
  28640,
  28643,
  28644,
  28647,
  28648,
  28649,
  28656,
  28663,
  28664,
  28667,
  28668,
  28669,
  28671,
  28675,
  28679,
  28686,
  28689,
  28695,
  28697,
  28698,
  28705,
  28706,
  28711,
  28715,
  28716,
  28717,
  28723,
  28727,
  28731,
  28733,
  28734,
  28737,
  28739,
  28741,
  28742,
  28743,
  28744,
  28747,
  28755,
  28767,
  28768,
  28770,
  28772,
  28778,
  28779,
  28782,
  28784,
  28787,
  28792,
  28796,
  28801,
  28802,
  28804,
  28805,
  28806,
  28811,
  28818,
  28820,
  28824,
  28826,
  28827,
  28828,
  28832,
  28834,
  28835,
  28839,
  28840,
  28841,
  28842,
  28843,
  28844,
  28846,
  28849,
  28850,
  28858,
  28859,
  28860,
  28861,
  28862,
  28866,
  28873,
  28876,
  28881,
  28882,
  28884,
  28886,
  28888,
  28890,
  28895,
  28899,
  28901,
  28903,
  28905,
  28908,
  28909,
  28910,
  28915,
  28916,
  28919,
  28922,
  28925,
  28926,
  28929,
  28931,
  28937,
  28943,
  28946,
  28948,
  28953,
  28961,
  28963,
  28965,
  28967,
  28972,
  28973,
  28976,
  28977,
  28978,
  28981,
  28984,
  28988,
  28993,
  28994,
  28996,
  28998,
  29001,
  29005,
  29006,
  29007,
  29008,
  29015,
  29027,
  29030,
  29035,
  29040,
  29042,
  29044,
  29045,
  29047,
  29049,
  29050,
  29053,
  29057,
  29058,
  29063,
  29065,
  29067,
  29068,
  29075,
  29084,
  29085,
  29087,
  29088,
  29091,
  29092,
  29093,
  29108,
  29113,
  29114,
  29116,
  29117,
  29120,
  29122,
  29128,
  29130,
  29131,
  29133,
  29134,
  29135,
  29136,
  29139,
  29140,
  29141,
  29143,
  29147,
  29150,
  29153,
  29158,
  29160,
  29164,
  29165,
  29167,
  29168,
  29173,
  29184,
  29194,
  29195,
  29198,
  29199,
  29200,
  29201,
  29203,
  29204,
  29208,
  29209,
  29210,
  29214,
  29215,
  29219,
  29220,
  29221,
  29224,
  29225,
  29226,
  29227,
  29229,
  29234,
  29235,
  29237,
  29239,
  29240,
  29241,
  29242,
  29243,
  29244,
  29247,
  29349,
  29351,
  29354,
  29355,
  29359,
  29360,
  29361,
  29365,
  29366,
  29367,
  29368,
  29369,
  29372,
  29373,
  29375,
  29376,
  29378,
  29379,
  29386,
  29440,
  29443,
  29444,
  29446,
  29448,
  29449,
  29451,
  29453,
  29454,
  29455,
  29458,
  29459,
  29461,
  29462,
  29467,
  29468,
  29470,
  29471,
  29474,
  29478,
  29480,
  29482,
  29483,
  29484,
  29485,
  29486,
  29487,
  29488,
  29490,
  29493,
  29494,
  29495,
  29498,
  29500,
  29501,
  29502,
  29505,
  29508,
  29509,
  29512,
  29515,
  29516,
  29519,
  29520,
  29523,
  29525,
  29526,
  29532,
  29533,
  29534,
  29539,
  29541,
  29543,
  29545,
  29550,
  29552,
  29556,
  29559,
  29561,
  29565,
  29573,
  29574,
  29575,
  29582,
  29583,
  29584,
  29585,
  29586,
  29590,
  29601,
  29604,
  29605,
  29608,
  29611,
  29613,
  29969,
  30127,
  30211,
  30212,
  30217,
  30223,
  30321,
  30476,
  30575,
  30943,
  31114,
  31181,
  41228,
  51699,
  54796,
  84323,
];

drush_print(format_string('Starting update of @count persons!', ['@count' => count($ids)]));

foreach ($ids as $id) {
  $wrapper = entity_metadata_wrapper('node', $id);

  if ($wrapper->field_gender->value() == 'female') {
    drush_print(format_string('Changing gender from @gender to male for person with @id',
      ['@gender' => $wrapper->field_gender->value(), '@id' => $id])
    );
    $wrapper->field_gender->set('male');
    $wrapper->save();
  }
  else {
    drush_print(format_string('Gender of person with @id already set to male', ['@id' => $id]));
  }
}

drush_print('Done!');
