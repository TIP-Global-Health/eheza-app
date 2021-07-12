<?php

/**
 * @file
 * hedley_well_child.features.field_base.inc
 */

/**
 * Implements hook_field_default_field_bases().
 */
function hedley_well_child_field_default_field_bases() {
  $field_bases = array();

  // Exported field_base: 'field_ecd_signs'.
  $field_bases['field_ecd_signs'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_ecd_signs',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'respont-to-sound-with-sound' => 'Respont to Sound with Sound',
        'turn-head-when-called' => 'Turn Head When Called',
        'sit-without-support' => 'Sit Without Support',
        'smile-back' => 'Smile Back',
        'roll-tummy-to-back' => 'Roll Tummy To Back',
        'reach-for-toys' => 'Reach For Toys',
        'use-simple-gestures' => 'Use Simple Gestures',
        'stand-on-their-own' => 'Stand On Their Own',
        'copy-during-play' => 'Copy During Play',
        'say-mama-dada' => 'Say Mama Dada',
        'can-hold-small-objects' => 'Can Hold Small Objects',
        'looks-when-pointed-at' => 'Looks When Pointed At',
        'use-single-words' => 'Use Single Words',
        'walk-without-help' => 'Walk Without Help',
        'play-pretend' => 'Play Pretend',
        'point-to-things-of-interest' => 'Point To Things Of Interest',
        'use-short-phrases' => 'Use Short Phrases',
        'interested-in-other-children' => 'Interested In Other Children',
        'follow-simple-instructions' => 'Follow Simple Instructions',
        'kick-ball' => 'Kick Ball',
        'point-at-named-objects' => 'Point At Named Objects',
        'dress-themselves' => 'Dress Themselves',
        'wash-hands-go-to-toiled' => 'Wash Hands Go To Toiled',
        'knows-colors-and-numbers' => 'Knows Colors And Numbers',
        'use-medium-phrases' => 'Use Medium Phrases',
        'play-make-believe' => 'Play Make Believe',
        'follow-three-step-instructions' => 'Follow Three Step Instructions',
        'stand-on-one-foot-five-seconds' => 'Stand On One Foot Five Seconds',
        'use-long-phrases' => 'Use Long Phrases',
        'share-with-other-children' => 'Share With Other Children',
        'count-to-ten' => 'Count To Ten',
        'none' => 'None of these',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_head_circumference'.
  $field_bases['field_head_circumference'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_head_circumference',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(
      'decimal_separator' => '.',
    ),
    'translatable' => 0,
    'type' => 'number_float',
  );

  // Exported field_base: 'field_well_child_encounter'.
  $field_bases['field_well_child_encounter'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_well_child_encounter',
    'indexes' => array(
      'target_id' => array(
        0 => 'target_id',
      ),
    ),
    'locked' => 0,
    'module' => 'entityreference',
    'settings' => array(
      'handler' => 'base',
      'handler_settings' => array(
        'behaviors' => array(
          'views-select-list' => array(
            'status' => 0,
          ),
        ),
        'sort' => array(
          'type' => 'none',
        ),
        'target_bundles' => array(
          'well_child_encounter' => 'well_child_encounter',
        ),
      ),
      'target_type' => 'node',
    ),
    'translatable' => 0,
    'type' => 'entityreference',
  );

  // Exported field_base: 'field_well_child_symptoms'.
  $field_bases['field_well_child_symptoms'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_well_child_symptoms',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'breathing-problems' => 'Breathing Problems',
        'convulsions' => 'Convulsions',
        'lethargy-or-unresponsiveness' => 'Lethargy Or Unresponsiveness',
        'diarrhea' => 'Diarrhea',
        'vomiting' => 'Vomiting',
        'umbilical-cord-redness' => 'Umbilical Cord Redness',
        'stiff-neck-or-bulging-fontanelle' => 'Stiff Neck Or Bulging Fontanelle',
        'severe-edema' => 'Severe Edema',
        'palmoplantar-pallor' => 'Palmoplantar Pallor',
        'history-of-fever' => 'History Of Fever',
        'baby-tires-quickly-when-feeding' => 'Baby Tires Quickly When Feeding',
        'coughing-or-tearing-while-feeding' => 'Coughing Or Tearing While Feeding',
        'rigid-muscles-or-jaw-clenching' => 'Rigid Muscles Or Jaw Clenching Preventing Feeding',
        'excessive-sweating-when-feeding' => 'Excessive Sweating When Feeding',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  return $field_bases;
}
