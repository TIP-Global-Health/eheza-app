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

  // Exported field_base: 'field_administered_doses'.
  $field_bases['field_administered_doses'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_administered_doses',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'dose-1' => 'Dose 1',
        'dose-2' => 'Dose 2',
        'dose-3' => 'Dose 3',
        'dose-4' => 'Dose 4',
        'dose-5' => 'Dose 5',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_administration_dates'.
  $field_bases['field_administration_dates'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_administration_dates',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'date',
    'settings' => array(
      'cache_count' => 4,
      'cache_enabled' => 0,
      'granularity' => array(
        'day' => 'day',
        'hour' => 0,
        'minute' => 0,
        'month' => 'month',
        'second' => 0,
        'year' => 'year',
      ),
      'timezone_db' => '',
      'todate' => '',
      'tz_handling' => 'none',
    ),
    'translatable' => 0,
    'type' => 'datetime',
  );

  // Exported field_base: 'field_administration_note'.
  $field_bases['field_administration_note'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_administration_note',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'administered-today' => 'Administered Today',
        'administered-previously' => 'Administered Previously',
        'lack-of-stock' => 'Not administered - Lack of stock',
        'known-allergy' => 'Not administered - Known allergy',
        'patient-declined' => 'Not administered - Patient declined',
        'patient-unable-to-afford' => 'Not administered - Patient unable to afford',
        'childs-condition' => 'Not administered - Childs condition',
        'other' => 'Not administered - Other',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_apgar_five_min'.
  $field_bases['field_apgar_five_min'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_apgar_five_min',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(
      'decimal_separator' => '.',
    ),
    'translatable' => 0,
    'type' => 'number_float',
  );

  // Exported field_base: 'field_apgar_one_min'.
  $field_bases['field_apgar_one_min'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_apgar_one_min',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(
      'decimal_separator' => '.',
    ),
    'translatable' => 0,
    'type' => 'number_float',
  );

  // Exported field_base: 'field_asap_immunisation_date'.
  $field_bases['field_asap_immunisation_date'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_asap_immunisation_date',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'date',
    'settings' => array(
      'granularity' => array(
        'day' => 'day',
        'hour' => 0,
        'minute' => 0,
        'month' => 'month',
        'second' => 0,
        'year' => 'year',
      ),
      'timezone_db' => '',
      'todate' => '',
      'tz_handling' => 'none',
    ),
    'translatable' => 0,
    'type' => 'datetime',
  );

  // Exported field_base: 'field_birth_defects'.
  $field_bases['field_birth_defects'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_birth_defects',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'birth-injury' => 'Birth Injury',
        'cleft-lip-with-cleft-palate' => 'Cleft Lip With Cleft Palate',
        'cleft-palate' => 'Cleft Palate',
        'club-foot' => 'Club Foot',
        'macrocephaly' => 'Macrocephaly',
        'gastroschisis' => 'Gastroschisis',
        'hearing-loss' => 'Hearing Loss',
        'undescended-testes' => 'Undescended Testes',
        'hypospadias' => 'Hypospadias',
        'inguinal-hernia' => 'Inguinal Hernia',
        'microcephaly' => 'Microcephaly',
        'neural-tubes' => 'Neural Tubes',
        'down-syndrome' => 'Down Syndrome',
        'congenital-heart' => 'Congenital Heart',
        'ventrical-septal' => 'Ventrical Septal',
        'pulmonary-valve-atresia-and-stenosis' => 'Pulmonary Valve Atresia And Stenosis',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_delivery_complications'.
  $field_bases['field_delivery_complications'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_delivery_complications',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'gestational-diabetes' => 'Gestational Diabetes',
        'emergency-c-section' => 'Emergency C-Section',
        'preclampsia' => 'Preclampsia',
        'maternal-hemmorhage' => 'Maternal Hemmorhage',
        'hiv' => 'HIV',
        'maternal-death' => 'Maternal Death',
        'other' => 'Other',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

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
        'follow-mothers-eyes' => 'Follow Mothers Eyes',
        'move-arms-and-legs' => 'Move Arms And Legs',
        'raise-hands-up' => 'Raise Hands Up',
        'smile' => 'Smile',
        'roll-sideways' => 'Roll Sideways',
        'bring-hands-to-mouth' => 'Bring Hands To Mouth',
        'hold-head-without-support' => 'Hold Head Without Support',
        'hold-and-shake-toys' => 'Hold And Shake Toys',
        'react-to-sudden-sounds' => 'React To Sudden Sounds',
        'use-consonant-sounds' => 'Use Consonant Sounds',
        'respond-to-sound-with-sound' => 'Respond to Sound with Sound',
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

  // Exported field_base: 'field_immunisation_date'.
  $field_bases['field_immunisation_date'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_immunisation_date',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'date',
    'settings' => array(
      'cache_count' => 4,
      'cache_enabled' => 0,
      'granularity' => array(
        'day' => 'day',
        'hour' => 0,
        'minute' => 0,
        'month' => 'month',
        'second' => 0,
        'year' => 'year',
      ),
      'timezone_db' => '',
      'todate' => '',
      'tz_handling' => 'none',
    ),
    'translatable' => 0,
    'type' => 'datetime',
  );

  // Exported field_base: 'field_pediatric_visit_date'.
  $field_bases['field_pediatric_visit_date'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_pediatric_visit_date',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'date',
    'settings' => array(
      'cache_count' => 4,
      'cache_enabled' => 0,
      'granularity' => array(
        'day' => 'day',
        'hour' => 0,
        'minute' => 0,
        'month' => 'month',
        'second' => 0,
        'year' => 'year',
      ),
      'timezone_db' => '',
      'todate' => '',
      'tz_handling' => 'none',
    ),
    'translatable' => 0,
    'type' => 'datetime',
  );

  // Exported field_base: 'field_pregnancy_summary_signs'.
  $field_bases['field_pregnancy_summary_signs'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_pregnancy_summary_signs',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'apgar-scores' => 'Apgar Scores',
        'birth-length' => 'Birth Length',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
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

  // Exported field_base: 'field_well_child_encounter_type'.
  $field_bases['field_well_child_encounter_type'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_well_child_encounter_type',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'pediatric-care' => 'Pediatric Care',
        'pediatric-care-chw' => 'Pediatric Care for CHW',
        'newborn-exam' => 'Newborn Exam',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
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
