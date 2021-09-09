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

  // Exported field_base: 'field_administered_vaccines'.
  $field_bases['field_administered_vaccines'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_administered_vaccines',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'bcg-1' => 'BCG-1',
        'opv-1' => 'OPV-1',
        'opv-2' => 'OPV-2',
        'opv-3' => 'OPV-3',
        'opv-4' => 'OPV-4',
        'dtp-1' => 'DTP-1',
        'dtp-2' => 'DTP-2',
        'pcv13-1' => 'PCV13-1',
        'pcv13-2' => 'PCV13-2',
        'pcv13-3' => 'PCV13-3',
        'rotarix-1' => 'Rotarix-1',
        'rotarix-2' => 'Rotarix-2',
        'ipv-1' => 'IPV-1',
        'mr-1' => 'MR-1',
        'mr-2' => 'MR-2',
        'hpv-1' => 'HPV-1',
        'hpv-2' => 'HPV-2',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
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

  // Exported field_base: 'field_bcg_vaccination_date'.
  $field_bases['field_bcg_vaccination_date'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_bcg_vaccination_date',
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

  // Exported field_base: 'field_dtp_vaccination_date'.
  $field_bases['field_dtp_vaccination_date'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_dtp_vaccination_date',
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

  // Exported field_base: 'field_hpv_vaccination_date'.
  $field_bases['field_hpv_vaccination_date'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_hpv_vaccination_date',
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

  // Exported field_base: 'field_ipv_vaccination_date'.
  $field_bases['field_ipv_vaccination_date'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_ipv_vaccination_date',
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

  // Exported field_base: 'field_mr_vaccination_date'.
  $field_bases['field_mr_vaccination_date'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_mr_vaccination_date',
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

  // Exported field_base: 'field_opv_vaccination_date'.
  $field_bases['field_opv_vaccination_date'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_opv_vaccination_date',
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

  // Exported field_base: 'field_pcv13_vaccination_date'.
  $field_bases['field_pcv13_vaccination_date'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_pcv13_vaccination_date',
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

  // Exported field_base: 'field_rotarix_vaccination_date'.
  $field_bases['field_rotarix_vaccination_date'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_rotarix_vaccination_date',
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

  // Exported field_base: 'field_suggested_vaccines'.
  $field_bases['field_suggested_vaccines'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_suggested_vaccines',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'bcg-1' => 'BCG-1',
        'opv-1' => 'OPV-1',
        'opv-2' => 'OPV-2',
        'opv-3' => 'OPV-3',
        'opv-4' => 'OPV-4',
        'dtp-1' => 'DTP-1',
        'dtp-2' => 'DTP-2',
        'pcv13-1' => 'PCV13-1',
        'pcv13-2' => 'PCV13-2',
        'pcv13-3' => 'PCV13-3',
        'rotarix-1' => 'Rotarix-1',
        'rotarix-2' => 'Rotarix-2',
        'ipv-1' => 'IPV-1',
        'mr-1' => 'MR-1',
        'mr-2' => 'MR-2',
        'hpv-1' => 'HPV-1',
        'hpv-2' => 'HPV-2',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_vaccination_notes'.
  $field_bases['field_vaccination_notes'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_vaccination_notes',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'bcg-administered-today' => 'BCG Administered Today',
        'bcg-administered-previously' => 'BCG Administered Previously',
        'bcg-lack-of-stock' => 'BCG Not administered - Lack of stock',
        'bcg-known-allergy' => 'BCG Not administered - Known allergy',
        'bcg-patient-declined' => 'BCG Not administered - Patient declined',
        'bcg-patient-unable-to-afford' => 'BCG Not administered - Patient unable to afford',
        'bcg-home-birth' => 'BCG Not administered - Home birth',
        'bcg-childs-condition' => 'BCG Not administered - Childs condition',
        'bcg-other' => 'BCG Not administered - Other',
        'opv-administered-today' => 'OPV Administered Today',
        'opv-administered-previously' => 'OPV Administered Previously',
        'opv-lack-of-stock' => 'OPV Not administered - Lack of stock',
        'opv-known-allergy' => 'OPV Not administered - Known allergy',
        'opv-patient-declined' => 'OPV Not administered - Patient declined',
        'opv-patient-unable-to-afford' => 'OPV Not administered - Patient unable to afford',
        'opv-home-birth' => 'OPV Not administered - Home birth',
        'opv-childs-condition' => 'OPV Not administered - Childs condition',
        'opv-other' => 'OPV Not administered - Other',
        'dtp-administered-today' => 'DTP Administered Today',
        'dtp-administered-previously' => 'DTP Administered Previously',
        'dtp-lack-of-stock' => 'DTP Not administered - Lack of stock',
        'dtp-known-allergy' => 'DTP Not administered - Known allergy',
        'dtp-patient-declined' => 'DTP Not administered - Patient declined',
        'dtp-patient-unable-to-afford' => 'DTP Not administered - Patient unable to afford',
        'dtp-childs-condition' => 'DTP Not administered - Childs condition',
        'dtp-other' => 'DTP Not administered - Other',
        'pcv13-administered-today' => 'PCV13 Administered Today',
        'pcv13-administered-previously' => 'PCV13 Administered Previously',
        'pcv13-lack-of-stock' => 'PCV13 Not administered - Lack of stock',
        'pcv13-known-allergy' => 'PCV13 Not administered - Known allergy',
        'pcv13-patient-declined' => 'PCV13 Not administered - Patient declined',
        'pcv13-patient-unable-to-afford' => 'PCV13 Not administered - Patient unable to afford',
        'pcv-childs-condition' => 'PCV Not administered - Childs condition',
        'pcv13-other' => 'PCV13 Not administered - Other',
        'rotarix-administered-today' => 'Rotarix Administered Today',
        'rotarix-administered-previously' => 'Rotarix Administered Previously',
        'rotarix-lack-of-stock' => 'Rotarix Not administered - Lack of stock',
        'rotarix-known-allergy' => 'Rotarix Not administered - Known allergy',
        'rotarix-patient-declined' => 'Rotarix Not administered - Patient declined',
        'rotarix-patient-unable-to-afford' => 'Rotarix Not administered - Patient unable to afford',
        'rotarix-childs-condition' => 'Rotarix Not administered - Childs condition',
        'rotarix-other' => 'Rotarix Not administered - Other',
        'ipv-administered-today' => 'IPV Administered Today',
        'ipv-administered-previously' => 'IPV Administered Previously',
        'ipv-lack-of-stock' => 'IPV Not administered - Lack of stock',
        'ipv-known-allergy' => 'IPV Not administered - Known allergy',
        'ipv-patient-declined' => 'IPV Not administered - Patient declined',
        'ipv-patient-unable-to-afford' => 'IPV Not administered - Patient unable to afford',
        'ipv-childs-condition' => 'IPV Not administered - Childs condition',
        'ipv-other' => 'IPV Not administered - Other',
        'mr-administered-today' => 'MR Administered Today',
        'mr-administered-previously' => 'MR Administered Previously',
        'mr-lack-of-stock' => 'MR Not administered - Lack of stock',
        'mr-known-allergy' => 'MR Not administered - Known allergy',
        'mr-patient-declined' => 'MR Not administered - Patient declined',
        'mr-patient-unable-to-afford' => 'MR Not administered - Patient unable to afford',
        'mr-childs-condition' => 'MR Not administered - Childs condition',
        'mr-other' => 'MR Not administered - Other',
        'hpv-administered-today' => 'HPV Administered Today',
        'hpv-administered-previously' => 'HPV Administered Previously',
        'hpv-lack-of-stock' => 'HPV Not administered - Lack of stock',
        'hpv-known-allergy' => 'HPV Not administered - Known allergy',
        'hpv-patient-declined' => 'HPV Not administered - Patient declined',
        'hpv-patient-unable-to-afford' => 'HPV Not administered - Patient unable to afford',
        'hpv-childs-condition' => 'HPV Not administered - Childs condition',
        'hpv-other' => 'HPV Not administered - Other',
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
