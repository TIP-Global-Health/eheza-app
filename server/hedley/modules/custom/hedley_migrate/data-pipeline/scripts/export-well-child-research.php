<?php

/**
 * @file
 * Export well-child encounter data to the revised research schema.
 *
 * This exporter implements the final schema design:
 * - dim_health_center: Health facility dimension
 * - dim_child: Child demographics with optional caregiver/birth data
 * - fact_encounter: Encounters with stored warnings
 * - fact_vaccination: Vaccines with pre-computed timing
 * - fact_growth: Measurements with Z-scores
 * - fact_ecd: ECD assessments with stored warnings
 * - fact_home_visit: Household socioeconomic data
 * - fact_medication: Vitamin A, deworming
 * - fact_danger_signs: Symptoms and vitals.
 *
 * Execution:
 *   ddev exec "cd /var/www/html/server/www && \
 *     drush scr /path/to/export-well-child-research.php" \
 *     > research-export.sql
 *
 * Options:
 *   --nid=N: Start from encounter node ID N (for resuming)
 *   --batch=N: Process N encounters per batch (default: 50)
 *   --site=rwanda|burundi: Set site for vaccine schedules (default: rwanda)
 */

if (!drupal_is_cli()) {
  return;
}

require_once dirname(__FILE__) . '/../lib/export-framework.php';

/**
 * Well-child research data exporter.
 */
class HedleyMigrateWellChildResearchExporter extends HedleyMigrateEntityExporter {

  /**
   * Vaccine schedule by site.
   *
   * @var array
   */
  protected $vaccineSchedule = [];

  /**
   * Track previous dose dates for interval-based timing.
   *
   * Keyed by "{child_id}:{vaccine_type}", value is
   * array of dose_num => timestamp.
   *
   * @var array
   */
  protected $previousDoseDates = [];

  /**
   * Cache of health centers.
   *
   * @var array
   */
  protected $healthCenters = [];

  /**
   * Cache of child data (birth dates, etc.) for age calculations.
   *
   * @var array
   */
  protected $childCache = [];

  /**
   * Constructor.
   */
  public function __construct($entity_type, array $config = []) {
    parent::__construct($entity_type, $config);
    $this->initializeVaccineSchedule();
  }

  /**
   * Initialize vaccine schedule reference data.
   */
  protected function initializeVaccineSchedule() {
    // Rwanda schedule (days from birth).
    $this->vaccineSchedule['rwanda'] = [
      'bcg' => [1 => ['expected' => 0, 'grace' => 28]],
      'opv' => [
        0 => ['expected' => 0, 'grace' => 28],
        1 => ['expected' => 42, 'grace' => 14],
        2 => ['expected' => 70, 'grace' => 14],
        3 => ['expected' => 98, 'grace' => 14],
      ],
      'dtp' => [
        1 => ['expected' => 42, 'grace' => 14],
        2 => ['expected' => 70, 'grace' => 14],
        3 => ['expected' => 98, 'grace' => 14],
      ],
      'pcv13' => [
        1 => ['expected' => 42, 'grace' => 14],
        2 => ['expected' => 70, 'grace' => 14],
        3 => ['expected' => 98, 'grace' => 14],
      ],
      'rotarix' => [
        1 => ['expected' => 42, 'grace' => 14],
        2 => ['expected' => 70, 'grace' => 14],
      ],
      'ipv' => [1 => ['expected' => 98, 'grace' => 28]],
      'mr' => [
        1 => ['expected' => 270, 'grace' => 28],
        2 => ['expected' => 450, 'grace' => 28],
      ],
      'hpv' => [
        1 => ['expected' => 3285, 'grace' => 60],
        2 => ['expected' => 3465, 'grace' => 60],
      ],
    ];

    // Burundi schedule (similar with minor differences).
    $this->vaccineSchedule['burundi'] = $this->vaccineSchedule['rwanda'];
  }

  /**
   * Map Drupal immunization types to standardized names.
   */
  protected function normalizeVaccineType($drupal_type) {
    $map = [
      'well_child_bcg_immunisation' => 'bcg',
      'well_child_dtp_immunisation' => 'dtp',
      'well_child_dtp_sa_immunisation' => 'dtp',
      'well_child_opv_immunisation' => 'opv',
      'well_child_ipv_immunisation' => 'ipv',
      'well_child_mr_immunisation' => 'mr',
      'well_child_pcv13_immunisation' => 'pcv13',
      'well_child_rotarix_immunisation' => 'rotarix',
      'well_child_hpv_immunisation' => 'hpv',
    ];
    return isset($map[$drupal_type]) ? $map[$drupal_type] : $drupal_type;
  }

  /**
   * Normalize ECD warning value from Drupal.
   */
  protected function normalizeEcdWarning($drupal_warnings) {
    if (empty($drupal_warnings)) {
      return 'none';
    }
    if (!is_array($drupal_warnings)) {
      $drupal_warnings = [$drupal_warnings];
    }

    foreach ($drupal_warnings as $warning) {
      if ($warning === 'warning-ecd-milestone-refer-to-specialist') {
        return 'refer_to_specialist';
      }
    }
    foreach ($drupal_warnings as $warning) {
      if ($warning === 'warning-ecd-milestone-behind') {
        return 'behind';
      }
    }
    return 'none';
  }

  /**
   * Normalize head circumference warning.
   */
  protected function normalizeHeadCircWarning($drupal_warnings) {
    if (empty($drupal_warnings)) {
      return 'none';
    }
    if (!is_array($drupal_warnings)) {
      $drupal_warnings = [$drupal_warnings];
    }

    foreach ($drupal_warnings as $warning) {
      if ($warning === 'warning-head-circumference-microcephaly') {
        return 'microcephaly';
      }
      if ($warning === 'warning-head-circumference-macrocephaly') {
        return 'macrocephaly';
      }
    }
    return 'none';
  }

  /**
   * {@inheritdoc}
   */
  protected function getSchemaConfig() {
    return [
      // Dimension: Health Centers.
      'dim_health_center' => [
        'columns' => [
          'health_center_id' => ['type' => 'BIGINT', 'constraint' => 'PRIMARY KEY'],
          'name' => ['type' => 'VARCHAR(200)', 'constraint' => 'NOT NULL'],
          'health_center_type' => ['type' => 'VARCHAR(50)'],
          'province' => ['type' => 'VARCHAR(100)'],
          'district' => ['type' => 'VARCHAR(100)'],
          'sector' => ['type' => 'VARCHAR(100)'],
          'site' => [
            'type' => 'VARCHAR(20)',
            'constraint' => 'NOT NULL',
            'default' => 'rwanda',
          ],
          'drupal_nid' => ['type' => 'BIGINT'],
          'created_at' => ['type' => 'TIMESTAMP', 'default' => 'CURRENT_TIMESTAMP'],
        ],
        'indexes' => [
          'idx_hc_province' => 'province',
          'idx_hc_district' => 'district',
        ],
      ],

      // Dimension: Children.
      'dim_child' => [
        'columns' => [
          'child_id' => ['type' => 'BIGINT', 'constraint' => 'PRIMARY KEY'],
          'national_id' => ['type' => 'VARCHAR(50)'],
          'hmis_number' => ['type' => 'VARCHAR(50)'],
          'gender' => ['type' => 'VARCHAR(10)', 'constraint' => 'NOT NULL'],
          'birth_date' => ['type' => 'DATE'],
          'birth_date_estimated' => ['type' => 'BOOLEAN', 'default' => 'FALSE'],
          'registration_date' => ['type' => 'DATE'],
          'province' => ['type' => 'VARCHAR(100)'],
          'district' => ['type' => 'VARCHAR(100)'],
          'sector' => ['type' => 'VARCHAR(100)'],
          'cell' => ['type' => 'VARCHAR(100)'],
          'village' => ['type' => 'VARCHAR(100)'],
          'health_center_id' => ['type' => 'BIGINT'],
          'hiv_status' => ['type' => 'VARCHAR(30)'],
          'birth_weight_grams' => ['type' => 'DECIMAL(6,1)'],
          'birth_length_cm' => ['type' => 'DOUBLE PRECISION'],
          'apgar_1_min' => ['type' => 'DECIMAL(3,1)'],
          'apgar_5_min' => ['type' => 'DECIMAL(3,1)'],
          'delivery_mode' => ['type' => 'VARCHAR(50)'],
          'has_birth_history' => ['type' => 'BOOLEAN', 'default' => 'FALSE'],
          'caregiver_id' => ['type' => 'BIGINT'],
          'caregiver_education' => ['type' => 'VARCHAR(50)'],
          'caregiver_ubudehe' => ['type' => 'VARCHAR(10)'],
          'has_caregiver_data' => ['type' => 'BOOLEAN', 'default' => 'FALSE'],
          'drupal_nid' => ['type' => 'BIGINT'],
          'created_at' => ['type' => 'TIMESTAMP', 'default' => 'CURRENT_TIMESTAMP'],
        ],
        'indexes' => [
          'idx_child_birth' => 'birth_date',
          'idx_child_hc' => 'health_center_id',
          'idx_child_district' => 'district',
          'idx_child_province' => 'province',
          'idx_child_gender' => 'gender',
        ],
      ],

      // Fact: Encounters.
      'fact_encounter' => [
        'columns' => [
          'encounter_id' => ['type' => 'BIGINT', 'constraint' => 'PRIMARY KEY'],
          'child_id' => ['type' => 'BIGINT', 'constraint' => 'NOT NULL'],
          'health_center_id' => ['type' => 'BIGINT'],
          'encounter_date' => ['type' => 'DATE', 'constraint' => 'NOT NULL'],
          'encounter_type' => ['type' => 'VARCHAR(30)', 'constraint' => 'NOT NULL'],
          'child_age_days' => ['type' => 'INTEGER'],
          'child_age_weeks' => ['type' => 'SMALLINT'],
          'child_age_months' => ['type' => 'SMALLINT'],
          'warning_ecd' => ['type' => 'VARCHAR(50)'],
          'warning_head_circumference' => ['type' => 'VARCHAR(50)'],
          'drupal_nid' => ['type' => 'BIGINT'],
          'created_at' => ['type' => 'TIMESTAMP', 'default' => 'CURRENT_TIMESTAMP'],
        ],
        'indexes' => [
          'idx_enc_child' => 'child_id',
          'idx_enc_date' => 'encounter_date',
          'idx_enc_hc' => 'health_center_id',
          'idx_enc_type' => 'encounter_type',
          'idx_enc_warning_ecd' => 'warning_ecd',
        ],
      ],

      // Fact: Vaccinations.
      'fact_vaccination' => [
        'columns' => [
          'vaccination_id' => ['type' => 'BIGSERIAL', 'constraint' => 'PRIMARY KEY'],
          'child_id' => ['type' => 'BIGINT', 'constraint' => 'NOT NULL'],
          'encounter_id' => ['type' => 'BIGINT'],
          'vaccine_type' => ['type' => 'VARCHAR(30)', 'constraint' => 'NOT NULL'],
          'dose_number' => ['type' => 'SMALLINT', 'constraint' => 'NOT NULL'],
          'administration_date' => ['type' => 'DATE', 'constraint' => 'NOT NULL'],
          'child_age_days' => ['type' => 'INTEGER'],
          'expected_age_days' => ['type' => 'INTEGER'],
          'days_from_expected' => ['type' => 'INTEGER'],
          'on_time' => ['type' => 'BOOLEAN'],
          'timing_category' => ['type' => 'VARCHAR(20)'],
          'administration_note' => ['type' => 'VARCHAR(100)'],
          'drupal_nid' => ['type' => 'BIGINT'],
          'created_at' => ['type' => 'TIMESTAMP', 'default' => 'CURRENT_TIMESTAMP'],
        ],
        'indexes' => [
          'idx_vacc_child' => 'child_id',
          'idx_vacc_encounter' => 'encounter_id',
          'idx_vacc_type' => ['vaccine_type', 'dose_number'],
          'idx_vacc_date' => 'administration_date',
          'idx_vacc_timing' => 'on_time',
        ],
      ],

      // Fact: Growth.
      'fact_growth' => [
        'columns' => [
          'growth_id' => ['type' => 'BIGSERIAL', 'constraint' => 'PRIMARY KEY'],
          'child_id' => ['type' => 'BIGINT', 'constraint' => 'NOT NULL'],
          'encounter_id' => ['type' => 'BIGINT'],
          'measurement_date' => ['type' => 'DATE', 'constraint' => 'NOT NULL'],
          'child_age_days' => ['type' => 'INTEGER'],
          'child_age_months' => ['type' => 'SMALLINT'],
          'weight_kg' => ['type' => 'DOUBLE PRECISION'],
          'height_cm' => ['type' => 'DOUBLE PRECISION'],
          'muac_cm' => ['type' => 'DOUBLE PRECISION'],
          'head_circumference_cm' => ['type' => 'DOUBLE PRECISION'],
          'zscore_age' => ['type' => 'DOUBLE PRECISION'],
          'zscore_length' => ['type' => 'DOUBLE PRECISION'],
          'zscore_bmi' => ['type' => 'DOUBLE PRECISION'],
          'stunting_status' => ['type' => 'VARCHAR(20)'],
          'wasting_status' => ['type' => 'VARCHAR(20)'],
          'underweight_status' => ['type' => 'VARCHAR(20)'],
          'drupal_nid' => ['type' => 'BIGINT'],
          'created_at' => ['type' => 'TIMESTAMP', 'default' => 'CURRENT_TIMESTAMP'],
        ],
        'indexes' => [
          'idx_growth_child' => 'child_id',
          'idx_growth_encounter' => 'encounter_id',
          'idx_growth_date' => 'measurement_date',
          'idx_growth_stunting' => 'stunting_status',
        ],
      ],

      // Fact: ECD.
      'fact_ecd' => [
        'columns' => [
          'ecd_id' => ['type' => 'BIGSERIAL', 'constraint' => 'PRIMARY KEY'],
          'child_id' => ['type' => 'BIGINT', 'constraint' => 'NOT NULL'],
          'encounter_id' => ['type' => 'BIGINT'],
          'assessment_date' => ['type' => 'DATE', 'constraint' => 'NOT NULL'],
          'child_age_weeks' => ['type' => 'SMALLINT'],
          'child_age_months' => ['type' => 'SMALLINT'],
          'achieved_milestones' => ['type' => 'TEXT[]'],
          'achieved_count' => ['type' => 'INTEGER'],
          'ecd_warning' => ['type' => 'VARCHAR(50)'],
          'drupal_nid' => ['type' => 'BIGINT'],
          'created_at' => ['type' => 'TIMESTAMP', 'default' => 'CURRENT_TIMESTAMP'],
        ],
        'indexes' => [
          'idx_ecd_child' => 'child_id',
          'idx_ecd_encounter' => 'encounter_id',
          'idx_ecd_warning' => 'ecd_warning',
        ],
      ],

      // Fact: Home Visit.
      'fact_home_visit' => [
        'columns' => [
          'home_visit_id' => ['type' => 'BIGSERIAL', 'constraint' => 'PRIMARY KEY'],
          'child_id' => ['type' => 'BIGINT', 'constraint' => 'NOT NULL'],
          'encounter_id' => ['type' => 'BIGINT'],
          'visit_date' => ['type' => 'DATE', 'constraint' => 'NOT NULL'],
          'main_water_source' => ['type' => 'VARCHAR(50)'],
          'water_preparation' => ['type' => 'VARCHAR(50)'],
          'has_soap' => ['type' => 'BOOLEAN'],
          'wash_hands_before_feeding' => ['type' => 'BOOLEAN'],
          'food_covered' => ['type' => 'BOOLEAN'],
          'main_income_source' => ['type' => 'VARCHAR(50)'],
          'household_has_food' => ['type' => 'BOOLEAN'],
          'is_breastfeeding' => ['type' => 'BOOLEAN'],
          'receives_supplement' => ['type' => 'BOOLEAN'],
          'supplement_type' => ['type' => 'VARCHAR(50)'],
          'parents_alive_healthy' => ['type' => 'BOOLEAN'],
          'child_clean' => ['type' => 'BOOLEAN'],
          'caring_option' => ['type' => 'VARCHAR(50)'],
          'drupal_nid' => ['type' => 'BIGINT'],
          'created_at' => ['type' => 'TIMESTAMP', 'default' => 'CURRENT_TIMESTAMP'],
        ],
        'indexes' => [
          'idx_hv_child' => 'child_id',
          'idx_hv_encounter' => 'encounter_id',
          'idx_hv_income' => 'main_income_source',
          'idx_hv_water' => 'main_water_source',
        ],
      ],

      // Fact: Medication.
      'fact_medication' => [
        'columns' => [
          'medication_id' => ['type' => 'BIGSERIAL', 'constraint' => 'PRIMARY KEY'],
          'child_id' => ['type' => 'BIGINT', 'constraint' => 'NOT NULL'],
          'encounter_id' => ['type' => 'BIGINT'],
          'administration_date' => ['type' => 'DATE', 'constraint' => 'NOT NULL'],
          'child_age_months' => ['type' => 'SMALLINT'],
          'medication_type' => ['type' => 'VARCHAR(30)', 'constraint' => 'NOT NULL'],
          'administered' => ['type' => 'BOOLEAN', 'constraint' => 'NOT NULL'],
          'non_administration_reason' => ['type' => 'VARCHAR(100)'],
          'drupal_nid' => ['type' => 'BIGINT'],
          'created_at' => ['type' => 'TIMESTAMP', 'default' => 'CURRENT_TIMESTAMP'],
        ],
        'indexes' => [
          'idx_med_child' => 'child_id',
          'idx_med_type' => 'medication_type',
        ],
      ],

      // Fact: Danger Signs.
      'fact_danger_signs' => [
        'columns' => [
          'danger_sign_id' => ['type' => 'BIGSERIAL', 'constraint' => 'PRIMARY KEY'],
          'child_id' => ['type' => 'BIGINT', 'constraint' => 'NOT NULL'],
          'encounter_id' => ['type' => 'BIGINT'],
          'assessment_date' => ['type' => 'DATE', 'constraint' => 'NOT NULL'],
          'has_breathing_problems' => ['type' => 'BOOLEAN', 'default' => 'FALSE'],
          'has_diarrhea' => ['type' => 'BOOLEAN', 'default' => 'FALSE'],
          'has_vomiting' => ['type' => 'BOOLEAN', 'default' => 'FALSE'],
          'has_fever' => ['type' => 'BOOLEAN', 'default' => 'FALSE'],
          'has_lethargy' => ['type' => 'BOOLEAN', 'default' => 'FALSE'],
          'heart_rate' => ['type' => 'SMALLINT'],
          'respiratory_rate' => ['type' => 'SMALLINT'],
          'body_temperature' => ['type' => 'DOUBLE PRECISION'],
          'drupal_nid' => ['type' => 'BIGINT'],
          'created_at' => ['type' => 'TIMESTAMP', 'default' => 'CURRENT_TIMESTAMP'],
        ],
        'indexes' => [
          'idx_danger_child' => 'child_id',
          'idx_danger_encounter' => 'encounter_id',
        ],
      ],
    ];
  }

  /**
   * {@inheritdoc}
   */
  protected function exportData() {
    drush_print("-- Starting data export...");
    drush_print("-- Time: " . date('Y-m-d H:i:s'));
    drush_print("");

    // Export in correct order for foreign key constraints.
    $this->exportHealthCenters();
    $this->exportChildren();
    $this->exportEncountersAndMeasurements();

    drush_print("");
    drush_print("-- Data export complete");
    drush_print("-- Time: " . date('Y-m-d H:i:s'));
  }

  /**
   * Export health centers dimension.
   */
  protected function exportHealthCenters() {
    drush_print("-- Exporting health centers...");

    $query = db_select('node', 'n');
    $query->condition('n.type', 'health_center');
    $query->condition('n.status', NODE_PUBLISHED);
    $query->fields('n', ['nid']);
    $hc_ids = $query->execute()->fetchCol();

    if (empty($hc_ids)) {
      drush_print("-- No health centers found");
      return;
    }

    drush_print("-- Found " . count($hc_ids) . " health centers");

    $health_centers = node_load_multiple($hc_ids);
    foreach ($health_centers as $hc) {
      $wrapper = entity_metadata_wrapper('node', $hc);

      // Check if deleted.
      if ($this->safeGetFieldValue($wrapper, 'field_deleted')) {
        continue;
      }

      $province = $this->safeGetFieldValue($wrapper, 'field_province');
      $district = $this->safeGetFieldValue($wrapper, 'field_district');
      $sector = $this->safeGetFieldValue($wrapper, 'field_sector');

      $this->printInsert('dim_health_center', [
        'health_center_id' => $hc->nid,
        'name' => $hc->title,
        'health_center_type' => $this->safeGetFieldValue($wrapper, 'field_health_center_type'),
        'province' => $province,
        'district' => $district,
        'sector' => $sector,
        'site' => $this->site,
        'drupal_nid' => $hc->nid,
      ]);

      $this->markExported('health_center', $hc->nid);
      $this->healthCenters[$hc->nid] = [
        'name' => $hc->title,
        'province' => $province,
        'district' => $district,
      ];
    }

    entity_get_controller('node')->resetCache(array_keys($health_centers));
    drush_print("-- Exported " . count($this->healthCenters) . " health centers");
    drush_print("");
  }

  /**
   * Export children dimension.
   */
  protected function exportChildren() {
    drush_print("-- Exporting children...");

    // Get all child IDs from well-child encounters.
    $query = db_select('node', 'n');
    $query->join('field_data_field_individual_participant', 'part', 'part.entity_id = n.nid');
    $query->join('field_data_field_person', 'p', 'p.entity_id = part.field_individual_participant_target_id');
    $query->condition('n.type', 'well_child_encounter');
    $query->condition('n.status', NODE_PUBLISHED);
    $query->fields('p', ['field_person_target_id']);
    $query->distinct();

    $child_ids = $query->execute()->fetchCol();

    if (empty($child_ids)) {
      drush_print("-- No children found");
      return;
    }

    drush_print("-- Found " . count($child_ids) . " unique children");

    // Process in batches.
    $batch_size = 500;
    $batches = array_chunk($child_ids, $batch_size);
    $total_exported = 0;

    foreach ($batches as $batch_num => $batch_ids) {
      drush_print("-- Processing child batch " . ($batch_num + 1) . " of " . count($batches));

      $children = node_load_multiple($batch_ids);

      foreach ($children as $child) {
        $wrapper = entity_metadata_wrapper('node', $child);

        // Skip deleted.
        if ($this->safeGetFieldValue($wrapper, 'field_deleted')) {
          continue;
        }

        // Get basic demographics.
        $birth_date = $this->safeGetFieldValue($wrapper, 'field_birth_date');
        $birth_ts = $this->convertDateToTimestamp($birth_date);
        $gender = $this->safeGetFieldValue($wrapper, 'field_gender') ?: 'unknown';

        // Get health center.
        $hc_id = NULL;
        $hc_ref = $this->safeGetFieldValue($wrapper, 'field_health_center');
        if ($hc_ref) {
          $hc_id = is_object($hc_ref) ? $hc_ref->nid : $hc_ref;
          // Only use if health center was exported.
          if (!$this->wasExported('health_center', $hc_id)) {
            $hc_id = NULL;
          }
        }

        // Get location.
        $province = $this->safeGetFieldValue($wrapper, 'field_province');
        $district = $this->safeGetFieldValue($wrapper, 'field_district');
        $sector = $this->safeGetFieldValue($wrapper, 'field_sector');
        $cell = $this->safeGetFieldValue($wrapper, 'field_cell');
        $village = $this->safeGetFieldValue($wrapper, 'field_village');

        // Get HIV status.
        $hiv_status = $this->safeGetFieldValue($wrapper, 'field_hiv_status');

        // Try to get caregiver data via relationship.
        $caregiver_data = $this->getCaregiverData($child->nid);

        // Output child record.
        $this->printInsert('dim_child', [
          'child_id' => $child->nid,
          'national_id' => $this->safeGetFieldValue($wrapper, 'field_national_id_number'),
          'hmis_number' => $this->safeGetFieldValue($wrapper, 'field_hmis_number'),
          'gender' => $gender,
          'birth_date' => $birth_ts,
          'birth_date_estimated' => $this->safeGetFieldValue($wrapper, 'field_birth_date_estimated') ? TRUE : FALSE,
          'registration_date' => $child->created,
          'province' => $province,
          'district' => $district,
          'sector' => $sector,
          'cell' => $cell,
          'village' => $village,
          'health_center_id' => $hc_id,
          'hiv_status' => $hiv_status,
          // Will be updated from newborn exam.
          'birth_weight_grams' => NULL,
          'birth_length_cm' => NULL,
          'apgar_1_min' => NULL,
          'apgar_5_min' => NULL,
          'delivery_mode' => NULL,
          'has_birth_history' => FALSE,
          'caregiver_id' => $caregiver_data['id'],
          'caregiver_education' => $caregiver_data['education'],
          'caregiver_ubudehe' => $caregiver_data['ubudehe'],
          'has_caregiver_data' => $caregiver_data['id'] ? TRUE : FALSE,
          'drupal_nid' => $child->nid,
        ]);

        $this->markExported('child', $child->nid);
        $this->childCache[$child->nid] = [
          'birth_date' => $birth_ts,
          'gender' => $gender,
        ];
        $total_exported++;
      }

      // Clear memory.
      entity_get_controller('node')->resetCache(array_keys($children));
      unset($children);
    }

    drush_print("-- Exported $total_exported children");
    drush_print("");
  }

  /**
   * Get caregiver data for a child via relationship.
   */
  protected function getCaregiverData($child_nid) {
    $result = [
      'id' => NULL,
      'education' => NULL,
      'ubudehe' => NULL,
    ];

    // Find relationship where child is field_related_to.
    $query = db_select('node', 'n');
    $query->join('field_data_field_related_to', 'rt', 'rt.entity_id = n.nid');
    $query->join('field_data_field_person', 'p', 'p.entity_id = n.nid');
    $query->condition('n.type', 'relationship');
    $query->condition('n.status', NODE_PUBLISHED);
    $query->condition('rt.field_related_to_target_id', $child_nid);
    $query->fields('p', ['field_person_target_id']);
    $query->range(0, 1);

    $caregiver_nid = $query->execute()->fetchField();

    if (!$caregiver_nid) {
      return $result;
    }

    // Load caregiver and get education/ubudehe.
    $caregiver = node_load($caregiver_nid);
    if (!$caregiver) {
      return $result;
    }

    $wrapper = entity_metadata_wrapper('node', $caregiver);

    $result['id'] = $caregiver_nid;
    $result['education'] = $this->safeGetFieldValue($wrapper, 'field_education_level');
    $result['ubudehe'] = $this->safeGetFieldValue($wrapper, 'field_ubudehe');

    return $result;
  }

  /**
   * Export encounters and all related measurements.
   */
  protected function exportEncountersAndMeasurements() {
    $start_nid = drush_get_option('nid', 0);
    $batch_size = $this->batchSize;

    drush_print("-- Exporting encounters and measurements...");
    if ($start_nid > 0) {
      drush_print("-- Resuming from NID: $start_nid");
    }

    // Count total encounters.
    $count_query = db_select('node', 'n');
    $count_query->condition('n.type', 'well_child_encounter');
    $count_query->condition('n.status', NODE_PUBLISHED);
    if ($start_nid > 0) {
      $count_query->condition('n.nid', $start_nid, '>');
    }
    $total = $count_query->countQuery()->execute()->fetchField();

    drush_print("-- Total encounters to process: $total");

    $processed = 0;
    $last_nid = $start_nid;

    while (TRUE) {
      // Query batch of encounters.
      $query = db_select('node', 'n');
      $query->condition('n.type', 'well_child_encounter');
      $query->condition('n.status', NODE_PUBLISHED);
      $query->condition('n.nid', $last_nid, '>');
      $query->orderBy('n.nid', 'ASC');
      $query->range(0, $batch_size);
      $query->fields('n', ['nid']);

      $nids = $query->execute()->fetchCol();

      if (empty($nids)) {
        break;
      }

      $encounters = node_load_multiple($nids);

      foreach ($encounters as $encounter) {
        $this->exportSingleEncounter($encounter);
        $last_nid = $encounter->nid;
        $processed++;
      }

      // Progress report.
      if ($processed % 500 == 0) {
        drush_print("-- Processed $processed / $total encounters...");
      }

      // Check memory.
      if (!$this->checkMemoryLimit($last_nid)) {
        break;
      }

      // Clear memory.
      entity_get_controller('node')->resetCache(array_keys($encounters));
      unset($encounters);
    }

    drush_print("-- Exported $processed encounters");
  }

  /**
   * Export a single encounter and all its measurements.
   */
  protected function exportSingleEncounter($encounter) {
    $wrapper = entity_metadata_wrapper('node', $encounter);

    // Skip deleted.
    if ($this->safeGetFieldValue($wrapper, 'field_deleted')) {
      return;
    }

    // Get participant and child.
    $participant = $this->safeGetFieldValue($wrapper, 'field_individual_participant');
    if (!$participant) {
      return;
    }

    $participant_wrapper = entity_metadata_wrapper('node', $participant);
    $child_ref = $this->safeGetFieldValue($participant_wrapper, 'field_person');
    if (!$child_ref) {
      return;
    }

    $child_id = is_object($child_ref) ? $child_ref->nid : $child_ref;

    // Skip if child wasn't exported.
    if (!$this->wasExported('child', $child_id)) {
      return;
    }

    // Get encounter date.
    $encounter_date = $this->safeGetFieldValue($wrapper, 'field_scheduled_date');
    $encounter_ts = $this->convertDateToTimestamp($encounter_date);
    if (!$encounter_ts) {
      return;
    }

    // Get health center.
    $hc_id = NULL;
    $clinic_ref = $this->safeGetFieldValue($participant_wrapper, 'field_clinic');
    if ($clinic_ref) {
      $hc_id = is_object($clinic_ref) ? $clinic_ref->nid : $clinic_ref;
      if (!$this->wasExported('health_center', $hc_id)) {
        $hc_id = NULL;
      }
    }

    // Calculate child age at encounter.
    $child_birth = isset($this->childCache[$child_id]['birth_date']) ? $this->childCache[$child_id]['birth_date'] : NULL;
    $age_days = $this->calculateAgeDays($child_birth, $encounter_ts);
    $age_weeks = $this->calculateAgeWeeks($child_birth, $encounter_ts);
    $age_months = $this->calculateAgeMonths($child_birth, $encounter_ts);

    // Get warnings from encounter.
    $warnings = $this->safeGetMultiFieldValue($wrapper, 'field_encounter_warnings');
    $warning_ecd = $this->normalizeEcdWarning($warnings);
    $warning_head = $this->normalizeHeadCircWarning($warnings);

    // Determine encounter type.
    $encounter_type = 'pediatric_care';
    // Could check for CHW encounters or newborn exams.
    // Export encounter.
    $this->printInsert('fact_encounter', [
      'encounter_id' => $encounter->nid,
      'child_id' => $child_id,
      'health_center_id' => $hc_id,
      'encounter_date' => $encounter_ts,
      'encounter_type' => $encounter_type,
      'child_age_days' => $age_days,
      'child_age_weeks' => $age_weeks,
      'child_age_months' => $age_months,
      'warning_ecd' => $warning_ecd,
      'warning_head_circumference' => $warning_head,
      'drupal_nid' => $encounter->nid,
    ]);

    $this->markExported('encounter', $encounter->nid);

    // Export all measurements for this encounter.
    $this->exportEncounterMeasurements($encounter->nid, $child_id, $child_birth, $encounter_ts, $warning_ecd);
  }

  /**
   * Export all measurements for an encounter.
   */
  protected function exportEncounterMeasurements($encounter_id, $child_id, $child_birth, $encounter_ts, $ecd_warning) {
    // Query all measurements referencing this encounter.
    $measurement_types = [
      // Growth measurements.
      'well_child_height',
      'well_child_weight',
      'well_child_muac',
      'well_child_head_circumference',
      // Immunizations.
      'well_child_bcg_immunisation',
      'well_child_dtp_immunisation',
      'well_child_dtp_sa_immunisation',
      'well_child_opv_immunisation',
      'well_child_ipv_immunisation',
      'well_child_mr_immunisation',
      'well_child_pcv13_immunisation',
      'well_child_rotarix_immunisation',
      'well_child_hpv_immunisation',
      // ECD.
      'well_child_ecd',
      // Medications.
      'well_child_albendazole',
      'well_child_mebendezole',
      'well_child_vitamin_a',
      // Vitals.
      'well_child_vitals',
      // Home visit data.
      'well_child_hygiene',
      'well_child_food_security',
      'well_child_feeding',
      'well_child_caring',
      // Danger signs.
      'well_child_symptoms_review',
    ];

    foreach ($measurement_types as $type) {
      $this->exportMeasurementsByType($type, $encounter_id, $child_id, $child_birth, $encounter_ts, $ecd_warning);
    }
  }

  /**
   * Export measurements of a specific type for an encounter.
   */
  protected function exportMeasurementsByType($type, $encounter_id, $child_id, $child_birth, $encounter_ts, $ecd_warning) {
    $query = db_select('node', 'n');
    $query->join('field_data_field_well_child_encounter', 'e', 'e.entity_id = n.nid');
    $query->condition('n.type', $type);
    $query->condition('n.status', NODE_PUBLISHED);
    $query->condition('e.field_well_child_encounter_target_id', $encounter_id);
    $query->fields('n', ['nid']);

    $nids = $query->execute()->fetchCol();

    if (empty($nids)) {
      return;
    }

    $measurements = node_load_multiple($nids);

    foreach ($measurements as $measurement) {
      $this->exportSingleMeasurement($measurement, $type, $encounter_id, $child_id, $child_birth, $encounter_ts, $ecd_warning);
    }

    entity_get_controller('node')->resetCache(array_keys($measurements));
  }

  /**
   * Export a single measurement based on its type.
   */
  protected function exportSingleMeasurement($measurement, $type, $encounter_id, $child_id, $child_birth, $encounter_ts, $ecd_warning) {
    $wrapper = entity_metadata_wrapper('node', $measurement);

    // Skip deleted.
    if ($this->safeGetFieldValue($wrapper, 'field_deleted')) {
      return;
    }

    // Get measurement date.
    $measured_date = $this->safeGetFieldValue($wrapper, 'field_date_measured');
    $measured_ts = $this->convertDateToTimestamp($measured_date);
    if (!$measured_ts) {
      // Fall back to encounter date.
      $measured_ts = $encounter_ts;
    }

    // Route to appropriate handler.
    switch ($type) {
      case 'well_child_height':
        $this->exportGrowthMeasurement($measurement, $wrapper, $encounter_id, $child_id, $child_birth, $measured_ts, 'height');
        break;

      case 'well_child_weight':
        $this->exportGrowthMeasurement($measurement, $wrapper, $encounter_id, $child_id, $child_birth, $measured_ts, 'weight');
        break;

      case 'well_child_muac':
        $this->exportGrowthMeasurement($measurement, $wrapper, $encounter_id, $child_id, $child_birth, $measured_ts, 'muac');
        break;

      case 'well_child_head_circumference':
        $this->exportGrowthMeasurement($measurement, $wrapper, $encounter_id, $child_id, $child_birth, $measured_ts, 'head_circ');
        break;

      case 'well_child_bcg_immunisation':
      case 'well_child_dtp_immunisation':
      case 'well_child_dtp_sa_immunisation':
      case 'well_child_opv_immunisation':
      case 'well_child_ipv_immunisation':
      case 'well_child_mr_immunisation':
      case 'well_child_pcv13_immunisation':
      case 'well_child_rotarix_immunisation':
      case 'well_child_hpv_immunisation':
        $this->exportVaccination($measurement, $wrapper, $type, $encounter_id, $child_id, $child_birth, $measured_ts);
        break;

      case 'well_child_ecd':
        $this->exportEcd($measurement, $wrapper, $encounter_id, $child_id, $child_birth, $measured_ts, $ecd_warning);
        break;

      case 'well_child_albendazole':
      case 'well_child_mebendezole':
      case 'well_child_vitamin_a':
        $this->exportMedication($measurement, $wrapper, $type, $encounter_id, $child_id, $child_birth, $measured_ts);
        break;

      case 'well_child_vitals':
        $this->exportVitals($measurement, $wrapper, $encounter_id, $child_id, $measured_ts);
        break;

      case 'well_child_hygiene':
      case 'well_child_food_security':
      case 'well_child_feeding':
      case 'well_child_caring':
        $this->exportHomeVisitData($measurement, $wrapper, $type, $encounter_id, $child_id, $measured_ts);
        break;

      case 'well_child_symptoms_review':
        $this->exportDangerSigns($measurement, $wrapper, $encounter_id, $child_id, $measured_ts);
        break;
    }
  }

  /**
   * Export growth measurement.
   */
  protected function exportGrowthMeasurement($measurement, $wrapper, $encounter_id, $child_id, $child_birth, $measured_ts, $measurement_type) {
    $age_days = $this->calculateAgeDays($child_birth, $measured_ts);
    $age_months = $this->calculateAgeMonths($child_birth, $measured_ts);

    $weight = NULL;
    $height = NULL;
    $muac = NULL;
    $head_circ = NULL;
    $zscore_age = NULL;
    $zscore_length = NULL;
    $zscore_bmi = NULL;

    switch ($measurement_type) {
      case 'height':
        $height = $this->safeGetFieldValue($wrapper, 'field_height');
        $zscore_age = $this->safeGetFieldValue($wrapper, 'field_zscore_age');
        break;

      case 'weight':
        $weight = $this->safeGetFieldValue($wrapper, 'field_weight');
        $zscore_age = $this->safeGetFieldValue($wrapper, 'field_zscore_age');
        $zscore_bmi = $this->safeGetFieldValue($wrapper, 'field_zscore_bmi');
        $zscore_length = $this->safeGetFieldValue($wrapper, 'field_zscore_length');
        break;

      case 'muac':
        $muac = $this->safeGetFieldValue($wrapper, 'field_muac');
        break;

      case 'head_circ':
        $head_circ = $this->safeGetFieldValue($wrapper, 'field_head_circumference');
        $zscore_age = $this->safeGetFieldValue($wrapper, 'field_zscore_age');
        break;
    }

    // Derive nutrition status from Z-score.
    $stunting = ($measurement_type === 'height') ? $this->getNutritionStatus($zscore_age) : NULL;
    $wasting = ($measurement_type === 'weight' && $zscore_length) ? $this->getNutritionStatus($zscore_length) : NULL;
    $underweight = ($measurement_type === 'weight' && $zscore_age) ? $this->getNutritionStatus($zscore_age) : NULL;

    $this->printInsert('fact_growth', [
      'child_id' => $child_id,
      'encounter_id' => $encounter_id,
      'measurement_date' => $measured_ts,
      'child_age_days' => $age_days,
      'child_age_months' => $age_months,
      'weight_kg' => $weight,
      'height_cm' => $height,
      'muac_cm' => $muac,
      'head_circumference_cm' => $head_circ,
      'zscore_age' => $zscore_age,
      'zscore_length' => $zscore_length,
      'zscore_bmi' => $zscore_bmi,
      'stunting_status' => $stunting,
      'wasting_status' => $wasting,
      'underweight_status' => $underweight,
      'drupal_nid' => $measurement->nid,
    ]);
  }

  /**
   * Export vaccination.
   */
  protected function exportVaccination($measurement, $wrapper, $type, $encounter_id, $child_id, $child_birth, $measured_ts) {
    $vaccine_type = $this->normalizeVaccineType($type);

    // Get administered doses.
    $doses = $this->safeGetMultiFieldValue($wrapper, 'field_administered_doses');
    $notes = $this->safeGetMultiFieldValue($wrapper, 'field_administration_note');

    if (empty($doses)) {
      return;
    }

    // Get schedule for timing calculation.
    $schedule = isset($this->vaccineSchedule[$this->site][$vaccine_type]) ? $this->vaccineSchedule[$this->site][$vaccine_type] : [];

    foreach ($doses as $dose) {
      // Parse dose number from value (e.g., "dose-1" -> 1).
      $dose_num = 1;
      if (preg_match('/dose-?(\d+)/i', $dose, $matches)) {
        $dose_num = (int) $matches[1];
      }
      elseif (is_numeric($dose)) {
        $dose_num = (int) $dose;
      }

      // Calculate timing.
      // For dose 1 (or dose 0): compare child age vs expected age from birth.
      // For dose 2+: compare interval since previous dose vs expected interval.
      $age_days = $this->calculateAgeDays($child_birth, $measured_ts);
      $expected_days = isset($schedule[$dose_num]['expected']) ? $schedule[$dose_num]['expected'] : NULL;
      $grace_period = isset($schedule[$dose_num]['grace']) ? $schedule[$dose_num]['grace'] : 14;

      $days_from_expected = NULL;
      $on_time = NULL;
      $timing_category = NULL;

      $dose_key = "{$child_id}:{$vaccine_type}";
      $prev_dose_num = $dose_num - 1;
      // For OPV, dose 0 is birth dose, so dose 1's previous is dose 0.
      $prev_expected = isset($schedule[$prev_dose_num]['expected']) ? $schedule[$prev_dose_num]['expected'] : NULL;
      $prev_date = isset($this->previousDoseDates[$dose_key][$prev_dose_num]) ? $this->previousDoseDates[$dose_key][$prev_dose_num] : NULL;

      if ($age_days !== NULL && $expected_days !== NULL) {
        if ($prev_date !== NULL && $prev_expected !== NULL) {
          // Dose 2+: measure interval from previous dose.
          $actual_interval = $this->calculateAgeDays($prev_date, $measured_ts);
          $expected_interval = $expected_days - $prev_expected;
          $days_from_expected = $actual_interval - $expected_interval;
        }
        else {
          // First dose of this vaccine: measure from birth.
          $days_from_expected = $age_days - $expected_days;
        }
        $on_time = abs($days_from_expected) <= $grace_period;
        $timing_category = $this->getTimingCategory($days_from_expected, $grace_period);
      }

      // Record this dose date for future interval calculations.
      if (!isset($this->previousDoseDates[$dose_key])) {
        $this->previousDoseDates[$dose_key] = [];
      }
      $this->previousDoseDates[$dose_key][$dose_num] = $measured_ts;

      // Get administration note if available.
      $admin_note = NULL;
      if (!empty($notes)) {
        $admin_note = is_array($notes) ? implode(', ', $notes) : $notes;
      }

      $this->printInsert('fact_vaccination', [
        'child_id' => $child_id,
        'encounter_id' => $encounter_id,
        'vaccine_type' => $vaccine_type,
        'dose_number' => $dose_num,
        'administration_date' => $measured_ts,
        'child_age_days' => $age_days,
        'expected_age_days' => $expected_days,
        'days_from_expected' => $days_from_expected,
        'on_time' => $on_time,
        'timing_category' => $timing_category,
        'administration_note' => $admin_note,
        'drupal_nid' => $measurement->nid,
      ]);
    }
  }

  /**
   * Export ECD assessment.
   */
  protected function exportEcd($measurement, $wrapper, $encounter_id, $child_id, $child_birth, $measured_ts, $ecd_warning) {
    $age_weeks = $this->calculateAgeWeeks($child_birth, $measured_ts);
    $age_months = $this->calculateAgeMonths($child_birth, $measured_ts);

    // Get achieved milestones.
    $signs = $this->safeGetMultiFieldValue($wrapper, 'field_ecd_signs');

    // Filter out "none" value and normalize.
    $milestones = [];
    foreach ($signs as $sign) {
      if ($sign && $sign !== 'none') {
        $milestones[] = $sign;
      }
    }

    $this->printInsert('fact_ecd', [
      'child_id' => $child_id,
      'encounter_id' => $encounter_id,
      'assessment_date' => $measured_ts,
      'child_age_weeks' => $age_weeks,
      'child_age_months' => $age_months,
      'achieved_milestones' => $milestones,
      'achieved_count' => count($milestones),
      'ecd_warning' => $ecd_warning,
      'drupal_nid' => $measurement->nid,
    ]);
  }

  /**
   * Export medication.
   */
  protected function exportMedication($measurement, $wrapper, $type, $encounter_id, $child_id, $child_birth, $measured_ts) {
    // Normalize medication type.
    $med_type = str_replace('well_child_', '', $type);

    // Check if administered.
    $administered = FALSE;
    $non_admin_reason = NULL;

    // Different fields for different medications.
    $admin_field = 'field_' . $med_type;
    $reason_field = 'field_' . $med_type . '_reason';

    $admin_value = $this->safeGetFieldValue($wrapper, $admin_field);
    if ($admin_value) {
      $administered = TRUE;
    }

    $reason = $this->safeGetFieldValue($wrapper, $reason_field);
    if ($reason && !$administered) {
      $non_admin_reason = $reason;
    }

    $age_months = $this->calculateAgeMonths($child_birth, $measured_ts);

    $this->printInsert('fact_medication', [
      'child_id' => $child_id,
      'encounter_id' => $encounter_id,
      'administration_date' => $measured_ts,
      'child_age_months' => $age_months,
      'medication_type' => $med_type,
      'administered' => $administered,
      'non_administration_reason' => $non_admin_reason,
      'drupal_nid' => $measurement->nid,
    ]);
  }

  /**
   * Export vitals as danger signs.
   */
  protected function exportVitals($measurement, $wrapper, $encounter_id, $child_id, $measured_ts) {
    $heart_rate = $this->safeGetFieldValue($wrapper, 'field_heart_rate');
    $respiratory_rate = $this->safeGetFieldValue($wrapper, 'field_respiratory_rate');
    $body_temp = $this->safeGetFieldValue($wrapper, 'field_body_temperature');

    // Only export if we have data.
    if (!$heart_rate && !$respiratory_rate && !$body_temp) {
      return;
    }

    $this->printInsert('fact_danger_signs', [
      'child_id' => $child_id,
      'encounter_id' => $encounter_id,
      'assessment_date' => $measured_ts,
      'has_breathing_problems' => FALSE,
      'has_diarrhea' => FALSE,
      'has_vomiting' => FALSE,
      'has_fever' => $body_temp && $body_temp >= 37.5 ? TRUE : FALSE,
      'has_lethargy' => FALSE,
      'heart_rate' => $heart_rate,
      'respiratory_rate' => $respiratory_rate,
      'body_temperature' => $body_temp,
      'drupal_nid' => $measurement->nid,
    ]);
  }

  /**
   * Export home visit data.
   */
  protected function exportHomeVisitData($measurement, $wrapper, $type, $encounter_id, $child_id, $measured_ts) {
    // Build home visit record incrementally from different measurement types.
    // Use a static cache to combine data from same encounter.
    static $home_visit_cache = [];

    $cache_key = $encounter_id;

    if (!isset($home_visit_cache[$cache_key])) {
      $home_visit_cache[$cache_key] = [
        'child_id' => $child_id,
        'encounter_id' => $encounter_id,
        'visit_date' => $measured_ts,
        'main_water_source' => NULL,
        'water_preparation' => NULL,
        'has_soap' => NULL,
        'wash_hands_before_feeding' => NULL,
        'food_covered' => NULL,
        'main_income_source' => NULL,
        'household_has_food' => NULL,
        'is_breastfeeding' => NULL,
        'receives_supplement' => NULL,
        'supplement_type' => NULL,
        'parents_alive_healthy' => NULL,
        'child_clean' => NULL,
        'caring_option' => NULL,
        'drupal_nid' => $measurement->nid,
        '_complete' => FALSE,
      ];
    }

    $cache = &$home_visit_cache[$cache_key];

    switch ($type) {
      case 'well_child_hygiene':
        $cache['main_water_source'] = $this->safeGetFieldValue($wrapper, 'field_main_water_source');
        $cache['water_preparation'] = $this->safeGetFieldValue($wrapper, 'field_water_preparation_option');
        $signs = $this->safeGetMultiFieldValue($wrapper, 'field_hygiene_signs');
        $cache['has_soap'] = in_array('soap-in-the-house', $signs);
        $cache['wash_hands_before_feeding'] = in_array('wash-hands-before-feeding', $signs);
        $cache['food_covered'] = in_array('food-is-covered', $signs);
        break;

      case 'well_child_food_security':
        $cache['main_income_source'] = $this->safeGetFieldValue($wrapper, 'field_main_income_source');
        $signs = $this->safeGetMultiFieldValue($wrapper, 'field_food_security_signs');
        $cache['household_has_food'] = in_array('household-got-food', $signs);
        break;

      case 'well_child_feeding':
        $signs = $this->safeGetMultiFieldValue($wrapper, 'field_nutrition_feeding_signs');
        $cache['is_breastfeeding'] = in_array('receive-breast-milk', $signs) || in_array('breastfed-only', $signs);
        $cache['receives_supplement'] = in_array('supplementary-food', $signs);
        $cache['supplement_type'] = $this->safeGetFieldValue($wrapper, 'field_supplement_type');
        break;

      case 'well_child_caring':
        $signs = $this->safeGetMultiFieldValue($wrapper, 'field_caring_signs');
        $cache['parents_alive_healthy'] = in_array('parents-alive-healthy', $signs);
        $cache['child_clean'] = in_array('child-clean', $signs);
        $cache['caring_option'] = $this->safeGetFieldValue($wrapper, 'field_caring_option');
        break;
    }

    // Check if we have enough data to export (at least one meaningful field).
    $has_data = $cache['main_water_source'] || $cache['main_income_source'] ||
                $cache['is_breastfeeding'] !== NULL || $cache['caring_option'];

    if ($has_data && !$cache['_complete']) {
      $cache['_complete'] = TRUE;

      $this->printInsert('fact_home_visit', [
        'child_id' => $cache['child_id'],
        'encounter_id' => $cache['encounter_id'],
        'visit_date' => $cache['visit_date'],
        'main_water_source' => $cache['main_water_source'],
        'water_preparation' => $cache['water_preparation'],
        'has_soap' => $cache['has_soap'],
        'wash_hands_before_feeding' => $cache['wash_hands_before_feeding'],
        'food_covered' => $cache['food_covered'],
        'main_income_source' => $cache['main_income_source'],
        'household_has_food' => $cache['household_has_food'],
        'is_breastfeeding' => $cache['is_breastfeeding'],
        'receives_supplement' => $cache['receives_supplement'],
        'supplement_type' => $cache['supplement_type'],
        'parents_alive_healthy' => $cache['parents_alive_healthy'],
        'child_clean' => $cache['child_clean'],
        'caring_option' => $cache['caring_option'],
        'drupal_nid' => $cache['drupal_nid'],
      ]);
    }
  }

  /**
   * Export danger signs from symptoms review.
   */
  protected function exportDangerSigns($measurement, $wrapper, $encounter_id, $child_id, $measured_ts) {
    $symptoms = $this->safeGetMultiFieldValue($wrapper, 'field_symptoms_review_signs');

    if (empty($symptoms)) {
      return;
    }

    $this->printInsert('fact_danger_signs', [
      'child_id' => $child_id,
      'encounter_id' => $encounter_id,
      'assessment_date' => $measured_ts,
      'has_breathing_problems' => in_array('breathing-problems', $symptoms) || in_array('difficulty-breathing', $symptoms),
      'has_diarrhea' => in_array('diarrhea', $symptoms),
      'has_vomiting' => in_array('vomiting', $symptoms),
      'has_fever' => in_array('fever', $symptoms),
      'has_lethargy' => in_array('lethargy', $symptoms) || in_array('abnormally-sleepy', $symptoms),
      'heart_rate' => NULL,
      'respiratory_rate' => NULL,
      'body_temperature' => NULL,
      'drupal_nid' => $measurement->nid,
    ]);
  }

}

// ============================================================================
// Main execution
// ============================================================================
// Only run full export when this script is called directly (not via require).
if (basename($_SERVER['argv'][1] ?? '') === basename(__FILE__)) {
  $config = [
    'batch_size' => drush_get_option('batch', 50),
    'memory_limit' => drush_get_option('memory_limit', 800),
    'site' => drush_get_option('site', 'rwanda'),
  ];

  $exporter = new HedleyMigrateWellChildResearchExporter('well_child_encounter', $config);
  $exporter->exportToSql();
}
