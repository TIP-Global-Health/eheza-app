<?php

/**
 * @file
 * Delta (incremental) exporter for well-child research data.
 *
 * Extends the well-child exporter to export only changed entities
 * since the last sync, using Drupal's revision IDs (vid) for change detection.
 *
 * Output: DELETE+INSERT SQL wrapped in a transaction (no DDL).
 *
 * Usage:
 *   drush scr export-delta.php --since-vid=12345 --site=rwanda > delta.sql
 */

/**
 * Delta exporter — exports only entities changed since a given revision ID.
 */
class HedleyMigrateDeltaExporter extends HedleyMigrateWellChildResearchExporter {

  /**
   * Last synced revision ID.
   *
   * @var int
   */
  protected $lastVid = 0;

  /**
   * Maximum revision ID seen during this export.
   *
   * @var int
   */
  protected $maxVid = 0;

  /**
   * NIDs of changed encounters (direct or via measurement changes).
   *
   * @var array
   */
  protected $changedEncounterNids = [];

  /**
   * NIDs of changed child (person) nodes.
   *
   * @var array
   */
  protected $changedChildNids = [];

  /**
   * NIDs of changed health center nodes.
   *
   * @var array
   */
  protected $changedHcNids = [];

  /**
   * NIDs of deleted (unpublished) encounters.
   *
   * @var array
   */
  protected $deletedEncounterNids = [];

  /**
   * Fact tables that reference encounter_id, in DELETE order.
   *
   * @var array
   */
  protected $encounterFactTables = [
    'fact_danger_signs',
    'fact_medication',
    'fact_home_visit',
    'fact_ecd',
    'fact_growth',
    'fact_vaccination',
    'fact_encounter',
  ];

  /**
   * All measurement node types that reference encounters.
   *
   * @var array
   */
  protected $measurementTypes = [
    'well_child_height',
    'well_child_weight',
    'well_child_muac',
    'well_child_head_circumference',
    'well_child_bcg_immunisation',
    'well_child_dtp_immunisation',
    'well_child_opv_immunisation',
    'well_child_ipv_immunisation',
    'well_child_mr_immunisation',
    'well_child_pcv13_immunisation',
    'well_child_rotarix_immunisation',
    'well_child_ecd',
    'well_child_albendazole',
    'well_child_mebendezole',
    'well_child_vitamin_a',
    'well_child_vitals',
    'well_child_hygiene',
    'well_child_food_security',
    'well_child_feeding',
    'well_child_caring',
    'well_child_symptoms_review',
    'well_child_pregnancy_summary',
    'well_child_ncda',
    'well_child_next_visit',
    'well_child_nutrition',
    'well_child_follow_up',
    'well_child_contributing_factors',
    'well_child_send_to_hc',
    'well_child_health_education',
    'well_child_photo',
  ];

  /**
   * Run the delta export.
   *
   * @param int $last_vid
   *   Process revisions with vid > this value.
   */
  public function exportDelta($last_vid) {
    $this->lastVid = (int) $last_vid;

    // Capture max vid before we start (watermark for next run).
    $this->maxVid = (int) db_query("SELECT MAX(vid) FROM {node_revision}")->fetchField();

    drush_print("-- ============================================================================");
    drush_print("-- E-Heza Delta Export");
    drush_print("-- Generated: " . date('Y-m-d H:i:s'));
    drush_print("-- Site: " . $this->site);
    drush_print("-- Since vid: " . $this->lastVid);
    drush_print("-- max_vid: " . $this->maxVid);
    drush_print("-- ============================================================================");
    drush_print("");

    // Detect what changed.
    $this->detectChanges();
    $this->detectDeletions();

    $total_changes = count($this->changedHcNids) + count($this->changedChildNids)
      + count($this->changedEncounterNids) + count($this->deletedEncounterNids);

    drush_print("-- Changes detected:");
    drush_print("--   Health centers: " . count($this->changedHcNids));
    drush_print("--   Children: " . count($this->changedChildNids));
    drush_print("--   Encounters (new/modified): " . count($this->changedEncounterNids));
    drush_print("--   Encounters (deleted): " . count($this->deletedEncounterNids));
    drush_print("--   Total: " . $total_changes);
    drush_print("");

    if ($total_changes == 0) {
      drush_print("-- No changes detected. Empty delta.");
      drush_print("BEGIN;");
      drush_print("COMMIT;");
      drush_print("-- encounters_processed: 0");
      drush_print("-- children_processed: 0");
      return;
    }

    drush_print("BEGIN;");
    drush_print("");

    // Process in dependency order.
    $this->exportChangedHealthCenters();
    $this->exportChangedChildren();
    $this->processDeletedEncounters();
    $this->exportChangedEncounters();

    // Flush any remaining buffered inserts.
    $this->flushAllInserts();

    drush_print("");
    drush_print("COMMIT;");
    drush_print("");
    drush_print("-- encounters_processed: " . count($this->changedEncounterNids));
    drush_print("-- children_processed: " . count($this->changedChildNids));
    drush_print("-- Delta export complete at: " . date('Y-m-d H:i:s'));
  }

  /**
   * Detect changed entities using node_revision.vid.
   */
  protected function detectChanges() {
    // Get max vid for the metadata comment (already done in exportDelta).
    // Find all nodes with new revisions since last_vid.
    $query = db_query("
      SELECT DISTINCT n.nid, n.type
      FROM {node_revision} r
      JOIN {node} n ON n.nid = r.nid
      WHERE r.vid > :last_vid
        AND n.status = :published
    ", [
      ':last_vid' => $this->lastVid,
      ':published' => NODE_PUBLISHED,
    ]);

    $changed_by_type = [];
    foreach ($query as $row) {
      $changed_by_type[$row->type][] = $row->nid;
    }

    // Health centers.
    $this->changedHcNids = isset($changed_by_type['health_center']) ? $changed_by_type['health_center'] : [];

    // Children (person nodes).
    if (isset($changed_by_type['person'])) {
      // Filter to only persons that are children in well-child encounters.
      $this->changedChildNids = $this->filterToWellChildChildren($changed_by_type['person']);
    }

    // Encounters — changed directly.
    $encounter_nids = isset($changed_by_type['well_child_encounter']) ? $changed_by_type['well_child_encounter'] : [];

    // Encounters — changed via measurement modifications.
    $measurement_changed_types = array_intersect_key($changed_by_type, array_flip($this->measurementTypes));
    foreach ($measurement_changed_types as $nids) {
      $parent_encounters = $this->resolveParentEncounters($nids);
      $encounter_nids = array_merge($encounter_nids, $parent_encounters);
    }

    $this->changedEncounterNids = array_unique($encounter_nids);
  }

  /**
   * Detect deleted (unpublished) encounters.
   */
  protected function detectDeletions() {
    $query = db_query("
      SELECT DISTINCT n.nid
      FROM {node_revision} r
      JOIN {node} n ON n.nid = r.nid
      WHERE r.vid > :last_vid
        AND n.type = 'well_child_encounter'
        AND n.status = :unpublished
    ", [
      ':last_vid' => $this->lastVid,
      ':unpublished' => NODE_NOT_PUBLISHED,
    ]);

    $this->deletedEncounterNids = $query->fetchCol();

    // Also check for soft-deleted encounters (field_deleted = TRUE).
    $soft_deleted = db_query("
      SELECT DISTINCT n.nid
      FROM {node_revision} r
      JOIN {node} n ON n.nid = r.nid
      JOIN {field_data_field_deleted} d ON d.entity_id = n.nid AND d.field_deleted_value = 1
      WHERE r.vid > :last_vid
        AND n.type = 'well_child_encounter'
    ", [':last_vid' => $this->lastVid])->fetchCol();

    $this->deletedEncounterNids = array_unique(array_merge(
      $this->deletedEncounterNids,
      $soft_deleted
    ));

    // Remove deleted encounters from the changed list (no need to re-export).
    $this->changedEncounterNids = array_diff(
      $this->changedEncounterNids,
      $this->deletedEncounterNids
    );
  }

  /**
   * Filter person NIDs to only well-child children.
   *
   * Only returns persons that appear as children in well-child encounters.
   *
   * @param array $person_nids
   *   Person node IDs to filter.
   *
   * @return array
   *   Filtered list of person NIDs that appear as children in well-child
   *   encounters.
   */
  protected function filterToWellChildChildren(array $person_nids) {
    if (empty($person_nids)) {
      return [];
    }

    $query = db_select('field_data_field_person', 'p');
    $query->join('field_data_field_individual_participant', 'part', 'part.field_individual_participant_target_id = p.entity_id');
    $query->join('node', 'n', 'n.nid = part.entity_id');
    $query->condition('n.type', 'well_child_encounter');
    $query->condition('p.field_person_target_id', $person_nids, 'IN');
    $query->fields('p', ['field_person_target_id']);
    $query->distinct();

    return $query->execute()->fetchCol();
  }

  /**
   * Resolve measurement NIDs to their parent encounter NIDs.
   *
   * @param array $measurement_nids
   *   Measurement node IDs.
   *
   * @return array
   *   Parent encounter node IDs.
   */
  protected function resolveParentEncounters(array $measurement_nids) {
    if (empty($measurement_nids)) {
      return [];
    }

    $query = db_select('field_data_field_well_child_encounter', 'e');
    $query->condition('e.entity_id', $measurement_nids, 'IN');
    $query->fields('e', ['field_well_child_encounter_target_id']);
    $query->distinct();

    return $query->execute()->fetchCol();
  }

  /**
   * Export changed health centers (DELETE + INSERT).
   */
  protected function exportChangedHealthCenters() {
    if (empty($this->changedHcNids)) {
      return;
    }

    drush_print("-- Exporting " . count($this->changedHcNids) . " changed health centers...");

    $health_centers = node_load_multiple($this->changedHcNids);

    foreach ($health_centers as $hc) {
      $wrapper = entity_metadata_wrapper('node', $hc);

      if ($this->safeGetFieldValue($wrapper, 'field_deleted')) {
        // Deleted health center — just emit DELETE.
        drush_print("DELETE FROM dim_health_center WHERE health_center_id = " . (int) $hc->nid . ";");
        continue;
      }

      // DELETE then INSERT.
      drush_print("DELETE FROM dim_health_center WHERE health_center_id = " . (int) $hc->nid . ";");

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

    $this->flushAllInserts();
    entity_get_controller('node')->resetCache(array_keys($health_centers));
    drush_print("");
  }

  /**
   * Export changed children (DELETE + INSERT for dim_child only).
   */
  protected function exportChangedChildren() {
    if (empty($this->changedChildNids)) {
      return;
    }

    drush_print("-- Exporting " . count($this->changedChildNids) . " changed children...");

    // We need all health centers loaded for the wasExported check.
    // Load any health centers not already in memory.
    $this->ensureHealthCentersLoaded();

    $batches = array_chunk($this->changedChildNids, 500);

    foreach ($batches as $batch_ids) {
      $children = node_load_multiple($batch_ids);

      foreach ($children as $child) {
        $wrapper = entity_metadata_wrapper('node', $child);

        if ($this->safeGetFieldValue($wrapper, 'field_deleted')) {
          drush_print("DELETE FROM dim_child WHERE child_id = " . (int) $child->nid . ";");
          continue;
        }

        // DELETE then re-INSERT.
        drush_print("DELETE FROM dim_child WHERE child_id = " . (int) $child->nid . ";");

        // Reuse parent's child export logic.
        $this->exportSingleChild($child, $wrapper);
      }

      entity_get_controller('node')->resetCache(array_keys($children));
    }

    $this->flushAllInserts();
    drush_print("");
  }

  /**
   * Export a single child record.
   *
   * Extracted from parent's exportChildren() loop body.
   */
  protected function exportSingleChild($child, $wrapper) {
    $birth_date = $this->safeGetFieldValue($wrapper, 'field_birth_date');
    $birth_ts = $this->convertDateToTimestamp($birth_date);
    $gender = $this->safeGetFieldValue($wrapper, 'field_gender') ?: 'unknown';

    $hc_id = NULL;
    $hc_ref = $this->safeGetFieldValue($wrapper, 'field_health_center');
    if ($hc_ref) {
      $hc_id = is_object($hc_ref) ? $hc_ref->nid : $hc_ref;
      if (!$this->wasExported('health_center', $hc_id)) {
        $hc_id = NULL;
      }
    }

    $province = $this->safeGetFieldValue($wrapper, 'field_province');
    $district = $this->safeGetFieldValue($wrapper, 'field_district');
    $sector = $this->safeGetFieldValue($wrapper, 'field_sector');
    $cell = $this->safeGetFieldValue($wrapper, 'field_cell');
    $village = $this->safeGetFieldValue($wrapper, 'field_village');

    // Caregiver data.
    $caregiver_data = $this->getCaregiverData($child->nid);

    // Match parent's column names exactly.
    $hiv_status = $this->safeGetFieldValue($wrapper, 'field_hiv_status');

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
  }

  /**
   * Process deleted encounters — emit cascading DELETEs.
   */
  protected function processDeletedEncounters() {
    if (empty($this->deletedEncounterNids)) {
      return;
    }

    drush_print("-- Deleting " . count($this->deletedEncounterNids) . " encounters...");

    foreach ($this->deletedEncounterNids as $enc_nid) {
      $this->emitEncounterDelete((int) $enc_nid);
    }

    drush_print("");
  }

  /**
   * Export changed encounters (DELETE + INSERT).
   */
  protected function exportChangedEncounters() {
    if (empty($this->changedEncounterNids)) {
      return;
    }

    drush_print("-- Exporting " . count($this->changedEncounterNids) . " changed encounters...");

    // Ensure all referenced children and health centers are loaded.
    $this->ensureHealthCentersLoaded();
    $this->ensureChildrenLoaded($this->changedEncounterNids);

    $batches = array_chunk($this->changedEncounterNids, $this->batchSize);
    $processed = 0;

    foreach ($batches as $batch_nids) {
      $encounters = node_load_multiple($batch_nids);

      foreach ($encounters as $encounter) {
        // DELETE all existing fact data for this encounter.
        $this->emitEncounterDelete((int) $encounter->nid);

        // Pre-load vaccine dose history for the child in this encounter.
        $child_id = $this->getChildIdFromEncounter($encounter);
        if ($child_id) {
          $this->preloadVaccineDoseHistory($child_id);
        }

        // Re-INSERT using parent's logic.
        $this->exportSingleEncounter($encounter);
        $processed++;
      }

      entity_get_controller('node')->resetCache(array_keys($encounters));
      unset($encounters);
    }

    $this->flushAllInserts();
    drush_print("-- Exported $processed encounters");
    drush_print("");
  }

  /**
   * Emit DELETE statements for all fact tables referencing an encounter.
   *
   * @param int $encounter_nid
   *   The encounter node ID.
   */
  protected function emitEncounterDelete($encounter_nid) {
    foreach ($this->encounterFactTables as $table) {
      drush_print("DELETE FROM {$table} WHERE encounter_id = {$encounter_nid};");
    }
  }

  /**
   * Ensure all health centers are loaded into memory.
   *
   * For delta, we need the health center cache populated even if no HCs
   * changed.
   */
  protected function ensureHealthCentersLoaded() {
    if (!empty($this->healthCenters)) {
      return;
    }

    // Load all health centers (small table, ~50 records).
    $query = db_select('node', 'n');
    $query->condition('n.type', 'health_center');
    $query->condition('n.status', NODE_PUBLISHED);
    $query->fields('n', ['nid']);
    $hc_ids = $query->execute()->fetchCol();

    $health_centers = node_load_multiple($hc_ids);
    foreach ($health_centers as $hc) {
      $wrapper = entity_metadata_wrapper('node', $hc);
      if ($this->safeGetFieldValue($wrapper, 'field_deleted')) {
        continue;
      }
      $this->markExported('health_center', $hc->nid);
      $this->healthCenters[$hc->nid] = [
        'name' => $hc->title,
        'province' => $this->safeGetFieldValue($wrapper, 'field_province'),
        'district' => $this->safeGetFieldValue($wrapper, 'field_district'),
      ];
    }
    entity_get_controller('node')->resetCache(array_keys($health_centers));
  }

  /**
   * Ensure children referenced by encounters are loaded into child_cache.
   *
   * @param array $encounter_nids
   *   Encounter node IDs.
   */
  protected function ensureChildrenLoaded(array $encounter_nids) {
    if (empty($encounter_nids)) {
      return;
    }

    // Find child IDs for these encounters.
    $query = db_select('node', 'n');
    $query->join('field_data_field_individual_participant', 'part', 'part.entity_id = n.nid');
    $query->join('field_data_field_person', 'p', 'p.entity_id = part.field_individual_participant_target_id');
    $query->condition('n.nid', $encounter_nids, 'IN');
    $query->fields('p', ['field_person_target_id']);
    $query->distinct();
    $child_ids = $query->execute()->fetchCol();

    // Filter to children not already in cache.
    $missing = [];
    foreach ($child_ids as $cid) {
      if (!isset($this->childCache[$cid]) && !$this->wasExported('child', $cid)) {
        $missing[] = $cid;
      }
    }

    if (empty($missing)) {
      // All children already in cache — just mark them as exported.
      foreach ($child_ids as $cid) {
        $this->markExported('child', $cid);
      }
      return;
    }

    // Load and cache missing children (but don't INSERT them — they're in
    // the DB already).
    $batches = array_chunk($missing, 500);
    foreach ($batches as $batch_ids) {
      $children = node_load_multiple($batch_ids);
      foreach ($children as $child) {
        $wrapper = entity_metadata_wrapper('node', $child);
        $birth_date = $this->safeGetFieldValue($wrapper, 'field_birth_date');
        $birth_ts = $this->convertDateToTimestamp($birth_date);
        $gender = $this->safeGetFieldValue($wrapper, 'field_gender') ?: 'unknown';

        $this->markExported('child', $child->nid);
        $this->childCache[$child->nid] = [
          'birth_date' => $birth_ts,
          'gender' => $gender,
        ];
      }
      entity_get_controller('node')->resetCache(array_keys($children));
    }

    // Also mark previously cached children.
    foreach ($child_ids as $cid) {
      $this->markExported('child', $cid);
    }
  }

  /**
   * Get child ID from an encounter node.
   *
   * @param object $encounter
   *   Encounter node.
   *
   * @return int|null
   *   Child node ID or NULL.
   */
  protected function getChildIdFromEncounter($encounter) {
    $wrapper = entity_metadata_wrapper('node', $encounter);
    $participant = $this->safeGetFieldValue($wrapper, 'field_individual_participant');
    if (!$participant) {
      return NULL;
    }
    $participant_wrapper = entity_metadata_wrapper('node', $participant);
    $child_ref = $this->safeGetFieldValue($participant_wrapper, 'field_person');
    if (!$child_ref) {
      return NULL;
    }
    return is_object($child_ref) ? $child_ref->nid : $child_ref;
  }

  /**
   * Pre-load vaccine dose history for a child from Drupal.
   *
   * This populates $this->previousDoseDates so that interval-based
   * timing calculations work correctly for delta exports.
   *
   * @param int $child_id
   *   Child node ID.
   */
  protected function preloadVaccineDoseHistory($child_id) {
    // Find all vaccination measurement nodes for this child.
    $vaccine_types = [
      'well_child_bcg_immunisation',
      'well_child_dtp_immunisation',
      'well_child_dtp_sa_immunisation',
      'well_child_opv_immunisation',
      'well_child_ipv_immunisation',
      'well_child_mr_immunisation',
      'well_child_pcv13_immunisation',
      'well_child_rotarix_immunisation',
      'well_child_hpv_immunisation',
    ];

    // Get all encounters for this child, ordered by date.
    $query = db_select('node', 'enc');
    $query->join('field_data_field_individual_participant', 'part', 'part.entity_id = enc.nid');
    $query->join('field_data_field_person', 'p', 'p.entity_id = part.field_individual_participant_target_id');
    $query->condition('p.field_person_target_id', $child_id);
    $query->condition('enc.type', 'well_child_encounter');
    $query->condition('enc.status', NODE_PUBLISHED);
    $query->fields('enc', ['nid']);
    $enc_nids = $query->execute()->fetchCol();

    if (empty($enc_nids)) {
      return;
    }

    // Find vaccination measurements for these encounters.
    $query = db_select('node', 'n');
    $query->join('field_data_field_well_child_encounter', 'e', 'e.entity_id = n.nid');
    $query->join('field_data_field_date_measured', 'dm', 'dm.entity_id = n.nid');
    $query->condition('n.type', $vaccine_types, 'IN');
    $query->condition('n.status', NODE_PUBLISHED);
    $query->condition('e.field_well_child_encounter_target_id', $enc_nids, 'IN');
    $query->fields('n', ['nid', 'type']);
    $query->addField('dm', 'field_date_measured_value', 'date_measured');
    $query->orderBy('dm.field_date_measured_value', 'ASC');

    $results = $query->execute();

    foreach ($results as $row) {
      $vaccine_type = $this->normalizeVaccineType($row->type);
      $measured_ts = $this->convertDateToTimestamp($row->date_measured);
      if (!$measured_ts) {
        continue;
      }

      // Load the node to get administered doses.
      $node = node_load($row->nid);
      if (!$node) {
        continue;
      }
      $wrapper = entity_metadata_wrapper('node', $node);
      $doses = $this->safeGetMultiFieldValue($wrapper, 'field_administered_doses');
      if (empty($doses)) {
        continue;
      }

      foreach ($doses as $dose) {
        $dose_num = 1;
        if (preg_match('/dose-?(\d+)/i', $dose, $matches)) {
          $dose_num = (int) $matches[1];
        }
        elseif (is_numeric($dose)) {
          $dose_num = (int) $dose;
        }

        $dose_key = "{$child_id}:{$vaccine_type}";
        if (!isset($this->previousDoseDates[$dose_key])) {
          $this->previousDoseDates[$dose_key] = [];
        }
        $this->previousDoseDates[$dose_key][$dose_num] = $measured_ts;
      }
    }
  }

  /**
   * Override: no DDL output for delta exports.
   */
  protected function printDdl() {
    // No-op.
  }

  /**
   * Override: do not use the full export flow.
   *
   * Delta uses exportDelta() instead.
   */
  protected function exportData() {
    // No-op — delta uses exportDelta() as entry point.
  }

}
