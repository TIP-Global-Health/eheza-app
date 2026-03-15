<?php

/**
 * @file
 * Generic entity export framework for SQL normalization.
 *
 * This framework provides a reusable base class for exporting Drupal entities
 * to normalized SQL tables. Based on the original framework with enhancements
 * for the revised research schema.
 *
 * Key features:
 * - Schema definition and DDL generation
 * - SQL escaping and value formatting
 * - Batch processing with memory management
 * - Array type support for PostgreSQL
 * - Relationship traversal helpers
 */

/**
 * Base class for exporting Drupal entities to normalized SQL.
 */
abstract class HedleyMigrateEntityExporter {

  /**
   * The entity type being exported.
   *
   * @var string
   */
  protected $entityType;

  /**
   * Schema configuration for all tables.
   *
   * @var array
   */
  protected $tables = [];

  /**
   * Number of entities to process per batch.
   *
   * @var int
   */
  protected $batchSize = 50;

  /**
   * Memory limit in MB before stopping.
   *
   * @var int
   */
  protected $memoryLimit = 800;

  /**
   * Track exported entity IDs to prevent orphaned references.
   *
   * @var array
   */
  protected $exportedIds = [];

  /**
   * Buffer for multi-row INSERT statements, keyed by table name.
   *
   * @var array
   */
  protected $insertBuffer = [];

  /**
   * Number of rows per multi-row INSERT statement.
   *
   * @var int
   */
  protected $insertBatchSize = 500;

  /**
   * Site identifier (rwanda or burundi).
   *
   * @var string
   */
  protected $site = 'rwanda';

  /**
   * Constructor.
   *
   * @param string $entityType
   *   The entity type being exported.
   * @param array $config
   *   Optional configuration array.
   */
  public function __construct($entityType, array $config = []) {
    $this->entityType = $entityType;
    $this->batchSize = isset($config['batch_size']) ? $config['batch_size'] : 50;
    $this->memoryLimit = isset($config['memory_limit']) ? $config['memory_limit'] : 800;
    $this->site = isset($config['site']) ? $config['site'] : 'rwanda';
    $this->tables = $this->getSchemaConfig();
  }

  /**
   * Main export method - generates complete SQL output.
   */
  public function exportToSql() {
    $this->printHeader();
    $this->printDdl();
    $this->exportData();
    $this->flushAllInserts();
    $this->printFooter();
  }

  /**
   * Print SQL header with metadata.
   */
  protected function printHeader() {
    drush_print("-- ============================================================================");
    drush_print("-- E-Heza Research Data Export");
    drush_print("-- Generated: " . date('Y-m-d H:i:s'));
    drush_print("-- Site: " . $this->site);
    drush_print("-- ============================================================================");
    drush_print("");
    drush_print("BEGIN;");
    drush_print("");
  }

  /**
   * Print SQL footer.
   */
  protected function printFooter() {
    drush_print("");
    drush_print("COMMIT;");
    drush_print("");
    drush_print("-- Export complete at: " . date('Y-m-d H:i:s'));
  }

  /**
   * Outputs all DDL statements.
   */
  protected function printDdl() {
    foreach ($this->tables as $table_name => $schema) {
      $this->printDropTable($table_name);
      $this->printCreateTable($table_name, $schema);
      $this->printIndexes($table_name, $schema);
    }
  }

  /**
   * Outputs a DROP TABLE statement.
   */
  protected function printDropTable($table_name) {
    drush_print("DROP TABLE IF EXISTS {$table_name} CASCADE;");
    drush_print("");
  }

  /**
   * Outputs a CREATE TABLE statement.
   */
  protected function printCreateTable($table_name, array $schema) {
    drush_print("CREATE TABLE {$table_name} (");

    $columns = [];
    foreach ($schema['columns'] as $name => $definition) {
      $columns[] = "  " . $this->formatColumn($name, $definition);
    }

    // Add foreign keys if they exist.
    if (!empty($schema['foreign_keys'])) {
      foreach ($schema['foreign_keys'] as $fk) {
        $columns[] = "  FOREIGN KEY ({$fk['column']}) REFERENCES {$fk['table']}({$fk['ref_column']})";
      }
    }

    drush_print(implode(",\n", $columns));
    drush_print(");");
    drush_print("");
  }

  /**
   * Outputs CREATE INDEX statements.
   */
  protected function printIndexes($table_name, array $schema) {
    if (!empty($schema['indexes'])) {
      foreach ($schema['indexes'] as $idx_name => $idx_columns) {
        $columns_str = is_array($idx_columns) ? implode(', ', $idx_columns) : $idx_columns;
        drush_print("CREATE INDEX {$idx_name} ON {$table_name}({$columns_str});");
      }
      drush_print("");
    }
  }

  /**
   * Formats a column definition for CREATE TABLE.
   */
  protected function formatColumn($name, array $definition) {
    $type = isset($definition['type']) ? $definition['type'] : 'TEXT';
    $constraint = isset($definition['constraint']) ? ' ' . $definition['constraint'] : '';
    $default = '';

    if (isset($definition['default'])) {
      $default_value = $definition['default'];
      if ($default_value === 'NOW()' || $default_value === 'CURRENT_TIMESTAMP') {
        $default = ' DEFAULT CURRENT_TIMESTAMP';
      }
      elseif ($default_value === 'FALSE') {
        $default = ' DEFAULT FALSE';
      }
      elseif ($default_value === 'TRUE') {
        $default = ' DEFAULT TRUE';
      }
      else {
        $default = ' DEFAULT ' . $this->formatValue($default_value, $type);
      }
    }

    return "{$name} {$type}{$constraint}{$default}";
  }

  /**
   * Buffers a row for batch INSERT output.
   *
   * Rows are accumulated per table and flushed as multi-row INSERT
   * statements every $insertBatchSize rows.
   */
  protected function printInsert($table_name, array $values) {
    if (empty($values)) {
      return;
    }

    $columns = array_keys($values);

    $value_list = [];
    foreach ($columns as $column) {
      $value = $values[$column];
      $column_def = $this->getColumnDefinition($table_name, $column);
      $type = isset($column_def['type']) ? $column_def['type'] : 'TEXT';
      $value_list[] = $this->formatValue($value, $type);
    }

    $row_str = '(' . implode(', ', $value_list) . ')';

    if (!isset($this->insertBuffer[$table_name])) {
      $this->insertBuffer[$table_name] = [
        'columns' => $columns,
        'rows' => [],
      ];
    }

    $this->insertBuffer[$table_name]['rows'][] = $row_str;

    if (count($this->insertBuffer[$table_name]['rows']) >= $this->insertBatchSize) {
      $this->flushInserts($table_name);
    }
  }

  /**
   * Flushes buffered INSERT rows for a specific table.
   */
  protected function flushInserts($table_name) {
    if (empty($this->insertBuffer[$table_name]['rows'])) {
      return;
    }

    $column_list = implode(', ', $this->insertBuffer[$table_name]['columns']);
    $rows = $this->insertBuffer[$table_name]['rows'];

    drush_print("INSERT INTO {$table_name} ({$column_list}) VALUES");
    drush_print(implode(",\n", $rows) . ";");

    $this->insertBuffer[$table_name]['rows'] = [];
  }

  /**
   * Flushes all buffered INSERT rows for all tables.
   */
  protected function flushAllInserts() {
    foreach (array_keys($this->insertBuffer) as $table_name) {
      $this->flushInserts($table_name);
    }
  }

  /**
   * Formats a value for SQL insertion.
   */
  protected function formatValue($value, $type) {
    // Handle NULL values.
    if (is_null($value) || $value === '') {
      return 'NULL';
    }

    // Handle array type for PostgreSQL.
    if (strpos($type, '[]') !== FALSE || $type === 'TEXT[]') {
      return $this->formatArrayValue($value);
    }

    switch ($type) {
      case 'INTEGER':
      case 'BIGINT':
      case 'SERIAL':
      case 'BIGSERIAL':
      case 'SMALLINT':
        return (int) $value;

      case 'FLOAT':
      case 'DOUBLE PRECISION':
      case 'DECIMAL':
      case 'DECIMAL(5,2)':
      case 'DECIMAL(4,2)':
      case 'DECIMAL(6,1)':
      case 'DECIMAL(3,1)':
      case 'DECIMAL(10,7)':
        return is_numeric($value) ? (float) $value : 'NULL';

      case 'BOOLEAN':
        if ($value === TRUE || $value === 1 || $value === '1' || strtolower($value) === 'true') {
          return 'TRUE';
        }
        return 'FALSE';

      case 'TIMESTAMP':
      case 'DATE':
        if (is_numeric($value)) {
          $format = ($type === 'DATE') ? 'Y-m-d' : 'Y-m-d H:i:s';
          return "'" . date($format, $value) . "'";
        }
        return "'" . $this->escapeSqlString($value) . "'";

      case 'TEXT':
      case 'VARCHAR':
      case 'JSONB':
      default:
        // Handle VARCHAR with length specification.
        if (strpos($type, 'VARCHAR') === 0) {
          return "'" . $this->escapeSqlString($value) . "'";
        }
        return "'" . $this->escapeSqlString($value) . "'";
    }
  }

  /**
   * Formats an array value for PostgreSQL.
   */
  protected function formatArrayValue($value) {
    if (empty($value)) {
      return "'{}'";
    }

    if (!is_array($value)) {
      $value = [$value];
    }

    $escaped = array_map(function ($item) {
      // Escape quotes and backslashes for PostgreSQL array format.
      $item = str_replace('\\', '\\\\', $item);
      $item = str_replace('"', '\\"', $item);
      return '"' . $item . '"';
    }, $value);

    return "'{" . implode(',', $escaped) . "}'";
  }

  /**
   * Escapes a string for safe SQL insertion.
   */
  protected function escapeSqlString($value) {
    $value = (string) $value;
    $value = str_replace("'", "''", $value);
    $value = str_replace("\\", "\\\\", $value);
    return $value;
  }

  /**
   * Gets the column definition for a specific column.
   */
  protected function getColumnDefinition($table_name, $column_name) {
    if (!isset($this->tables[$table_name]['columns'][$column_name])) {
      return [];
    }
    return $this->tables[$table_name]['columns'][$column_name];
  }

  /**
   * Converts a Drupal date field value to a timestamp.
   */
  protected function convertDateToTimestamp($date_value) {
    if (is_numeric($date_value)) {
      return (int) $date_value;
    }

    if (is_array($date_value) && isset($date_value['value'])) {
      $value = $date_value['value'];
      if (is_numeric($value)) {
        return (int) $value;
      }
      return strtotime($value) ?: NULL;
    }

    if (is_string($date_value)) {
      return strtotime($date_value) ?: NULL;
    }

    return NULL;
  }

  /**
   * Safely get a field value from an entity wrapper.
   */
  protected function safeGetFieldValue($wrapper, $field_name) {
    try {
      $info = $wrapper->getPropertyInfo();
      if (!isset($info[$field_name])) {
        return NULL;
      }
      return $wrapper->{$field_name}->value();
    }
    catch (Exception $e) {
      return NULL;
    }
  }

  /**
   * Safely get a multi-value field as array.
   */
  protected function safeGetMultiFieldValue($wrapper, $field_name) {
    try {
      $info = $wrapper->getPropertyInfo();
      if (!isset($info[$field_name])) {
        return [];
      }
      $values = $wrapper->{$field_name}->value();
      return is_array($values) ? $values : [$values];
    }
    catch (Exception $e) {
      return [];
    }
  }

  /**
   * Calculate age in days between two dates.
   */
  protected function calculateAgeDays($birth_date, $event_date) {
    if (!$birth_date || !$event_date) {
      return NULL;
    }
    $birth_ts = is_numeric($birth_date) ? $birth_date : strtotime($birth_date);
    $event_ts = is_numeric($event_date) ? $event_date : strtotime($event_date);

    if (!$birth_ts || !$event_ts) {
      return NULL;
    }

    return (int) floor(($event_ts - $birth_ts) / 86400);
  }

  /**
   * Calculate age in weeks.
   */
  protected function calculateAgeWeeks($birth_date, $event_date) {
    $days = $this->calculateAgeDays($birth_date, $event_date);
    return $days !== NULL ? (int) floor($days / 7) : NULL;
  }

  /**
   * Calculate age in months (approximate).
   */
  protected function calculateAgeMonths($birth_date, $event_date) {
    $days = $this->calculateAgeDays($birth_date, $event_date);
    return $days !== NULL ? (int) floor($days / 30.44) : NULL;
  }

  /**
   * Get nutrition status from Z-score.
   */
  protected function getNutritionStatus($zscore) {
    if ($zscore === NULL || $zscore === '') {
      return NULL;
    }
    $zscore = (float) $zscore;
    if ($zscore >= -2) {
      return 'normal';
    }
    elseif ($zscore >= -3) {
      return 'moderate';
    }
    else {
      return 'severe';
    }
  }

  /**
   * Get vaccination timing category.
   */
  protected function getTimingCategory($days_from_expected, $grace_period = 14) {
    if ($days_from_expected === NULL) {
      return NULL;
    }
    if ($days_from_expected < -$grace_period) {
      return 'early';
    }
    elseif ($days_from_expected <= $grace_period) {
      return 'on_time';
    }
    elseif ($days_from_expected <= $grace_period * 2) {
      return 'late';
    }
    else {
      return 'very_late';
    }
  }

  /**
   * Checks memory usage and stops if limit is reached.
   */
  protected function checkMemoryLimit($last_nid = 0) {
    $memory_usage_mb = round(memory_get_usage() / 1048576);

    if ($memory_usage_mb >= $this->memoryLimit) {
      if ($last_nid > 0) {
        drush_print("-- Stopped before out of memory. Resume from node ID: " . $last_nid);
      }
      return FALSE;
    }

    return TRUE;
  }

  /**
   * Mark an entity as exported.
   */
  protected function markExported($type, $id) {
    if (!isset($this->exportedIds[$type])) {
      $this->exportedIds[$type] = [];
    }
    $this->exportedIds[$type][$id] = TRUE;
  }

  /**
   * Check if an entity was exported.
   */
  protected function wasExported($type, $id) {
    return isset($this->exportedIds[$type][$id]);
  }

  /**
   * Gets the schema configuration for all tables.
   */
  abstract protected function getSchemaConfig();

  /**
   * Exports the actual data as INSERT statements.
   */
  abstract protected function exportData();

}
