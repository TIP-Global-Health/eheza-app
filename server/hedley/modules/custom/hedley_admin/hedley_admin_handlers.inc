<?php

/**
 * @file
 * Define some calculated fields.
 */

/**
 * Handler for WFA.
 */
class HedleyAdminZscoreWFA extends views_handler_field {

  /**
   * Overrides views_handler_field::render().
   */
  public function render($values) {
    if ($values->node_type != 'weight') {
      return "";
    }

    if (empty($values->field_field_zscore_age[0]['rendered']['#markup'])) {
      return "";
    }

    return $values->field_field_zscore_age[0]['rendered']['#markup'];
  }

  /**
   * Overrides views_handler_field::query().
   */
  public function query() {
    // Do nothing -- to override the parent query.
  }

}

/**
 * Handler for LHFA.
 */
class HedleyAdminZscoreLHFA extends views_handler_field {

  /**
   * Overrides views_handler_field::render().
   */
  public function render($values) {
    if ($values->node_type != 'height') {
      return "";
    }

    if (empty($values->field_field_zscore_age[0]['rendered']['#markup'])) {
      return "";
    }

    return $values->field_field_zscore_age[0]['rendered']['#markup'];
  }

  /**
   * Overrides views_handler_field::query().
   */
  public function query() {
    // Do nothing -- to override the parent query.
  }

}
