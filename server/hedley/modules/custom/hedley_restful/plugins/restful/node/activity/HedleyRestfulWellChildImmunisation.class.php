<?php

/**
 * @file
 * Contains HedleyRestfulWellChildImmunisation.
 */

/**
 * Class HedleyRestfulWellChildImmunisation.
 */
class HedleyRestfulWellChildImmunisation extends HedleyRestfulWellChildActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_suggested_vaccines',
    'field_vaccination_notes',
  ];

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_bcg_vaccination_date',
    'field_opv_vaccination_date',
    'field_dtp_vaccination_date',
    'field_pcv13_vaccination_date',
    'field_rotarix_vaccination_date',
    'field_ipv_vaccination_date',
    'field_mr_vaccination_date',
    'field_hpv_vaccination_date',
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_bcg_vaccination_date',
    'field_opv_vaccination_date',
    'field_dtp_vaccination_date',
    'field_pcv13_vaccination_date',
    'field_rotarix_vaccination_date',
    'field_ipv_vaccination_date',
    'field_mr_vaccination_date',
    'field_hpv_vaccination_date',
  ];

}
