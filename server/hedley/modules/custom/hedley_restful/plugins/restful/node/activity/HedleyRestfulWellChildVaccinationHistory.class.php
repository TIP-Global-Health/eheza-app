<?php

/**
 * @file
 * Contains HedleyRestfulWellChildVaccinationHistory.
 */

/**
 * Class HedleyRestfulWellChildVaccinationHistory.
 */
class HedleyRestfulWellChildVaccinationHistory extends HedleyRestfulWellChildActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_suggested_vaccines',
    'field_administered_vaccines',
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
  protected $multiDateFields = [
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
