<?php

/**
 * @file
 * Define some calculated fields.
 */

/**
 * Define custom fields for views.
 */
function hedley_admin_views_data() {
  $data = [];

  $data['node']['zscore_wfa'] = [
    'title' => t('ZScore WFA'),
    'help' => t('Virtual field for WFA ZScore'),
    'field' => [
      'handler' => 'HedleyAdminZscoreWFA',
    ],
  ];

  $data['node']['zscore_lhfa'] = [
    'title' => t('ZScore LHFA'),
    'help' => t('Virtual field for LHFA ZScore'),
    'field' => [
      'handler' => 'HedleyAdminZscoreLHFA',
    ],
  ];

  return $data;
}
