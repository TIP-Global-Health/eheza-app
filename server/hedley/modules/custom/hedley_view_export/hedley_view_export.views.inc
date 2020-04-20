<?php

/**
 * @file
 * Views include file with views hooks.
 */

/**
 * Implements hook_views_plugins().
 */
function hedley_view_export_views_plugins() {
  $path = drupal_get_path('module', 'hedley_view_export');

  return [
    'display' => [
      'hedley_view_export' => [
        'title' => t('AQ export'),
        'help' => t('Export view with Advanced queue.'),
        'path' => $path . '/plugins',
        'handler' => 'hedley_view_export_plugin_display_export',
        'parent' => 'feed',
        'uses hook menu' => TRUE,
        'use ajax' => FALSE,
        'use pager' => FALSE,
        'accept attachments' => FALSE,
        'admin' => t('AQ export'),
      ],
    ],
  ];
}
