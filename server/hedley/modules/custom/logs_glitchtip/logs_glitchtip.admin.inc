<?php

/**
 * @file
 * Admin settings for the Logs GlitchTip module.
 */

/**
 * Admin settings form.
 *
 * @param array $form
 *   The form array.
 * @param array $form_state
 *   The form state array.
 *
 * @return array
 *   The settings form.
 */
function logs_glitchtip_admin_settings(array $form, array &$form_state) {
  $form['logs_glitchtip_enabled'] = [
    '#type' => 'checkbox',
    '#title' => t('Enabled'),
    '#description' => t('Send watchdog events to GlitchTip.'),
    '#default_value' => variable_get('logs_glitchtip_enabled', TRUE),
  ];

  $form['logs_glitchtip_dsn'] = [
    '#type' => 'textfield',
    '#title' => t('DSN'),
    '#description' => t('The GlitchTip project DSN. Leave empty to fall back to the GLITCHTIP_DSN environment variable; when neither is set, nothing is sent.'),
    '#default_value' => variable_get('logs_glitchtip_dsn', ''),
  ];

  $form['logs_glitchtip_severity_level'] = [
    '#type' => 'select',
    '#title' => t('Severity threshold'),
    '#description' => t('Events with this severity or worse are sent.'),
    '#options' => watchdog_severity_levels(),
    '#default_value' => variable_get('logs_glitchtip_severity_level', WATCHDOG_ERROR),
  ];

  $form['logs_glitchtip_environment'] = [
    '#type' => 'textfield',
    '#title' => t('Environment'),
    '#description' => t('Environment name reported to GlitchTip, for example "rwanda-live". Leave empty to auto-detect.'),
    '#default_value' => variable_get('logs_glitchtip_environment', ''),
  ];

  $form['logs_glitchtip_auto_disable_on_local'] = [
    '#type' => 'checkbox',
    '#title' => t('Auto disable on local'),
    '#description' => t('Do not send events from environments that were not identified as a real deployment.'),
    '#default_value' => variable_get('logs_glitchtip_auto_disable_on_local', TRUE),
  ];

  return system_settings_form($form);
}
