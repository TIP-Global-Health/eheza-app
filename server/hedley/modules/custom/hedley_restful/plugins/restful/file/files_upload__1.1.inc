<?php

/**
 * @file
 * Restful plugin.
 *
 * This is a copy of the upstream files_upload__1.0.inc, with the `scheme` set
 * to `private` and the minor version bumped.
 */

$plugin = array(
  'label' => t('File upload'),
  'description' => t('A file upload wrapped with RESTful.'),
  'minor_version' => 1,
  'resource' => 'files',
  'class' => 'RestfulFilesUpload',
  'entity_type' => 'file',
  'authentication_types' => TRUE,
  'authentication_optional' => FALSE,
  'menu_item' => variable_get('restful_hook_menu_base_path', 'api') . '/file-upload',
  'options' => [
    'scheme' => 'private',
  ],
);
