<?php

/**
 * @file
 * Restful plugin.
 *
 * This is a copy of the upstream files_upload__1.0.inc, with the `scheme` set
 * to `public` and the minor version bumped.
 */

$plugin = array(
  'label' => t('File upload public'),
  'description' => t('A file upload to public file system wrapped with RESTful.'),
  'resource' => 'files',
  'class' => 'HedleyRestfulFilesUploadPublic',
  'entity_type' => 'file',
  'authentication_types' => TRUE,
  'authentication_optional' => FALSE,
  'menu_item' => variable_get('restful_hook_menu_base_path', 'api') . '/file-upload-public',
);
