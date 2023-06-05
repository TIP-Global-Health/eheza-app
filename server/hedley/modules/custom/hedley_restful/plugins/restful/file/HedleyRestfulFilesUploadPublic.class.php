<?php

/**
 * @file
 * Contains HedleyRestfulFilesUploadPublic.
 */

class HedleyRestfulFilesUploadPublic extends RestfulFilesUpload {

  /**
   * Overrides \RestfulFilesUpload::__construct()
   *
   * Sets public scheme, and no validation.
   */
  public function __construct(array $plugin, \RestfulAuthenticationManager $auth_manager = NULL, \DrupalCacheInterface $cache_controller = NULL, $language = NULL) {
    parent::__construct($plugin, $auth_manager, $cache_controller, $language);

    $default_values = array(
      'validators' => array(
        'file_validate_extensions' => array(),
        'file_validate_size' => array(),
      ),
      'scheme' => 'public',
      'replace' => FILE_EXISTS_RENAME,
    );

    $this->setPluginKey('options', $default_values);
  }

}
