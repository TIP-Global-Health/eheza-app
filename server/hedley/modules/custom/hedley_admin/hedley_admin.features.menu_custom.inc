<?php
/**
 * @file
 * hedley_admin.features.menu_custom.inc
 */

/**
 * Implements hook_menu_default_menu_custom().
 */
function hedley_admin_menu_default_menu_custom() {
  $menus = array();

  // Exported menu: menu-ihangane-admin-menu.
  $menus['menu-ihangane-admin-menu'] = array(
    'menu_name' => 'menu-ihangane-admin-menu',
    'title' => 'E-Heza Admin Menu',
    'description' => 'Contains the links for the ihangane admins',
  );
  // Translatables
  // Included for use with string extractors like potx.
  t('Contains the links for the ihangane admins');
  t('E-Heza Admin Menu');

  return $menus;
}
