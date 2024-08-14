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

  // Exported menu: main-menu.
  $menus['main-menu'] = array(
    'menu_name' => 'main-menu',
    'title' => 'Main menu',
    'description' => 'The <em>Main</em> menu is used on many sites to show the major sections of the site, often in a top navigation bar.',
  );
  // Exported menu: menu-eheza-app-admin-menu.
  $menus['menu-eheza-app-admin-menu'] = array(
    'menu_name' => 'menu-eheza-app-admin-menu',
    'title' => 'E-Heza Admin Menu',
    'description' => 'Contains the links for the Eheza App admins',
  );
  // Translatables
  // Included for use with string extractors like potx.
  t('Contains the links for the Eheza App admins');
  t('E-Heza Admin Menu');
  t('Main menu');
  t('The <em>Main</em> menu is used on many sites to show the major sections of the site, often in a top navigation bar.');

  return $menus;
}
