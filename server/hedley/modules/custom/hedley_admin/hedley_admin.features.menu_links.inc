<?php
/**
 * @file
 * hedley_admin.features.menu_links.inc
 */

/**
 * Implements hook_menu_default_menu_links().
 */
function hedley_admin_menu_default_menu_links() {
  $menu_links = array();

  // Exported menu link: menu-ihangane-admin-menu_content-list:admin/content.
  $menu_links['menu-ihangane-admin-menu_content-list:admin/content'] = array(
    'menu_name' => 'menu-ihangane-admin-menu',
    'link_path' => 'admin/content',
    'router_path' => 'admin/content',
    'link_title' => 'Content list',
    'options' => array(
      'attributes' => array(
        'title' => '',
      ),
      'identifier' => 'menu-ihangane-admin-menu_content-list:admin/content',
    ),
    'module' => 'menu',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -44,
    'customized' => 1,
  );
  // Exported menu link: menu-ihangane-admin-menu_create-child:node/add/child.
  $menu_links['menu-ihangane-admin-menu_create-child:node/add/child'] = array(
    'menu_name' => 'menu-ihangane-admin-menu',
    'link_path' => 'node/add/child',
    'router_path' => 'node/add',
    'link_title' => 'Create child',
    'options' => array(
      'attributes' => array(
        'title' => '',
      ),
      'identifier' => 'menu-ihangane-admin-menu_create-child:node/add/child',
    ),
    'module' => 'menu',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -48,
    'customized' => 1,
  );
  // Exported menu link: menu-ihangane-admin-menu_create-clinic:node/add/clinic.
  $menu_links['menu-ihangane-admin-menu_create-clinic:node/add/clinic'] = array(
    'menu_name' => 'menu-ihangane-admin-menu',
    'link_path' => 'node/add/clinic',
    'router_path' => 'node/add',
    'link_title' => 'Create clinic',
    'options' => array(
      'attributes' => array(
        'title' => '',
      ),
      'identifier' => 'menu-ihangane-admin-menu_create-clinic:node/add/clinic',
    ),
    'module' => 'menu',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -50,
    'customized' => 1,
  );
  // Exported menu link: menu-ihangane-admin-menu_create-mother:node/add/mother.
  $menu_links['menu-ihangane-admin-menu_create-mother:node/add/mother'] = array(
    'menu_name' => 'menu-ihangane-admin-menu',
    'link_path' => 'node/add/mother',
    'router_path' => 'node/add',
    'link_title' => 'Create mother',
    'options' => array(
      'attributes' => array(
        'title' => '',
      ),
      'identifier' => 'menu-ihangane-admin-menu_create-mother:node/add/mother',
    ),
    'module' => 'menu',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -49,
    'customized' => 1,
  );
  // Exported menu link: menu-ihangane-admin-menu_create-session:node/add/session.
  $menu_links['menu-ihangane-admin-menu_create-session:node/add/session'] = array(
    'menu_name' => 'menu-ihangane-admin-menu',
    'link_path' => 'node/add/session',
    'router_path' => 'node/add',
    'link_title' => 'Create session',
    'options' => array(
      'attributes' => array(
        'title' => '',
      ),
      'identifier' => 'menu-ihangane-admin-menu_create-session:node/add/session',
    ),
    'module' => 'menu',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -46,
    'customized' => 1,
  );
  // Exported menu link: menu-ihangane-admin-menu_create-user:admin/people/create.
  $menu_links['menu-ihangane-admin-menu_create-user:admin/people/create'] = array(
    'menu_name' => 'menu-ihangane-admin-menu',
    'link_path' => 'admin/people/create',
    'router_path' => 'admin/people/create',
    'link_title' => 'Create user',
    'options' => array(
      'attributes' => array(
        'title' => '',
      ),
      'identifier' => 'menu-ihangane-admin-menu_create-user:admin/people/create',
    ),
    'module' => 'menu',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -47,
    'customized' => 1,
  );

  // Translatables
  // Included for use with string extractors like potx.
  t('Content list');
  t('Create child');
  t('Create clinic');
  t('Create mother');
  t('Create session');
  t('Create user');

  return $menu_links;
}
