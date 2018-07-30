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

  // Exported menu link: main-menu_children:admin/content/children.
  $menu_links['main-menu_children:admin/content/children'] = array(
    'menu_name' => 'main-menu',
    'link_path' => 'admin/content/children',
    'router_path' => 'admin/content/children',
    'link_title' => 'Children',
    'options' => array(
      'identifier' => 'main-menu_children:admin/content/children',
    ),
    'module' => 'system',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -47,
    'customized' => 1,
  );
  // Exported menu link: main-menu_clinics:admin/content/clinics.
  $menu_links['main-menu_clinics:admin/content/clinics'] = array(
    'menu_name' => 'main-menu',
    'link_path' => 'admin/content/clinics',
    'router_path' => 'admin/content/clinics',
    'link_title' => 'Clinics',
    'options' => array(
      'identifier' => 'main-menu_clinics:admin/content/clinics',
    ),
    'module' => 'system',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -49,
    'customized' => 1,
  );
  // Exported menu link: main-menu_historical-child:admin/measurement-historical-child.
  $menu_links['main-menu_historical-child:admin/measurement-historical-child'] = array(
    'menu_name' => 'main-menu',
    'link_path' => 'admin/measurement-historical-child',
    'router_path' => 'admin/measurement-historical-child',
    'link_title' => 'Historical Child',
    'options' => array(
      'identifier' => 'main-menu_historical-child:admin/measurement-historical-child',
    ),
    'module' => 'system',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -43,
    'customized' => 1,
  );
  // Exported menu link: main-menu_historical-mother:admin/measurement-historical-mother.
  $menu_links['main-menu_historical-mother:admin/measurement-historical-mother'] = array(
    'menu_name' => 'main-menu',
    'link_path' => 'admin/measurement-historical-mother',
    'router_path' => 'admin/measurement-historical-mother',
    'link_title' => 'Historical Mother',
    'options' => array(
      'identifier' => 'main-menu_historical-mother:admin/measurement-historical-mother',
    ),
    'module' => 'system',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -44,
    'customized' => 1,
  );
  // Exported menu link: main-menu_home:<front>.
  $menu_links['main-menu_home:<front>'] = array(
    'menu_name' => 'main-menu',
    'link_path' => '<front>',
    'router_path' => '',
    'link_title' => 'Home',
    'options' => array(
      'identifier' => 'main-menu_home:<front>',
    ),
    'module' => 'menu',
    'hidden' => 0,
    'external' => 1,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -50,
    'customized' => 1,
  );
  // Exported menu link: main-menu_measurements:admin/content/measurements.
  $menu_links['main-menu_measurements:admin/content/measurements'] = array(
    'menu_name' => 'main-menu',
    'link_path' => 'admin/content/measurements',
    'router_path' => 'admin/content/measurements',
    'link_title' => 'Measurements',
    'options' => array(
      'identifier' => 'main-menu_measurements:admin/content/measurements',
    ),
    'module' => 'system',
    'hidden' => 1,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -45,
    'customized' => 1,
  );
  // Exported menu link: main-menu_mothers:admin/content/mothers.
  $menu_links['main-menu_mothers:admin/content/mothers'] = array(
    'menu_name' => 'main-menu',
    'link_path' => 'admin/content/mothers',
    'router_path' => 'admin/content/mothers',
    'link_title' => 'Mothers',
    'options' => array(
      'identifier' => 'main-menu_mothers:admin/content/mothers',
    ),
    'module' => 'system',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -48,
    'customized' => 1,
  );
  // Exported menu link: main-menu_sessions:admin/content/sessions.
  $menu_links['main-menu_sessions:admin/content/sessions'] = array(
    'menu_name' => 'main-menu',
    'link_path' => 'admin/content/sessions',
    'router_path' => 'admin/content/sessions',
    'link_title' => 'Sessions',
    'options' => array(
      'identifier' => 'main-menu_sessions:admin/content/sessions',
    ),
    'module' => 'system',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -46,
    'customized' => 1,
  );
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
  // Exported menu link: menu-ihangane-admin-menu_create-caregiver:node/add/mother.
  $menu_links['menu-ihangane-admin-menu_create-caregiver:node/add/mother'] = array(
    'menu_name' => 'menu-ihangane-admin-menu',
    'link_path' => 'node/add/mother',
    'router_path' => 'node/add',
    'link_title' => 'Create caregiver',
    'options' => array(
      'attributes' => array(
        'title' => '',
      ),
      'identifier' => 'menu-ihangane-admin-menu_create-caregiver:node/add/mother',
    ),
    'module' => 'menu',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -48,
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
    'weight' => -47,
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
    'weight' => -45,
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
    'weight' => -46,
    'customized' => 1,
  );

  // Translatables
  // Included for use with string extractors like potx.
  t('Children');
  t('Clinics');
  t('Content list');
  t('Create caregiver');
  t('Create child');
  t('Create clinic');
  t('Create mother');
  t('Create session');
  t('Create user');
  t('Historical Child');
  t('Historical Mother');
  t('Home');
  t('Measurements');
  t('Mothers');
  t('Sessions');

  return $menu_links;
}
