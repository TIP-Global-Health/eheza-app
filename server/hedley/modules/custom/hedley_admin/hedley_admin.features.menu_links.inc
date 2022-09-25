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
    'router_path' => 'admin/content',
    'link_title' => 'Children',
    'options' => array(
      'identifier' => 'main-menu_children:admin/content/children',
    ),
    'module' => 'system',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -44,
    'customized' => 1,
  );
  // Exported menu link: main-menu_group-encounters:admin/content/sessions.
  $menu_links['main-menu_group-encounters:admin/content/sessions'] = array(
    'menu_name' => 'main-menu',
    'link_path' => 'admin/content/sessions',
    'router_path' => 'admin/content',
    'link_title' => 'Group Encounters',
    'options' => array(
      'identifier' => 'main-menu_group-encounters:admin/content/sessions',
    ),
    'module' => 'system',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -43,
    'customized' => 1,
  );
  // Exported menu link: main-menu_groups:admin/content/clinics.
  $menu_links['main-menu_groups:admin/content/clinics'] = array(
    'menu_name' => 'main-menu',
    'link_path' => 'admin/content/clinics',
    'router_path' => 'admin/content',
    'link_title' => 'Groups',
    'options' => array(
      'identifier' => 'main-menu_groups:admin/content/clinics',
    ),
    'module' => 'system',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -48,
    'customized' => 1,
  );
  // Exported menu link: main-menu_historical-child:admin/measurement-historical-child.
  $menu_links['main-menu_historical-child:admin/measurement-historical-child'] = array(
    'menu_name' => 'main-menu',
    'link_path' => 'admin/measurement-historical-child',
    'router_path' => 'admin',
    'link_title' => 'Historical Child',
    'options' => array(
      'identifier' => 'main-menu_historical-child:admin/measurement-historical-child',
    ),
    'module' => 'system',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -39,
    'customized' => 1,
  );
  // Exported menu link: main-menu_historical-mother:admin/measurement-historical-mother.
  $menu_links['main-menu_historical-mother:admin/measurement-historical-mother'] = array(
    'menu_name' => 'main-menu',
    'link_path' => 'admin/measurement-historical-mother',
    'router_path' => 'admin',
    'link_title' => 'Historical Mother',
    'options' => array(
      'identifier' => 'main-menu_historical-mother:admin/measurement-historical-mother',
    ),
    'module' => 'system',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -40,
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
    'router_path' => 'admin/content',
    'link_title' => 'Measurements',
    'options' => array(
      'identifier' => 'main-menu_measurements:admin/content/measurements',
    ),
    'module' => 'system',
    'hidden' => 1,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -41,
    'customized' => 1,
  );
  // Exported menu link: main-menu_mothers--caregivers:admin/content/mothers.
  $menu_links['main-menu_mothers--caregivers:admin/content/mothers'] = array(
    'menu_name' => 'main-menu',
    'link_path' => 'admin/content/mothers',
    'router_path' => 'admin/content',
    'link_title' => 'Mothers / Caregivers',
    'options' => array(
      'identifier' => 'main-menu_mothers--caregivers:admin/content/mothers',
    ),
    'module' => 'system',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -46,
    'customized' => 1,
  );
  // Exported menu link: menu-eheza-app-admin-menu_content-list:admin/content.
  $menu_links['menu-eheza-app-admin-menu_content-list:admin/content'] = array(
    'menu_name' => 'menu-eheza-app-admin-menu',
    'link_path' => 'admin/content',
    'router_path' => 'admin/content',
    'link_title' => 'Content list',
    'options' => array(
      'attributes' => array(
        'title' => '',
      ),
      'identifier' => 'menu-eheza-app-admin-menu_content-list:admin/content',
    ),
    'module' => 'menu',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -43,
    'customized' => 1,
  );
  // Exported menu link: menu-eheza-app-admin-menu_create-device:node/add/device.
  $menu_links['menu-eheza-app-admin-menu_create-device:node/add/device'] = array(
    'menu_name' => 'menu-eheza-app-admin-menu',
    'link_path' => 'node/add/device',
    'router_path' => 'node/add/device',
    'link_title' => 'Create Device',
    'options' => array(
      'attributes' => array(
        'title' => '',
      ),
      'identifier' => 'menu-eheza-app-admin-menu_create-device:node/add/device',
    ),
    'module' => 'menu',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -50,
    'customized' => 1,
  );
  // Exported menu link: menu-eheza-app-admin-menu_create-group-encounter:node/add/session.
  $menu_links['menu-eheza-app-admin-menu_create-group-encounter:node/add/session'] = array(
    'menu_name' => 'menu-eheza-app-admin-menu',
    'link_path' => 'node/add/session',
    'router_path' => 'node/add',
    'link_title' => 'Create group encounter',
    'options' => array(
      'attributes' => array(
        'title' => '',
      ),
      'identifier' => 'menu-eheza-app-admin-menu_create-group-encounter:node/add/session',
    ),
    'module' => 'menu',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -44,
    'customized' => 1,
  );
  // Exported menu link: menu-eheza-app-admin-menu_create-group:node/add/clinic.
  $menu_links['menu-eheza-app-admin-menu_create-group:node/add/clinic'] = array(
    'menu_name' => 'menu-eheza-app-admin-menu',
    'link_path' => 'node/add/clinic',
    'router_path' => 'node/add',
    'link_title' => 'Create group',
    'options' => array(
      'attributes' => array(
        'title' => '',
      ),
      'identifier' => 'menu-eheza-app-admin-menu_create-group:node/add/clinic',
    ),
    'module' => 'menu',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -45,
    'customized' => 1,
  );
  // Exported menu link: menu-eheza-app-admin-menu_create-nurse:node/add/nurse.
  $menu_links['menu-eheza-app-admin-menu_create-nurse:node/add/nurse'] = array(
    'menu_name' => 'menu-eheza-app-admin-menu',
    'link_path' => 'node/add/nurse',
    'router_path' => 'node/add/nurse',
    'link_title' => 'Create Nurse',
    'options' => array(
      'attributes' => array(
        'title' => '',
      ),
      'identifier' => 'menu-eheza-app-admin-menu_create-nurse:node/add/nurse',
    ),
    'module' => 'menu',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -49,
    'customized' => 1,
  );
  // Exported menu link: menu-eheza-app-admin-menu_create-person:node/add/person.
  $menu_links['menu-eheza-app-admin-menu_create-person:node/add/person'] = array(
    'menu_name' => 'menu-eheza-app-admin-menu',
    'link_path' => 'node/add/person',
    'router_path' => 'node/add/person',
    'link_title' => 'Create Person',
    'options' => array(
      'attributes' => array(
        'title' => '',
      ),
      'identifier' => 'menu-eheza-app-admin-menu_create-person:node/add/person',
    ),
    'module' => 'menu',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -48,
    'customized' => 1,
  );
  // Exported menu link: menu-eheza-app-admin-menu_create-pmtct-participation:node/add/pmtct-paticipation.
  $menu_links['menu-eheza-app-admin-menu_create-pmtct-participation:node/add/pmtct-paticipation'] = array(
    'menu_name' => 'menu-eheza-app-admin-menu',
    'link_path' => 'node/add/pmtct-paticipation',
    'router_path' => 'node/add',
    'link_title' => 'Create PMTCT Participation',
    'options' => array(
      'attributes' => array(
        'title' => '',
      ),
      'identifier' => 'menu-eheza-app-admin-menu_create-pmtct-participation:node/add/pmtct-paticipation',
    ),
    'module' => 'menu',
    'hidden' => 0,
    'external' => 0,
    'has_children' => 0,
    'expanded' => 0,
    'weight' => -46,
    'customized' => 1,
  );
  // Exported menu link: menu-eheza-app-admin-menu_create-relationship:node/add/relationship.
  $menu_links['menu-eheza-app-admin-menu_create-relationship:node/add/relationship'] = array(
    'menu_name' => 'menu-eheza-app-admin-menu',
    'link_path' => 'node/add/relationship',
    'router_path' => 'node/add/relationship',
    'link_title' => 'Create Relationship',
    'options' => array(
      'attributes' => array(
        'title' => '',
      ),
      'identifier' => 'menu-eheza-app-admin-menu_create-relationship:node/add/relationship',
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
  t('Children');
  t('Content list');
  t('Create Device');
  t('Create Nurse');
  t('Create PMTCT Participation');
  t('Create Person');
  t('Create Relationship');
  t('Create group');
  t('Create group encounter');
  t('Group Encounters');
  t('Groups');
  t('Historical Child');
  t('Historical Mother');
  t('Home');
  t('Measurements');
  t('Mothers / Caregivers');

  return $menu_links;
}
