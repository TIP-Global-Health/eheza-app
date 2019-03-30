<?php
/**
 * @file
 * hedley_admin.views_default.inc
 */

/**
 * Implements hook_views_default_views().
 */
function hedley_admin_views_default_views() {
  $export = array();

  $view = new view();
  $view->name = 'admin_views';
  $view->description = '';
  $view->tag = 'default';
  $view->base_table = 'node';
  $view->human_name = 'Admin views';
  $view->core = 7;
  $view->api_version = '3.0';
  $view->disabled = FALSE; /* Edit this to true to make a default view disabled initially */

  /* Display: Master */
  $handler = $view->new_display('default', 'Master', 'default');
  $handler->display->display_options['title'] = 'Children';
  $handler->display->display_options['use_more_always'] = FALSE;
  $handler->display->display_options['group_by'] = TRUE;
  $handler->display->display_options['access']['type'] = 'perm';
  $handler->display->display_options['cache']['type'] = 'none';
  $handler->display->display_options['query']['type'] = 'views_query';
  $handler->display->display_options['query']['options']['pure_distinct'] = TRUE;
  $handler->display->display_options['exposed_form']['type'] = 'basic';
  $handler->display->display_options['pager']['type'] = 'full';
  $handler->display->display_options['pager']['options']['items_per_page'] = '50';
  $handler->display->display_options['style_plugin'] = 'table';
  $handler->display->display_options['style_options']['columns'] = array(
    'title' => 'title',
    'field_clinic' => 'field_clinic',
    'title_1' => 'title_1',
    'field_closed' => 'field_closed',
    'nid' => 'nid',
    'edit_node' => 'edit_node',
  );
  $handler->display->display_options['style_options']['default'] = '-1';
  $handler->display->display_options['style_options']['info'] = array(
    'title' => array(
      'sortable' => 1,
      'default_sort_order' => 'asc',
      'align' => '',
      'separator' => '',
      'empty_column' => 0,
    ),
    'field_clinic' => array(
      'sortable' => 0,
      'default_sort_order' => 'asc',
      'align' => '',
      'separator' => '',
      'empty_column' => 0,
    ),
    'title_1' => array(
      'sortable' => 0,
      'default_sort_order' => 'asc',
      'align' => '',
      'separator' => '',
      'empty_column' => 0,
    ),
    'field_closed' => array(
      'sortable' => 0,
      'default_sort_order' => 'asc',
      'align' => '',
      'separator' => '',
      'empty_column' => 0,
    ),
    'nid' => array(
      'sortable' => 0,
      'default_sort_order' => 'asc',
      'align' => '',
      'separator' => '',
      'empty_column' => 0,
    ),
    'edit_node' => array(
      'align' => '',
      'separator' => '',
      'empty_column' => 0,
    ),
  );
  /* Relationship: Relationship */
  $handler->display->display_options['relationships']['reverse_field_related_to_node']['id'] = 'reverse_field_related_to_node';
  $handler->display->display_options['relationships']['reverse_field_related_to_node']['table'] = 'node';
  $handler->display->display_options['relationships']['reverse_field_related_to_node']['field'] = 'reverse_field_related_to_node';
  $handler->display->display_options['relationships']['reverse_field_related_to_node']['ui_name'] = 'Relationship';
  $handler->display->display_options['relationships']['reverse_field_related_to_node']['label'] = 'Relationship';
  /* Relationship: Mother/Caregiver */
  $handler->display->display_options['relationships']['field_person_target_id']['id'] = 'field_person_target_id';
  $handler->display->display_options['relationships']['field_person_target_id']['table'] = 'field_data_field_person';
  $handler->display->display_options['relationships']['field_person_target_id']['field'] = 'field_person_target_id';
  $handler->display->display_options['relationships']['field_person_target_id']['relationship'] = 'reverse_field_related_to_node';
  $handler->display->display_options['relationships']['field_person_target_id']['ui_name'] = 'Mother/Caregiver';
  $handler->display->display_options['relationships']['field_person_target_id']['label'] = 'Mother/Caregiver';
  /* Relationship: Referencing child */
  $handler->display->display_options['relationships']['reverse_field_person_node']['id'] = 'reverse_field_person_node';
  $handler->display->display_options['relationships']['reverse_field_person_node']['table'] = 'node';
  $handler->display->display_options['relationships']['reverse_field_person_node']['field'] = 'reverse_field_person_node';
  $handler->display->display_options['relationships']['reverse_field_person_node']['ui_name'] = 'Referencing child';
  $handler->display->display_options['relationships']['reverse_field_person_node']['label'] = 'Referencing child';
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title']['id'] = 'title';
  $handler->display->display_options['fields']['title']['table'] = 'node';
  $handler->display->display_options['fields']['title']['field'] = 'title';
  $handler->display->display_options['fields']['title']['label'] = 'Name';
  $handler->display->display_options['fields']['title']['alter']['word_boundary'] = FALSE;
  $handler->display->display_options['fields']['title']['alter']['ellipsis'] = FALSE;
  /* Field: MAX(Content: Title) */
  $handler->display->display_options['fields']['title_1']['id'] = 'title_1';
  $handler->display->display_options['fields']['title_1']['table'] = 'node';
  $handler->display->display_options['fields']['title_1']['field'] = 'title';
  $handler->display->display_options['fields']['title_1']['relationship'] = 'field_person_target_id';
  $handler->display->display_options['fields']['title_1']['group_type'] = 'max';
  $handler->display->display_options['fields']['title_1']['label'] = 'Mother / Caregiver';
  /* Field: MAX(Content: Clinic) */
  $handler->display->display_options['fields']['field_clinic']['id'] = 'field_clinic';
  $handler->display->display_options['fields']['field_clinic']['table'] = 'field_data_field_clinic';
  $handler->display->display_options['fields']['field_clinic']['field'] = 'field_clinic';
  $handler->display->display_options['fields']['field_clinic']['relationship'] = 'reverse_field_person_node';
  $handler->display->display_options['fields']['field_clinic']['group_type'] = 'max';
  $handler->display->display_options['fields']['field_clinic']['settings'] = array(
    'bypass_access' => 0,
    'link' => 0,
  );
  /* Sort criterion: Content: Post date */
  $handler->display->display_options['sorts']['created']['id'] = 'created';
  $handler->display->display_options['sorts']['created']['table'] = 'node';
  $handler->display->display_options['sorts']['created']['field'] = 'created';
  $handler->display->display_options['sorts']['created']['order'] = 'DESC';
  /* Sort criterion: Content: Nid */
  $handler->display->display_options['sorts']['nid']['id'] = 'nid';
  $handler->display->display_options['sorts']['nid']['table'] = 'node';
  $handler->display->display_options['sorts']['nid']['field'] = 'nid';
  /* Filter criterion: Content: Published */
  $handler->display->display_options['filters']['status']['id'] = 'status';
  $handler->display->display_options['filters']['status']['table'] = 'node';
  $handler->display->display_options['filters']['status']['field'] = 'status';
  $handler->display->display_options['filters']['status']['value'] = 1;
  $handler->display->display_options['filters']['status']['group'] = 1;
  $handler->display->display_options['filters']['status']['expose']['operator'] = FALSE;
  /* Filter criterion: Content: Type */
  $handler->display->display_options['filters']['type']['id'] = 'type';
  $handler->display->display_options['filters']['type']['table'] = 'node';
  $handler->display->display_options['filters']['type']['field'] = 'type';
  $handler->display->display_options['filters']['type']['value'] = array(
    'person' => 'person',
  );
  /* Filter criterion: Child name */
  $handler->display->display_options['filters']['title']['id'] = 'title';
  $handler->display->display_options['filters']['title']['table'] = 'node';
  $handler->display->display_options['filters']['title']['field'] = 'title';
  $handler->display->display_options['filters']['title']['ui_name'] = 'Child name';
  $handler->display->display_options['filters']['title']['exposed'] = TRUE;
  $handler->display->display_options['filters']['title']['expose']['operator_id'] = 'title_op';
  $handler->display->display_options['filters']['title']['expose']['label'] = 'Child name';
  $handler->display->display_options['filters']['title']['expose']['operator'] = 'title_op';
  $handler->display->display_options['filters']['title']['expose']['identifier'] = 'child_name';
  $handler->display->display_options['filters']['title']['expose']['remember_roles'] = array(
    2 => '2',
    1 => 0,
    3 => 0,
    4 => 0,
  );
  /* Filter criterion: Mother name */
  $handler->display->display_options['filters']['title_1']['id'] = 'title_1';
  $handler->display->display_options['filters']['title_1']['table'] = 'node';
  $handler->display->display_options['filters']['title_1']['field'] = 'title';
  $handler->display->display_options['filters']['title_1']['relationship'] = 'field_person_target_id';
  $handler->display->display_options['filters']['title_1']['ui_name'] = 'Mother name';
  $handler->display->display_options['filters']['title_1']['exposed'] = TRUE;
  $handler->display->display_options['filters']['title_1']['expose']['operator_id'] = 'title_1_op';
  $handler->display->display_options['filters']['title_1']['expose']['label'] = 'Mother name';
  $handler->display->display_options['filters']['title_1']['expose']['operator'] = 'title_1_op';
  $handler->display->display_options['filters']['title_1']['expose']['identifier'] = 'mother_name';
  $handler->display->display_options['filters']['title_1']['expose']['remember_roles'] = array(
    2 => '2',
    1 => 0,
    4 => 0,
    3 => 0,
    5 => 0,
  );
  /* Filter criterion: Clinic */
  $handler->display->display_options['filters']['field_clinic_target_id']['id'] = 'field_clinic_target_id';
  $handler->display->display_options['filters']['field_clinic_target_id']['table'] = 'field_data_field_clinic';
  $handler->display->display_options['filters']['field_clinic_target_id']['field'] = 'field_clinic_target_id';
  $handler->display->display_options['filters']['field_clinic_target_id']['relationship'] = 'reverse_field_person_node';
  $handler->display->display_options['filters']['field_clinic_target_id']['ui_name'] = 'Clinic';
  $handler->display->display_options['filters']['field_clinic_target_id']['exposed'] = TRUE;
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['operator_id'] = 'field_clinic_target_id_op';
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['label'] = 'Clinic';
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['operator'] = 'field_clinic_target_id_op';
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['identifier'] = 'clinic';
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['remember_roles'] = array(
    2 => '2',
    1 => 0,
    4 => 0,
    3 => 0,
    5 => 0,
  );
  /* Filter criterion: COUNT(Content: Nid) */
  $handler->display->display_options['filters']['nid']['id'] = 'nid';
  $handler->display->display_options['filters']['nid']['table'] = 'node';
  $handler->display->display_options['filters']['nid']['field'] = 'nid';
  $handler->display->display_options['filters']['nid']['relationship'] = 'reverse_field_related_to_node';
  $handler->display->display_options['filters']['nid']['group_type'] = 'count';
  $handler->display->display_options['filters']['nid']['operator'] = '>';
  $handler->display->display_options['filters']['nid']['value']['value'] = '0';

  /* Display: Child */
  $handler = $view->new_display('page', 'Child', 'page');
  $handler->display->display_options['defaults']['empty'] = FALSE;
  /* No results behavior: Global: Text area */
  $handler->display->display_options['empty']['area']['id'] = 'area';
  $handler->display->display_options['empty']['area']['table'] = 'views';
  $handler->display->display_options['empty']['area']['field'] = 'area';
  $handler->display->display_options['empty']['area']['empty'] = TRUE;
  $handler->display->display_options['empty']['area']['content'] = 'No records exist for these criteria';
  $handler->display->display_options['empty']['area']['format'] = 'filtered_html';
  $handler->display->display_options['defaults']['fields'] = FALSE;
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title']['id'] = 'title';
  $handler->display->display_options['fields']['title']['table'] = 'node';
  $handler->display->display_options['fields']['title']['field'] = 'title';
  $handler->display->display_options['fields']['title']['label'] = 'Name';
  $handler->display->display_options['fields']['title']['alter']['word_boundary'] = FALSE;
  $handler->display->display_options['fields']['title']['alter']['ellipsis'] = FALSE;
  /* Field: MIN(Content revision: Title) */
  $handler->display->display_options['fields']['title_1']['id'] = 'title_1';
  $handler->display->display_options['fields']['title_1']['table'] = 'node_revision';
  $handler->display->display_options['fields']['title_1']['field'] = 'title';
  $handler->display->display_options['fields']['title_1']['relationship'] = 'field_person_target_id';
  $handler->display->display_options['fields']['title_1']['group_type'] = 'min';
  $handler->display->display_options['fields']['title_1']['label'] = 'Mother / Caregiver';
  $handler->display->display_options['fields']['title_1']['link_to_node'] = TRUE;
  /* Field: MAX(Content: Clinic) */
  $handler->display->display_options['fields']['field_clinic_1']['id'] = 'field_clinic_1';
  $handler->display->display_options['fields']['field_clinic_1']['table'] = 'field_data_field_clinic';
  $handler->display->display_options['fields']['field_clinic_1']['field'] = 'field_clinic';
  $handler->display->display_options['fields']['field_clinic_1']['relationship'] = 'reverse_field_person_node';
  $handler->display->display_options['fields']['field_clinic_1']['group_type'] = 'max';
  $handler->display->display_options['fields']['field_clinic_1']['settings'] = array(
    'bypass_access' => 0,
    'link' => 0,
  );
  /* Field: Content: Edit link */
  $handler->display->display_options['fields']['edit_node']['id'] = 'edit_node';
  $handler->display->display_options['fields']['edit_node']['table'] = 'views_entity_node';
  $handler->display->display_options['fields']['edit_node']['field'] = 'edit_node';
  $handler->display->display_options['fields']['edit_node']['label'] = 'Edit';
  $handler->display->display_options['defaults']['filter_groups'] = FALSE;
  $handler->display->display_options['defaults']['filters'] = FALSE;
  /* Filter criterion: Content: Published */
  $handler->display->display_options['filters']['status']['id'] = 'status';
  $handler->display->display_options['filters']['status']['table'] = 'node';
  $handler->display->display_options['filters']['status']['field'] = 'status';
  $handler->display->display_options['filters']['status']['value'] = 1;
  $handler->display->display_options['filters']['status']['group'] = 1;
  $handler->display->display_options['filters']['status']['expose']['operator'] = FALSE;
  /* Filter criterion: Content: Type */
  $handler->display->display_options['filters']['type']['id'] = 'type';
  $handler->display->display_options['filters']['type']['table'] = 'node';
  $handler->display->display_options['filters']['type']['field'] = 'type';
  $handler->display->display_options['filters']['type']['value'] = array(
    'person' => 'person',
  );
  /* Filter criterion: Child name */
  $handler->display->display_options['filters']['title']['id'] = 'title';
  $handler->display->display_options['filters']['title']['table'] = 'node';
  $handler->display->display_options['filters']['title']['field'] = 'title';
  $handler->display->display_options['filters']['title']['ui_name'] = 'Child name';
  $handler->display->display_options['filters']['title']['operator'] = 'word';
  $handler->display->display_options['filters']['title']['exposed'] = TRUE;
  $handler->display->display_options['filters']['title']['expose']['operator_id'] = 'title_op';
  $handler->display->display_options['filters']['title']['expose']['label'] = 'Child name';
  $handler->display->display_options['filters']['title']['expose']['operator'] = 'title_op';
  $handler->display->display_options['filters']['title']['expose']['identifier'] = 'child_name';
  $handler->display->display_options['filters']['title']['expose']['remember_roles'] = array(
    2 => '2',
    1 => 0,
    3 => 0,
    4 => 0,
  );
  /* Filter criterion: Mother name */
  $handler->display->display_options['filters']['title_1']['id'] = 'title_1';
  $handler->display->display_options['filters']['title_1']['table'] = 'node';
  $handler->display->display_options['filters']['title_1']['field'] = 'title';
  $handler->display->display_options['filters']['title_1']['relationship'] = 'field_person_target_id';
  $handler->display->display_options['filters']['title_1']['ui_name'] = 'Mother name';
  $handler->display->display_options['filters']['title_1']['operator'] = 'word';
  $handler->display->display_options['filters']['title_1']['exposed'] = TRUE;
  $handler->display->display_options['filters']['title_1']['expose']['operator_id'] = 'title_1_op';
  $handler->display->display_options['filters']['title_1']['expose']['label'] = 'Mother/Caregiver name';
  $handler->display->display_options['filters']['title_1']['expose']['operator'] = 'title_1_op';
  $handler->display->display_options['filters']['title_1']['expose']['identifier'] = 'mother_name';
  $handler->display->display_options['filters']['title_1']['expose']['remember_roles'] = array(
    2 => '2',
    1 => 0,
    4 => 0,
    3 => 0,
    5 => 0,
  );
  /* Filter criterion: Clinic */
  $handler->display->display_options['filters']['field_clinic_target_id']['id'] = 'field_clinic_target_id';
  $handler->display->display_options['filters']['field_clinic_target_id']['table'] = 'field_data_field_clinic';
  $handler->display->display_options['filters']['field_clinic_target_id']['field'] = 'field_clinic_target_id';
  $handler->display->display_options['filters']['field_clinic_target_id']['relationship'] = 'reverse_field_person_node';
  $handler->display->display_options['filters']['field_clinic_target_id']['ui_name'] = 'Clinic';
  $handler->display->display_options['filters']['field_clinic_target_id']['exposed'] = TRUE;
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['operator_id'] = 'field_clinic_target_id_op';
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['label'] = 'Clinic';
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['operator'] = 'field_clinic_target_id_op';
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['identifier'] = 'clinic';
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['remember_roles'] = array(
    2 => '2',
    1 => 0,
    4 => 0,
    3 => 0,
    5 => 0,
  );
  /* Filter criterion: COUNT(Content: Nid) */
  $handler->display->display_options['filters']['nid']['id'] = 'nid';
  $handler->display->display_options['filters']['nid']['table'] = 'node';
  $handler->display->display_options['filters']['nid']['field'] = 'nid';
  $handler->display->display_options['filters']['nid']['relationship'] = 'reverse_field_related_to_node';
  $handler->display->display_options['filters']['nid']['group_type'] = 'count';
  $handler->display->display_options['filters']['nid']['operator'] = '>';
  $handler->display->display_options['filters']['nid']['value']['value'] = '0';
  $handler->display->display_options['path'] = 'admin/content/children';
  $handler->display->display_options['menu']['type'] = 'normal';
  $handler->display->display_options['menu']['title'] = 'Children';
  $handler->display->display_options['menu']['weight'] = '6';
  $handler->display->display_options['menu']['name'] = 'main-menu';
  $handler->display->display_options['menu']['context'] = 0;
  $handler->display->display_options['menu']['context_only_inline'] = 0;

  /* Display: Clinic */
  $handler = $view->new_display('page', 'Clinic', 'page_1');
  $handler->display->display_options['defaults']['title'] = FALSE;
  $handler->display->display_options['title'] = 'Clinics';
  $handler->display->display_options['defaults']['relationships'] = FALSE;
  /* Relationship: Author */
  $handler->display->display_options['relationships']['uid']['id'] = 'uid';
  $handler->display->display_options['relationships']['uid']['table'] = 'node';
  $handler->display->display_options['relationships']['uid']['field'] = 'uid';
  $handler->display->display_options['relationships']['uid']['ui_name'] = 'Author';
  $handler->display->display_options['relationships']['uid']['label'] = 'Author';
  /* Relationship: Referencing clinic */
  $handler->display->display_options['relationships']['reverse_field_clinic_node']['id'] = 'reverse_field_clinic_node';
  $handler->display->display_options['relationships']['reverse_field_clinic_node']['table'] = 'node';
  $handler->display->display_options['relationships']['reverse_field_clinic_node']['field'] = 'reverse_field_clinic_node';
  $handler->display->display_options['relationships']['reverse_field_clinic_node']['ui_name'] = 'Referencing clinic';
  $handler->display->display_options['relationships']['reverse_field_clinic_node']['label'] = 'Referencing clinic';
  /* Relationship: Adult */
  $handler->display->display_options['relationships']['field_adult_target_id']['id'] = 'field_adult_target_id';
  $handler->display->display_options['relationships']['field_adult_target_id']['table'] = 'field_data_field_adult';
  $handler->display->display_options['relationships']['field_adult_target_id']['field'] = 'field_adult_target_id';
  $handler->display->display_options['relationships']['field_adult_target_id']['relationship'] = 'reverse_field_clinic_node';
  $handler->display->display_options['relationships']['field_adult_target_id']['ui_name'] = 'Adult';
  $handler->display->display_options['relationships']['field_adult_target_id']['label'] = 'Adult';
  $handler->display->display_options['defaults']['fields'] = FALSE;
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title']['id'] = 'title';
  $handler->display->display_options['fields']['title']['table'] = 'node';
  $handler->display->display_options['fields']['title']['field'] = 'title';
  $handler->display->display_options['fields']['title']['label'] = 'Clinic name';
  $handler->display->display_options['fields']['title']['link_to_node'] = FALSE;
  /* Field: COUNT(Content: Nid) */
  $handler->display->display_options['fields']['nid']['id'] = 'nid';
  $handler->display->display_options['fields']['nid']['table'] = 'node';
  $handler->display->display_options['fields']['nid']['field'] = 'nid';
  $handler->display->display_options['fields']['nid']['relationship'] = 'field_adult_target_id';
  $handler->display->display_options['fields']['nid']['group_type'] = 'count';
  $handler->display->display_options['fields']['nid']['label'] = 'Mothers/Caregivers assigned';
  /* Field: MAX(Content: Scheduled Date) */
  $handler->display->display_options['fields']['field_scheduled_date']['id'] = 'field_scheduled_date';
  $handler->display->display_options['fields']['field_scheduled_date']['table'] = 'field_data_field_scheduled_date';
  $handler->display->display_options['fields']['field_scheduled_date']['field'] = 'field_scheduled_date';
  $handler->display->display_options['fields']['field_scheduled_date']['relationship'] = 'reverse_field_clinic_node';
  $handler->display->display_options['fields']['field_scheduled_date']['group_type'] = 'max';
  $handler->display->display_options['fields']['field_scheduled_date']['label'] = 'Latest session';
  $handler->display->display_options['fields']['field_scheduled_date']['settings'] = array(
    'format_type' => 'custom',
    'custom_date_format' => 'Y M d',
    'fromto' => 'value',
    'multiple_number' => '',
    'multiple_from' => '',
    'multiple_to' => '',
    'show_remaining_days' => 0,
  );
  /* Field: User: Name */
  $handler->display->display_options['fields']['name']['id'] = 'name';
  $handler->display->display_options['fields']['name']['table'] = 'users';
  $handler->display->display_options['fields']['name']['field'] = 'name';
  $handler->display->display_options['fields']['name']['relationship'] = 'uid';
  $handler->display->display_options['fields']['name']['label'] = 'Created by';
  /* Field: Content: Edit link */
  $handler->display->display_options['fields']['edit_node']['id'] = 'edit_node';
  $handler->display->display_options['fields']['edit_node']['table'] = 'views_entity_node';
  $handler->display->display_options['fields']['edit_node']['field'] = 'edit_node';
  $handler->display->display_options['fields']['edit_node']['label'] = 'Edit';
  $handler->display->display_options['defaults']['filter_groups'] = FALSE;
  $handler->display->display_options['defaults']['filters'] = FALSE;
  /* Filter criterion: Content: Published */
  $handler->display->display_options['filters']['status']['id'] = 'status';
  $handler->display->display_options['filters']['status']['table'] = 'node';
  $handler->display->display_options['filters']['status']['field'] = 'status';
  $handler->display->display_options['filters']['status']['value'] = 1;
  $handler->display->display_options['filters']['status']['group'] = 1;
  $handler->display->display_options['filters']['status']['expose']['operator'] = FALSE;
  /* Filter criterion: Content: Type */
  $handler->display->display_options['filters']['type']['id'] = 'type';
  $handler->display->display_options['filters']['type']['table'] = 'node';
  $handler->display->display_options['filters']['type']['field'] = 'type';
  $handler->display->display_options['filters']['type']['value'] = array(
    'clinic' => 'clinic',
  );
  $handler->display->display_options['filters']['type']['group'] = 1;
  $handler->display->display_options['path'] = 'admin/content/clinics';
  $handler->display->display_options['menu']['type'] = 'normal';
  $handler->display->display_options['menu']['title'] = 'Clinics';
  $handler->display->display_options['menu']['weight'] = '2';
  $handler->display->display_options['menu']['name'] = 'main-menu';
  $handler->display->display_options['menu']['context'] = 0;
  $handler->display->display_options['menu']['context_only_inline'] = 0;

  /* Display: Session */
  $handler = $view->new_display('page', 'Session', 'page_2');
  $handler->display->display_options['defaults']['title'] = FALSE;
  $handler->display->display_options['title'] = 'Sessions';
  $handler->display->display_options['defaults']['relationships'] = FALSE;
  /* Relationship: Entity Reference: Referenced Entity */
  $handler->display->display_options['relationships']['field_clinic_target_id']['id'] = 'field_clinic_target_id';
  $handler->display->display_options['relationships']['field_clinic_target_id']['table'] = 'field_data_field_clinic';
  $handler->display->display_options['relationships']['field_clinic_target_id']['field'] = 'field_clinic_target_id';
  $handler->display->display_options['relationships']['field_clinic_target_id']['label'] = 'Clinic';
  $handler->display->display_options['defaults']['fields'] = FALSE;
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title_1']['id'] = 'title_1';
  $handler->display->display_options['fields']['title_1']['table'] = 'node';
  $handler->display->display_options['fields']['title_1']['field'] = 'title';
  $handler->display->display_options['fields']['title_1']['label'] = 'Session';
  /* Field: Content: Nid */
  $handler->display->display_options['fields']['nid']['id'] = 'nid';
  $handler->display->display_options['fields']['nid']['table'] = 'node';
  $handler->display->display_options['fields']['nid']['field'] = 'nid';
  $handler->display->display_options['fields']['nid']['label'] = 'Session ID';
  /* Field: Content: Scheduled Date */
  $handler->display->display_options['fields']['field_scheduled_date']['id'] = 'field_scheduled_date';
  $handler->display->display_options['fields']['field_scheduled_date']['table'] = 'field_data_field_scheduled_date';
  $handler->display->display_options['fields']['field_scheduled_date']['field'] = 'field_scheduled_date';
  $handler->display->display_options['fields']['field_scheduled_date']['exclude'] = TRUE;
  $handler->display->display_options['fields']['field_scheduled_date']['element_class'] = 'nowrap';
  $handler->display->display_options['fields']['field_scheduled_date']['settings'] = array(
    'format_type' => 'custom',
    'custom_date_format' => 'd-m-Y',
    'fromto' => 'value',
    'multiple_number' => '',
    'multiple_from' => '',
    'multiple_to' => '',
    'show_remaining_days' => 0,
  );
  /* Field: Content: Scheduled Date */
  $handler->display->display_options['fields']['field_scheduled_date_1']['id'] = 'field_scheduled_date_1';
  $handler->display->display_options['fields']['field_scheduled_date_1']['table'] = 'field_data_field_scheduled_date';
  $handler->display->display_options['fields']['field_scheduled_date_1']['field'] = 'field_scheduled_date';
  $handler->display->display_options['fields']['field_scheduled_date_1']['alter']['alter_text'] = TRUE;
  $handler->display->display_options['fields']['field_scheduled_date_1']['alter']['text'] = '[field_scheduled_date] – [field_scheduled_date_1] ';
  $handler->display->display_options['fields']['field_scheduled_date_1']['element_class'] = 'nowrap';
  $handler->display->display_options['fields']['field_scheduled_date_1']['settings'] = array(
    'format_type' => 'custom',
    'custom_date_format' => 'd-m-Y',
    'fromto' => 'value2',
    'multiple_number' => '',
    'multiple_from' => '',
    'multiple_to' => '',
    'show_remaining_days' => 0,
  );
  $handler->display->display_options['fields']['field_scheduled_date_1']['group_column'] = 'value2';
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title']['id'] = 'title';
  $handler->display->display_options['fields']['title']['table'] = 'node';
  $handler->display->display_options['fields']['title']['field'] = 'title';
  $handler->display->display_options['fields']['title']['relationship'] = 'field_clinic_target_id';
  $handler->display->display_options['fields']['title']['label'] = 'Clinic';
  $handler->display->display_options['fields']['title']['link_to_node'] = FALSE;
  /* Field: Content: Closed */
  $handler->display->display_options['fields']['field_closed']['id'] = 'field_closed';
  $handler->display->display_options['fields']['field_closed']['table'] = 'field_data_field_closed';
  $handler->display->display_options['fields']['field_closed']['field'] = 'field_closed';
  $handler->display->display_options['fields']['field_closed']['label'] = 'Status';
  $handler->display->display_options['fields']['field_closed']['type'] = 'list_key';
  /* Field: Content: Post date */
  $handler->display->display_options['fields']['created']['id'] = 'created';
  $handler->display->display_options['fields']['created']['table'] = 'node';
  $handler->display->display_options['fields']['created']['field'] = 'created';
  $handler->display->display_options['fields']['created']['label'] = 'Uploaded';
  $handler->display->display_options['fields']['created']['element_class'] = 'nowrap';
  $handler->display->display_options['fields']['created']['date_format'] = 'custom';
  $handler->display->display_options['fields']['created']['custom_date_format'] = 'd-m-Y';
  $handler->display->display_options['fields']['created']['second_date_format'] = 'long';
  /* Field: Content: Edit link */
  $handler->display->display_options['fields']['edit_node']['id'] = 'edit_node';
  $handler->display->display_options['fields']['edit_node']['table'] = 'views_entity_node';
  $handler->display->display_options['fields']['edit_node']['field'] = 'edit_node';
  $handler->display->display_options['fields']['edit_node']['label'] = 'Edit';
  /* Field: Global: Custom text */
  $handler->display->display_options['fields']['nothing']['id'] = 'nothing';
  $handler->display->display_options['fields']['nothing']['table'] = 'views';
  $handler->display->display_options['fields']['nothing']['field'] = 'nothing';
  $handler->display->display_options['fields']['nothing']['label'] = 'Measurements';
  $handler->display->display_options['fields']['nothing']['alter']['text'] = 'Measurements';
  $handler->display->display_options['fields']['nothing']['alter']['make_link'] = TRUE;
  $handler->display->display_options['fields']['nothing']['alter']['path'] = '/admin/content/measurements/[nid]';
  $handler->display->display_options['defaults']['filter_groups'] = FALSE;
  $handler->display->display_options['defaults']['filters'] = FALSE;
  /* Filter criterion: Content: Published */
  $handler->display->display_options['filters']['status']['id'] = 'status';
  $handler->display->display_options['filters']['status']['table'] = 'node';
  $handler->display->display_options['filters']['status']['field'] = 'status';
  $handler->display->display_options['filters']['status']['value'] = 1;
  $handler->display->display_options['filters']['status']['group'] = 1;
  $handler->display->display_options['filters']['status']['expose']['operator'] = FALSE;
  /* Filter criterion: Content: Type */
  $handler->display->display_options['filters']['type']['id'] = 'type';
  $handler->display->display_options['filters']['type']['table'] = 'node';
  $handler->display->display_options['filters']['type']['field'] = 'type';
  $handler->display->display_options['filters']['type']['value'] = array(
    'session' => 'session',
  );
  $handler->display->display_options['path'] = 'admin/content/sessions';
  $handler->display->display_options['menu']['type'] = 'normal';
  $handler->display->display_options['menu']['title'] = 'Sessions';
  $handler->display->display_options['menu']['weight'] = '8';
  $handler->display->display_options['menu']['name'] = 'main-menu';
  $handler->display->display_options['menu']['context'] = 0;
  $handler->display->display_options['menu']['context_only_inline'] = 0;

  /* Display: Parent / Caregiver */
  $handler = $view->new_display('page', 'Parent / Caregiver', 'page_3');
  $handler->display->display_options['defaults']['title'] = FALSE;
  $handler->display->display_options['title'] = 'Parents / Caregivers';
  $handler->display->display_options['defaults']['empty'] = FALSE;
  /* No results behavior: Global: Text area */
  $handler->display->display_options['empty']['area']['id'] = 'area';
  $handler->display->display_options['empty']['area']['table'] = 'views';
  $handler->display->display_options['empty']['area']['field'] = 'area';
  $handler->display->display_options['empty']['area']['empty'] = TRUE;
  $handler->display->display_options['empty']['area']['content'] = 'No records exist for these criteria';
  $handler->display->display_options['empty']['area']['format'] = 'filtered_html';
  $handler->display->display_options['defaults']['relationships'] = FALSE;
  /* Relationship: Relationship as parent */
  $handler->display->display_options['relationships']['reverse_field_person_node']['id'] = 'reverse_field_person_node';
  $handler->display->display_options['relationships']['reverse_field_person_node']['table'] = 'node';
  $handler->display->display_options['relationships']['reverse_field_person_node']['field'] = 'reverse_field_person_node';
  $handler->display->display_options['relationships']['reverse_field_person_node']['ui_name'] = 'Relationship as parent';
  $handler->display->display_options['relationships']['reverse_field_person_node']['label'] = 'Relationship as parent';
  /* Relationship: Child */
  $handler->display->display_options['relationships']['field_related_to_target_id']['id'] = 'field_related_to_target_id';
  $handler->display->display_options['relationships']['field_related_to_target_id']['table'] = 'field_data_field_related_to';
  $handler->display->display_options['relationships']['field_related_to_target_id']['field'] = 'field_related_to_target_id';
  $handler->display->display_options['relationships']['field_related_to_target_id']['relationship'] = 'reverse_field_person_node';
  $handler->display->display_options['relationships']['field_related_to_target_id']['ui_name'] = 'Child';
  $handler->display->display_options['relationships']['field_related_to_target_id']['label'] = 'Child';
  /* Relationship: Relationship as child */
  $handler->display->display_options['relationships']['reverse_field_related_to_node']['id'] = 'reverse_field_related_to_node';
  $handler->display->display_options['relationships']['reverse_field_related_to_node']['table'] = 'node';
  $handler->display->display_options['relationships']['reverse_field_related_to_node']['field'] = 'reverse_field_related_to_node';
  $handler->display->display_options['relationships']['reverse_field_related_to_node']['ui_name'] = 'Relationship as child';
  $handler->display->display_options['relationships']['reverse_field_related_to_node']['label'] = 'Relationship as child';
  /* Relationship: Adult */
  $handler->display->display_options['relationships']['reverse_field_adult_node']['id'] = 'reverse_field_adult_node';
  $handler->display->display_options['relationships']['reverse_field_adult_node']['table'] = 'node';
  $handler->display->display_options['relationships']['reverse_field_adult_node']['field'] = 'reverse_field_adult_node';
  $handler->display->display_options['relationships']['reverse_field_adult_node']['ui_name'] = 'Adult';
  $handler->display->display_options['relationships']['reverse_field_adult_node']['label'] = 'Adult';
  $handler->display->display_options['defaults']['fields'] = FALSE;
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title']['id'] = 'title';
  $handler->display->display_options['fields']['title']['table'] = 'node';
  $handler->display->display_options['fields']['title']['field'] = 'title';
  $handler->display->display_options['fields']['title']['label'] = 'Name';
  $handler->display->display_options['fields']['title']['alter']['word_boundary'] = FALSE;
  $handler->display->display_options['fields']['title']['alter']['ellipsis'] = FALSE;
  /* Field: COUNT(Content: Nid) */
  $handler->display->display_options['fields']['nid']['id'] = 'nid';
  $handler->display->display_options['fields']['nid']['table'] = 'node';
  $handler->display->display_options['fields']['nid']['field'] = 'nid';
  $handler->display->display_options['fields']['nid']['relationship'] = 'field_related_to_target_id';
  $handler->display->display_options['fields']['nid']['group_type'] = 'count';
  $handler->display->display_options['fields']['nid']['label'] = 'Children';
  /* Field: MAX(Content: Clinic) */
  $handler->display->display_options['fields']['field_clinic']['id'] = 'field_clinic';
  $handler->display->display_options['fields']['field_clinic']['table'] = 'field_data_field_clinic';
  $handler->display->display_options['fields']['field_clinic']['field'] = 'field_clinic';
  $handler->display->display_options['fields']['field_clinic']['relationship'] = 'reverse_field_adult_node';
  $handler->display->display_options['fields']['field_clinic']['group_type'] = 'max';
  $handler->display->display_options['fields']['field_clinic']['settings'] = array(
    'bypass_access' => 0,
    'link' => 0,
  );
  /* Field: Content: Ubudehe */
  $handler->display->display_options['fields']['field_ubudehe']['id'] = 'field_ubudehe';
  $handler->display->display_options['fields']['field_ubudehe']['table'] = 'field_data_field_ubudehe';
  $handler->display->display_options['fields']['field_ubudehe']['field'] = 'field_ubudehe';
  /* Field: Content: Education Level */
  $handler->display->display_options['fields']['field_education_level']['id'] = 'field_education_level';
  $handler->display->display_options['fields']['field_education_level']['table'] = 'field_data_field_education_level';
  $handler->display->display_options['fields']['field_education_level']['field'] = 'field_education_level';
  /* Field: Content: Edit link */
  $handler->display->display_options['fields']['edit_node']['id'] = 'edit_node';
  $handler->display->display_options['fields']['edit_node']['table'] = 'views_entity_node';
  $handler->display->display_options['fields']['edit_node']['field'] = 'edit_node';
  $handler->display->display_options['fields']['edit_node']['label'] = 'Edit';
  $handler->display->display_options['defaults']['sorts'] = FALSE;
  /* Sort criterion: Content: Post date */
  $handler->display->display_options['sorts']['created']['id'] = 'created';
  $handler->display->display_options['sorts']['created']['table'] = 'node';
  $handler->display->display_options['sorts']['created']['field'] = 'created';
  $handler->display->display_options['sorts']['created']['order'] = 'DESC';
  /* Sort criterion: Content: Nid */
  $handler->display->display_options['sorts']['nid']['id'] = 'nid';
  $handler->display->display_options['sorts']['nid']['table'] = 'node';
  $handler->display->display_options['sorts']['nid']['field'] = 'nid';
  $handler->display->display_options['defaults']['filter_groups'] = FALSE;
  $handler->display->display_options['defaults']['filters'] = FALSE;
  /* Filter criterion: Content: Published */
  $handler->display->display_options['filters']['status']['id'] = 'status';
  $handler->display->display_options['filters']['status']['table'] = 'node';
  $handler->display->display_options['filters']['status']['field'] = 'status';
  $handler->display->display_options['filters']['status']['value'] = 1;
  $handler->display->display_options['filters']['status']['group'] = 1;
  $handler->display->display_options['filters']['status']['expose']['operator'] = FALSE;
  /* Filter criterion: Content: Type */
  $handler->display->display_options['filters']['type']['id'] = 'type';
  $handler->display->display_options['filters']['type']['table'] = 'node';
  $handler->display->display_options['filters']['type']['field'] = 'type';
  $handler->display->display_options['filters']['type']['value'] = array(
    'person' => 'person',
  );
  $handler->display->display_options['filters']['type']['group'] = 1;
  /* Filter criterion: COUNT(Content: Nid) */
  $handler->display->display_options['filters']['nid']['id'] = 'nid';
  $handler->display->display_options['filters']['nid']['table'] = 'node';
  $handler->display->display_options['filters']['nid']['field'] = 'nid';
  $handler->display->display_options['filters']['nid']['relationship'] = 'reverse_field_related_to_node';
  $handler->display->display_options['filters']['nid']['group_type'] = 'count';
  $handler->display->display_options['filters']['nid']['value']['value'] = '0';
  $handler->display->display_options['filters']['nid']['group'] = 1;
  /* Filter criterion: Parent name */
  $handler->display->display_options['filters']['title']['id'] = 'title';
  $handler->display->display_options['filters']['title']['table'] = 'node';
  $handler->display->display_options['filters']['title']['field'] = 'title';
  $handler->display->display_options['filters']['title']['ui_name'] = 'Parent name';
  $handler->display->display_options['filters']['title']['operator'] = 'word';
  $handler->display->display_options['filters']['title']['group'] = 1;
  $handler->display->display_options['filters']['title']['exposed'] = TRUE;
  $handler->display->display_options['filters']['title']['expose']['operator_id'] = 'title_op';
  $handler->display->display_options['filters']['title']['expose']['label'] = 'Parent/Caregiver name';
  $handler->display->display_options['filters']['title']['expose']['operator'] = 'title_op';
  $handler->display->display_options['filters']['title']['expose']['identifier'] = 'mother_name';
  $handler->display->display_options['filters']['title']['expose']['remember_roles'] = array(
    2 => '2',
    1 => 0,
    4 => 0,
    3 => 0,
    5 => 0,
  );
  /* Filter criterion: Content: Clinic (field_clinic) */
  $handler->display->display_options['filters']['field_clinic_target_id']['id'] = 'field_clinic_target_id';
  $handler->display->display_options['filters']['field_clinic_target_id']['table'] = 'field_data_field_clinic';
  $handler->display->display_options['filters']['field_clinic_target_id']['field'] = 'field_clinic_target_id';
  $handler->display->display_options['filters']['field_clinic_target_id']['relationship'] = 'reverse_field_adult_node';
  $handler->display->display_options['filters']['field_clinic_target_id']['group'] = 1;
  $handler->display->display_options['filters']['field_clinic_target_id']['exposed'] = TRUE;
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['operator_id'] = 'field_clinic_target_id_op';
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['label'] = 'Clinic';
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['operator'] = 'field_clinic_target_id_op';
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['identifier'] = 'field_clinic_target_id';
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['remember_roles'] = array(
    2 => '2',
    1 => 0,
    4 => 0,
    3 => 0,
    5 => 0,
  );
  $handler->display->display_options['path'] = 'admin/content/mothers';
  $handler->display->display_options['menu']['type'] = 'normal';
  $handler->display->display_options['menu']['title'] = 'Mothers / Caregivers';
  $handler->display->display_options['menu']['weight'] = '4';
  $handler->display->display_options['menu']['name'] = 'main-menu';
  $handler->display->display_options['menu']['context'] = 0;
  $handler->display->display_options['menu']['context_only_inline'] = 0;

  /* Display: Measurements */
  $handler = $view->new_display('page', 'Measurements', 'page_4');
  $handler->display->display_options['defaults']['title'] = FALSE;
  $handler->display->display_options['title'] = 'Measurements';
  $handler->display->display_options['defaults']['relationships'] = FALSE;
  /* Relationship: Entity Reference: Referenced Entity */
  $handler->display->display_options['relationships']['field_session_target_id']['id'] = 'field_session_target_id';
  $handler->display->display_options['relationships']['field_session_target_id']['table'] = 'field_data_field_session';
  $handler->display->display_options['relationships']['field_session_target_id']['field'] = 'field_session_target_id';
  $handler->display->display_options['relationships']['field_session_target_id']['label'] = 'Session';
  /* Relationship: Entity Reference: Referenced Entity */
  $handler->display->display_options['relationships']['field_clinic_target_id']['id'] = 'field_clinic_target_id';
  $handler->display->display_options['relationships']['field_clinic_target_id']['table'] = 'field_data_field_clinic';
  $handler->display->display_options['relationships']['field_clinic_target_id']['field'] = 'field_clinic_target_id';
  $handler->display->display_options['relationships']['field_clinic_target_id']['relationship'] = 'field_session_target_id';
  $handler->display->display_options['relationships']['field_clinic_target_id']['label'] = 'Clinic';
  $handler->display->display_options['defaults']['fields'] = FALSE;
  /* Field: Content: Nid */
  $handler->display->display_options['fields']['nid']['id'] = 'nid';
  $handler->display->display_options['fields']['nid']['table'] = 'node';
  $handler->display->display_options['fields']['nid']['field'] = 'nid';
  $handler->display->display_options['fields']['nid']['relationship'] = 'field_session_target_id';
  $handler->display->display_options['fields']['nid']['label'] = 'Session Id';
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title']['id'] = 'title';
  $handler->display->display_options['fields']['title']['table'] = 'node';
  $handler->display->display_options['fields']['title']['field'] = 'title';
  $handler->display->display_options['fields']['title']['relationship'] = 'field_clinic_target_id';
  $handler->display->display_options['fields']['title']['label'] = 'Clinic';
  $handler->display->display_options['fields']['title']['link_to_node'] = FALSE;
  /* Field: Content: Date Measured */
  $handler->display->display_options['fields']['field_date_measured']['id'] = 'field_date_measured';
  $handler->display->display_options['fields']['field_date_measured']['table'] = 'field_data_field_date_measured';
  $handler->display->display_options['fields']['field_date_measured']['field'] = 'field_date_measured';
  $handler->display->display_options['fields']['field_date_measured']['element_class'] = 'nowrap';
  $handler->display->display_options['fields']['field_date_measured']['settings'] = array(
    'format_type' => 'custom',
    'custom_date_format' => 'd-m-Y',
    'fromto' => 'both',
    'multiple_number' => '',
    'multiple_from' => '',
    'multiple_to' => '',
    'show_remaining_days' => 0,
  );
  /* Field: Content: Type */
  $handler->display->display_options['fields']['type']['id'] = 'type';
  $handler->display->display_options['fields']['type']['table'] = 'node';
  $handler->display->display_options['fields']['type']['field'] = 'type';
  $handler->display->display_options['fields']['type']['label'] = 'Measurement type';
  /* Field: Content: Nurse */
  $handler->display->display_options['fields']['field_nurse']['id'] = 'field_nurse';
  $handler->display->display_options['fields']['field_nurse']['table'] = 'field_data_field_nurse';
  $handler->display->display_options['fields']['field_nurse']['field'] = 'field_nurse';
  /* Field: Content: Family Planning */
  $handler->display->display_options['fields']['field_family_planning_signs']['id'] = 'field_family_planning_signs';
  $handler->display->display_options['fields']['field_family_planning_signs']['table'] = 'field_data_field_family_planning_signs';
  $handler->display->display_options['fields']['field_family_planning_signs']['field'] = 'field_family_planning_signs';
  $handler->display->display_options['fields']['field_family_planning_signs']['label'] = '';
  $handler->display->display_options['fields']['field_family_planning_signs']['exclude'] = TRUE;
  $handler->display->display_options['fields']['field_family_planning_signs']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_family_planning_signs']['group_column'] = 'entity_id';
  $handler->display->display_options['fields']['field_family_planning_signs']['delta_offset'] = '0';
  /* Field: Content: Height */
  $handler->display->display_options['fields']['field_height']['id'] = 'field_height';
  $handler->display->display_options['fields']['field_height']['table'] = 'field_data_field_height';
  $handler->display->display_options['fields']['field_height']['field'] = 'field_height';
  $handler->display->display_options['fields']['field_height']['label'] = '';
  $handler->display->display_options['fields']['field_height']['exclude'] = TRUE;
  $handler->display->display_options['fields']['field_height']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_height']['settings'] = array(
    'thousand_separator' => '',
    'decimal_separator' => '.',
    'scale' => '2',
    'prefix_suffix' => 1,
  );
  $handler->display->display_options['fields']['field_height']['group_column'] = 'entity_id';
  /* Field: Content: MUAC */
  $handler->display->display_options['fields']['field_muac']['id'] = 'field_muac';
  $handler->display->display_options['fields']['field_muac']['table'] = 'field_data_field_muac';
  $handler->display->display_options['fields']['field_muac']['field'] = 'field_muac';
  $handler->display->display_options['fields']['field_muac']['label'] = '';
  $handler->display->display_options['fields']['field_muac']['exclude'] = TRUE;
  $handler->display->display_options['fields']['field_muac']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_muac']['settings'] = array(
    'thousand_separator' => '',
    'decimal_separator' => '.',
    'scale' => '2',
    'prefix_suffix' => 1,
  );
  /* Field: Content: Nutrition Signs */
  $handler->display->display_options['fields']['field_nutrition_signs']['id'] = 'field_nutrition_signs';
  $handler->display->display_options['fields']['field_nutrition_signs']['table'] = 'field_data_field_nutrition_signs';
  $handler->display->display_options['fields']['field_nutrition_signs']['field'] = 'field_nutrition_signs';
  $handler->display->display_options['fields']['field_nutrition_signs']['label'] = '';
  $handler->display->display_options['fields']['field_nutrition_signs']['exclude'] = TRUE;
  $handler->display->display_options['fields']['field_nutrition_signs']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_nutrition_signs']['type'] = 'list_key';
  $handler->display->display_options['fields']['field_nutrition_signs']['delta_offset'] = '0';
  /* Field: Content: Photo */
  $handler->display->display_options['fields']['field_photo']['id'] = 'field_photo';
  $handler->display->display_options['fields']['field_photo']['table'] = 'field_data_field_photo';
  $handler->display->display_options['fields']['field_photo']['field'] = 'field_photo';
  $handler->display->display_options['fields']['field_photo']['label'] = '';
  $handler->display->display_options['fields']['field_photo']['exclude'] = TRUE;
  $handler->display->display_options['fields']['field_photo']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_photo']['click_sort_column'] = 'fid';
  $handler->display->display_options['fields']['field_photo']['settings'] = array(
    'image_style' => 'thumbnail',
    'image_link' => '',
  );
  $handler->display->display_options['fields']['field_photo']['group_column'] = 'entity_id';
  /* Field: Content: Weight */
  $handler->display->display_options['fields']['field_weight']['id'] = 'field_weight';
  $handler->display->display_options['fields']['field_weight']['table'] = 'field_data_field_weight';
  $handler->display->display_options['fields']['field_weight']['field'] = 'field_weight';
  $handler->display->display_options['fields']['field_weight']['label'] = '';
  $handler->display->display_options['fields']['field_weight']['exclude'] = TRUE;
  $handler->display->display_options['fields']['field_weight']['alter']['alter_text'] = TRUE;
  $handler->display->display_options['fields']['field_weight']['alter']['text'] = '[field_family_planning_signs] [field_height] [field_muac] [field_nutrition_signs] [field_photo] [field_weight]';
  $handler->display->display_options['fields']['field_weight']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_weight']['settings'] = array(
    'thousand_separator' => '',
    'decimal_separator' => '.',
    'scale' => '2',
    'prefix_suffix' => 1,
  );
  /* Field: Global: Custom text */
  $handler->display->display_options['fields']['nothing']['id'] = 'nothing';
  $handler->display->display_options['fields']['nothing']['table'] = 'views';
  $handler->display->display_options['fields']['nothing']['field'] = 'nothing';
  $handler->display->display_options['fields']['nothing']['label'] = 'Measurement value';
  $handler->display->display_options['fields']['nothing']['alter']['text'] = '[field_family_planning_signs] [field_height] [field_muac] [field_nutrition_signs] [field_photo] [field_weight]';
  $handler->display->display_options['defaults']['arguments'] = FALSE;
  /* Contextual filter: Content: Nid */
  $handler->display->display_options['arguments']['nid']['id'] = 'nid';
  $handler->display->display_options['arguments']['nid']['table'] = 'node';
  $handler->display->display_options['arguments']['nid']['field'] = 'nid';
  $handler->display->display_options['arguments']['nid']['relationship'] = 'field_session_target_id';
  $handler->display->display_options['arguments']['nid']['default_argument_type'] = 'fixed';
  $handler->display->display_options['arguments']['nid']['default_argument_skip_url'] = TRUE;
  $handler->display->display_options['arguments']['nid']['summary']['number_of_records'] = '0';
  $handler->display->display_options['arguments']['nid']['summary']['format'] = 'default_summary';
  $handler->display->display_options['arguments']['nid']['summary_options']['items_per_page'] = '25';
  $handler->display->display_options['defaults']['filter_groups'] = FALSE;
  $handler->display->display_options['defaults']['filters'] = FALSE;
  /* Filter criterion: Content: Published */
  $handler->display->display_options['filters']['status']['id'] = 'status';
  $handler->display->display_options['filters']['status']['table'] = 'node';
  $handler->display->display_options['filters']['status']['field'] = 'status';
  $handler->display->display_options['filters']['status']['value'] = 1;
  $handler->display->display_options['filters']['status']['group'] = 1;
  $handler->display->display_options['filters']['status']['expose']['operator'] = FALSE;
  /* Filter criterion: Content: Type */
  $handler->display->display_options['filters']['type']['id'] = 'type';
  $handler->display->display_options['filters']['type']['table'] = 'node';
  $handler->display->display_options['filters']['type']['field'] = 'type';
  $handler->display->display_options['filters']['type']['value'] = array(
    'family_planning' => 'family_planning',
    'height' => 'height',
    'muac' => 'muac',
    'nutrition' => 'nutrition',
    'photo' => 'photo',
    'weight' => 'weight',
  );
  $handler->display->display_options['path'] = 'admin/content/measurements';
  $handler->display->display_options['menu']['type'] = 'normal';
  $handler->display->display_options['menu']['title'] = 'Measurements';
  $handler->display->display_options['menu']['weight'] = '10';
  $handler->display->display_options['menu']['name'] = 'main-menu';
  $handler->display->display_options['menu']['context'] = 0;
  $handler->display->display_options['menu']['context_only_inline'] = 0;

  /* Display: Child CSV */
  $handler = $view->new_display('views_data_export', 'Child CSV', 'views_data_export_1');
  $handler->display->display_options['pager']['type'] = 'none';
  $handler->display->display_options['pager']['options']['offset'] = '0';
  $handler->display->display_options['style_plugin'] = 'views_data_export_csv';
  $handler->display->display_options['style_options']['provide_file'] = 1;
  $handler->display->display_options['style_options']['filename'] = 'children-%timestamp-yy-%timestamp-mm-%timestamp-dd.csv';
  $handler->display->display_options['style_options']['parent_sort'] = 0;
  $handler->display->display_options['style_options']['quote'] = 1;
  $handler->display->display_options['style_options']['trim'] = 0;
  $handler->display->display_options['style_options']['replace_newlines'] = 0;
  $handler->display->display_options['style_options']['newline_token'] = '1';
  $handler->display->display_options['style_options']['header'] = 1;
  $handler->display->display_options['style_options']['keep_html'] = 0;
  $handler->display->display_options['path'] = 'admin/content/childs/csv';
  $handler->display->display_options['displays'] = array(
    'page' => 'page',
    'default' => 0,
    'page_1' => 0,
    'page_2' => 0,
    'page_3' => 0,
    'page_4' => 0,
  );

  /* Display: Clinic CSV */
  $handler = $view->new_display('views_data_export', 'Clinic CSV', 'views_data_export_2');
  $handler->display->display_options['defaults']['title'] = FALSE;
  $handler->display->display_options['title'] = 'Clinic CSV';
  $handler->display->display_options['pager']['type'] = 'none';
  $handler->display->display_options['pager']['options']['offset'] = '0';
  $handler->display->display_options['style_plugin'] = 'views_data_export_csv';
  $handler->display->display_options['style_options']['provide_file'] = 1;
  $handler->display->display_options['style_options']['filename'] = 'clinics-%timestamp-yy-%timestamp-mm-%timestamp-dd.csv';
  $handler->display->display_options['style_options']['parent_sort'] = 0;
  $handler->display->display_options['style_options']['quote'] = 1;
  $handler->display->display_options['style_options']['trim'] = 0;
  $handler->display->display_options['style_options']['replace_newlines'] = 0;
  $handler->display->display_options['style_options']['newline_token'] = '1';
  $handler->display->display_options['style_options']['header'] = 1;
  $handler->display->display_options['style_options']['keep_html'] = 0;
  $handler->display->display_options['defaults']['relationships'] = FALSE;
  /* Relationship: Author */
  $handler->display->display_options['relationships']['uid']['id'] = 'uid';
  $handler->display->display_options['relationships']['uid']['table'] = 'node';
  $handler->display->display_options['relationships']['uid']['field'] = 'uid';
  $handler->display->display_options['relationships']['uid']['ui_name'] = 'Author';
  $handler->display->display_options['relationships']['uid']['label'] = 'Author';
  /* Relationship: Referencing clinic */
  $handler->display->display_options['relationships']['reverse_field_clinic_node']['id'] = 'reverse_field_clinic_node';
  $handler->display->display_options['relationships']['reverse_field_clinic_node']['table'] = 'node';
  $handler->display->display_options['relationships']['reverse_field_clinic_node']['field'] = 'reverse_field_clinic_node';
  $handler->display->display_options['relationships']['reverse_field_clinic_node']['ui_name'] = 'Referencing clinic';
  $handler->display->display_options['relationships']['reverse_field_clinic_node']['label'] = 'Referencing clinic';
  /* Relationship: Adult */
  $handler->display->display_options['relationships']['field_adult_target_id']['id'] = 'field_adult_target_id';
  $handler->display->display_options['relationships']['field_adult_target_id']['table'] = 'field_data_field_adult';
  $handler->display->display_options['relationships']['field_adult_target_id']['field'] = 'field_adult_target_id';
  $handler->display->display_options['relationships']['field_adult_target_id']['relationship'] = 'reverse_field_clinic_node';
  $handler->display->display_options['relationships']['field_adult_target_id']['ui_name'] = 'Adult';
  $handler->display->display_options['relationships']['field_adult_target_id']['label'] = 'Adult';
  $handler->display->display_options['defaults']['fields'] = FALSE;
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title']['id'] = 'title';
  $handler->display->display_options['fields']['title']['table'] = 'node';
  $handler->display->display_options['fields']['title']['field'] = 'title';
  $handler->display->display_options['fields']['title']['label'] = 'Clinic name';
  $handler->display->display_options['fields']['title']['link_to_node'] = FALSE;
  /* Field: COUNT(Content: Nid) */
  $handler->display->display_options['fields']['nid']['id'] = 'nid';
  $handler->display->display_options['fields']['nid']['table'] = 'node';
  $handler->display->display_options['fields']['nid']['field'] = 'nid';
  $handler->display->display_options['fields']['nid']['relationship'] = 'field_adult_target_id';
  $handler->display->display_options['fields']['nid']['group_type'] = 'count';
  $handler->display->display_options['fields']['nid']['label'] = 'Mothers Assigned';
  /* Field: User: Name */
  $handler->display->display_options['fields']['name']['id'] = 'name';
  $handler->display->display_options['fields']['name']['table'] = 'users';
  $handler->display->display_options['fields']['name']['field'] = 'name';
  $handler->display->display_options['fields']['name']['relationship'] = 'uid';
  $handler->display->display_options['fields']['name']['label'] = 'Created by';
  $handler->display->display_options['defaults']['filter_groups'] = FALSE;
  $handler->display->display_options['defaults']['filters'] = FALSE;
  /* Filter criterion: Content: Published */
  $handler->display->display_options['filters']['status']['id'] = 'status';
  $handler->display->display_options['filters']['status']['table'] = 'node';
  $handler->display->display_options['filters']['status']['field'] = 'status';
  $handler->display->display_options['filters']['status']['value'] = 1;
  $handler->display->display_options['filters']['status']['group'] = 1;
  $handler->display->display_options['filters']['status']['expose']['operator'] = FALSE;
  /* Filter criterion: Content: Type */
  $handler->display->display_options['filters']['type']['id'] = 'type';
  $handler->display->display_options['filters']['type']['table'] = 'node';
  $handler->display->display_options['filters']['type']['field'] = 'type';
  $handler->display->display_options['filters']['type']['value'] = array(
    'clinic' => 'clinic',
  );
  $handler->display->display_options['path'] = 'admin/content/clinics/csv';
  $handler->display->display_options['displays'] = array(
    'page_1' => 'page_1',
    'default' => 0,
    'page' => 0,
    'page_2' => 0,
    'page_3' => 0,
    'page_4' => 0,
  );
  $handler->display->display_options['sitename_title'] = 0;

  /* Display: Session CSV */
  $handler = $view->new_display('views_data_export', 'Session CSV', 'views_data_export_3');
  $handler->display->display_options['defaults']['title'] = FALSE;
  $handler->display->display_options['title'] = 'Session CSV';
  $handler->display->display_options['pager']['type'] = 'none';
  $handler->display->display_options['pager']['options']['offset'] = '0';
  $handler->display->display_options['style_plugin'] = 'views_data_export_csv';
  $handler->display->display_options['style_options']['provide_file'] = 1;
  $handler->display->display_options['style_options']['filename'] = 'sessions-%timestamp-yy-%timestamp-mm-%timestamp-dd.csv';
  $handler->display->display_options['style_options']['parent_sort'] = 0;
  $handler->display->display_options['style_options']['quote'] = 1;
  $handler->display->display_options['style_options']['trim'] = 0;
  $handler->display->display_options['style_options']['replace_newlines'] = 0;
  $handler->display->display_options['style_options']['newline_token'] = '1';
  $handler->display->display_options['style_options']['header'] = 1;
  $handler->display->display_options['style_options']['keep_html'] = 0;
  $handler->display->display_options['defaults']['relationships'] = FALSE;
  /* Relationship: Clinic */
  $handler->display->display_options['relationships']['field_clinic_target_id']['id'] = 'field_clinic_target_id';
  $handler->display->display_options['relationships']['field_clinic_target_id']['table'] = 'field_data_field_clinic';
  $handler->display->display_options['relationships']['field_clinic_target_id']['field'] = 'field_clinic_target_id';
  $handler->display->display_options['relationships']['field_clinic_target_id']['ui_name'] = 'Clinic';
  $handler->display->display_options['relationships']['field_clinic_target_id']['label'] = 'Clinic';
  $handler->display->display_options['defaults']['fields'] = FALSE;
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title']['id'] = 'title';
  $handler->display->display_options['fields']['title']['table'] = 'node';
  $handler->display->display_options['fields']['title']['field'] = 'title';
  $handler->display->display_options['fields']['title']['label'] = 'Session';
  $handler->display->display_options['fields']['title']['link_to_node'] = FALSE;
  /* Field: Content: Scheduled Date */
  $handler->display->display_options['fields']['field_scheduled_date']['id'] = 'field_scheduled_date';
  $handler->display->display_options['fields']['field_scheduled_date']['table'] = 'field_data_field_scheduled_date';
  $handler->display->display_options['fields']['field_scheduled_date']['field'] = 'field_scheduled_date';
  $handler->display->display_options['fields']['field_scheduled_date']['label'] = 'Start Date';
  $handler->display->display_options['fields']['field_scheduled_date']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_scheduled_date']['settings'] = array(
    'format_type' => 'custom',
    'custom_date_format' => 'd-m-Y',
    'fromto' => 'value',
    'multiple_number' => '',
    'multiple_from' => '',
    'multiple_to' => '',
    'show_remaining_days' => 0,
  );
  /* Field: Content: Scheduled Date */
  $handler->display->display_options['fields']['field_scheduled_date_1']['id'] = 'field_scheduled_date_1';
  $handler->display->display_options['fields']['field_scheduled_date_1']['table'] = 'field_data_field_scheduled_date';
  $handler->display->display_options['fields']['field_scheduled_date_1']['field'] = 'field_scheduled_date';
  $handler->display->display_options['fields']['field_scheduled_date_1']['label'] = 'End Date';
  $handler->display->display_options['fields']['field_scheduled_date_1']['alter']['text'] = '[field_scheduled_date] - [field_scheduled_date_1] ';
  $handler->display->display_options['fields']['field_scheduled_date_1']['settings'] = array(
    'format_type' => 'custom',
    'custom_date_format' => 'd-m-Y',
    'fromto' => 'value2',
    'multiple_number' => '',
    'multiple_from' => '',
    'multiple_to' => '',
    'show_remaining_days' => 0,
  );
  $handler->display->display_options['fields']['field_scheduled_date_1']['group_column'] = 'value2';
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title_1']['id'] = 'title_1';
  $handler->display->display_options['fields']['title_1']['table'] = 'node';
  $handler->display->display_options['fields']['title_1']['field'] = 'title';
  $handler->display->display_options['fields']['title_1']['relationship'] = 'field_clinic_target_id';
  $handler->display->display_options['fields']['title_1']['label'] = 'Clinic';
  $handler->display->display_options['fields']['title_1']['link_to_node'] = FALSE;
  /* Field: Content: Closed */
  $handler->display->display_options['fields']['field_closed']['id'] = 'field_closed';
  $handler->display->display_options['fields']['field_closed']['table'] = 'field_data_field_closed';
  $handler->display->display_options['fields']['field_closed']['field'] = 'field_closed';
  $handler->display->display_options['fields']['field_closed']['label'] = 'Status';
  $handler->display->display_options['fields']['field_closed']['type'] = 'list_key';
  /* Field: Content: Post date */
  $handler->display->display_options['fields']['created']['id'] = 'created';
  $handler->display->display_options['fields']['created']['table'] = 'node';
  $handler->display->display_options['fields']['created']['field'] = 'created';
  $handler->display->display_options['fields']['created']['label'] = 'Uploaded';
  $handler->display->display_options['fields']['created']['date_format'] = 'custom';
  $handler->display->display_options['fields']['created']['custom_date_format'] = 'd/m/Y';
  $handler->display->display_options['fields']['created']['second_date_format'] = 'long';
  $handler->display->display_options['defaults']['filter_groups'] = FALSE;
  $handler->display->display_options['defaults']['filters'] = FALSE;
  /* Filter criterion: Content: Published */
  $handler->display->display_options['filters']['status']['id'] = 'status';
  $handler->display->display_options['filters']['status']['table'] = 'node';
  $handler->display->display_options['filters']['status']['field'] = 'status';
  $handler->display->display_options['filters']['status']['value'] = 1;
  $handler->display->display_options['filters']['status']['group'] = 1;
  $handler->display->display_options['filters']['status']['expose']['operator'] = FALSE;
  /* Filter criterion: Content: Type */
  $handler->display->display_options['filters']['type']['id'] = 'type';
  $handler->display->display_options['filters']['type']['table'] = 'node';
  $handler->display->display_options['filters']['type']['field'] = 'type';
  $handler->display->display_options['filters']['type']['value'] = array(
    'session' => 'session',
  );
  $handler->display->display_options['path'] = 'admin/content/sessions/csv';
  $handler->display->display_options['displays'] = array(
    'page_2' => 'page_2',
    'default' => 0,
    'page' => 0,
    'page_1' => 0,
    'page_3' => 0,
    'page_4' => 0,
  );
  $handler->display->display_options['sitename_title'] = 0;

  /* Display: Mother CSV */
  $handler = $view->new_display('views_data_export', 'Mother CSV', 'views_data_export_4');
  $handler->display->display_options['defaults']['title'] = FALSE;
  $handler->display->display_options['title'] = 'Mother';
  $handler->display->display_options['pager']['type'] = 'none';
  $handler->display->display_options['pager']['options']['offset'] = '0';
  $handler->display->display_options['style_plugin'] = 'views_data_export_csv';
  $handler->display->display_options['style_options']['provide_file'] = 1;
  $handler->display->display_options['style_options']['filename'] = 'mothers-%timestamp-yy-%timestamp-mm-%timestamp-dd.csv';
  $handler->display->display_options['style_options']['parent_sort'] = 0;
  $handler->display->display_options['style_options']['quote'] = 1;
  $handler->display->display_options['style_options']['trim'] = 0;
  $handler->display->display_options['style_options']['replace_newlines'] = 0;
  $handler->display->display_options['style_options']['newline_token'] = '1';
  $handler->display->display_options['style_options']['header'] = 1;
  $handler->display->display_options['style_options']['keep_html'] = 0;
  $handler->display->display_options['defaults']['relationships'] = FALSE;
  /* Relationship: Relationship as parent */
  $handler->display->display_options['relationships']['reverse_field_person_node']['id'] = 'reverse_field_person_node';
  $handler->display->display_options['relationships']['reverse_field_person_node']['table'] = 'node';
  $handler->display->display_options['relationships']['reverse_field_person_node']['field'] = 'reverse_field_person_node';
  $handler->display->display_options['relationships']['reverse_field_person_node']['ui_name'] = 'Relationship as parent';
  $handler->display->display_options['relationships']['reverse_field_person_node']['label'] = 'Relationship as parent';
  /* Relationship: Child */
  $handler->display->display_options['relationships']['field_related_to_target_id']['id'] = 'field_related_to_target_id';
  $handler->display->display_options['relationships']['field_related_to_target_id']['table'] = 'field_data_field_related_to';
  $handler->display->display_options['relationships']['field_related_to_target_id']['field'] = 'field_related_to_target_id';
  $handler->display->display_options['relationships']['field_related_to_target_id']['relationship'] = 'reverse_field_person_node';
  $handler->display->display_options['relationships']['field_related_to_target_id']['ui_name'] = 'Child';
  $handler->display->display_options['relationships']['field_related_to_target_id']['label'] = 'Child';
  /* Relationship: Relationship as child */
  $handler->display->display_options['relationships']['reverse_field_related_to_node']['id'] = 'reverse_field_related_to_node';
  $handler->display->display_options['relationships']['reverse_field_related_to_node']['table'] = 'node';
  $handler->display->display_options['relationships']['reverse_field_related_to_node']['field'] = 'reverse_field_related_to_node';
  $handler->display->display_options['relationships']['reverse_field_related_to_node']['ui_name'] = 'Relationship as child';
  $handler->display->display_options['relationships']['reverse_field_related_to_node']['label'] = 'Relationship as child';
  /* Relationship: Adult */
  $handler->display->display_options['relationships']['reverse_field_adult_node']['id'] = 'reverse_field_adult_node';
  $handler->display->display_options['relationships']['reverse_field_adult_node']['table'] = 'node';
  $handler->display->display_options['relationships']['reverse_field_adult_node']['field'] = 'reverse_field_adult_node';
  $handler->display->display_options['relationships']['reverse_field_adult_node']['ui_name'] = 'Adult';
  $handler->display->display_options['relationships']['reverse_field_adult_node']['label'] = 'Adult';
  $handler->display->display_options['defaults']['fields'] = FALSE;
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title']['id'] = 'title';
  $handler->display->display_options['fields']['title']['table'] = 'node';
  $handler->display->display_options['fields']['title']['field'] = 'title';
  $handler->display->display_options['fields']['title']['label'] = 'Name';
  $handler->display->display_options['fields']['title']['link_to_node'] = FALSE;
  /* Field: COUNT(Content: Nid) */
  $handler->display->display_options['fields']['nid']['id'] = 'nid';
  $handler->display->display_options['fields']['nid']['table'] = 'node';
  $handler->display->display_options['fields']['nid']['field'] = 'nid';
  $handler->display->display_options['fields']['nid']['relationship'] = 'field_related_to_target_id';
  $handler->display->display_options['fields']['nid']['group_type'] = 'count';
  $handler->display->display_options['fields']['nid']['label'] = 'Children';
  /* Field: MAX(Content: Clinic) */
  $handler->display->display_options['fields']['field_clinic']['id'] = 'field_clinic';
  $handler->display->display_options['fields']['field_clinic']['table'] = 'field_data_field_clinic';
  $handler->display->display_options['fields']['field_clinic']['field'] = 'field_clinic';
  $handler->display->display_options['fields']['field_clinic']['relationship'] = 'reverse_field_adult_node';
  $handler->display->display_options['fields']['field_clinic']['group_type'] = 'max';
  $handler->display->display_options['fields']['field_clinic']['settings'] = array(
    'bypass_access' => 0,
    'link' => 0,
  );
  /* Field: Content: Ubudehe */
  $handler->display->display_options['fields']['field_ubudehe']['id'] = 'field_ubudehe';
  $handler->display->display_options['fields']['field_ubudehe']['table'] = 'field_data_field_ubudehe';
  $handler->display->display_options['fields']['field_ubudehe']['field'] = 'field_ubudehe';
  /* Field: Content: Education Level */
  $handler->display->display_options['fields']['field_education_level']['id'] = 'field_education_level';
  $handler->display->display_options['fields']['field_education_level']['table'] = 'field_data_field_education_level';
  $handler->display->display_options['fields']['field_education_level']['field'] = 'field_education_level';
  $handler->display->display_options['defaults']['filter_groups'] = FALSE;
  $handler->display->display_options['defaults']['filters'] = FALSE;
  /* Filter criterion: Content: Published */
  $handler->display->display_options['filters']['status']['id'] = 'status';
  $handler->display->display_options['filters']['status']['table'] = 'node';
  $handler->display->display_options['filters']['status']['field'] = 'status';
  $handler->display->display_options['filters']['status']['value'] = 1;
  $handler->display->display_options['filters']['status']['group'] = 1;
  $handler->display->display_options['filters']['status']['expose']['operator'] = FALSE;
  /* Filter criterion: Content: Type */
  $handler->display->display_options['filters']['type']['id'] = 'type';
  $handler->display->display_options['filters']['type']['table'] = 'node';
  $handler->display->display_options['filters']['type']['field'] = 'type';
  $handler->display->display_options['filters']['type']['value'] = array(
    'person' => 'person',
  );
  /* Filter criterion: Mother name */
  $handler->display->display_options['filters']['title']['id'] = 'title';
  $handler->display->display_options['filters']['title']['table'] = 'node';
  $handler->display->display_options['filters']['title']['field'] = 'title';
  $handler->display->display_options['filters']['title']['ui_name'] = 'Mother name';
  $handler->display->display_options['filters']['title']['exposed'] = TRUE;
  $handler->display->display_options['filters']['title']['expose']['operator_id'] = 'title_op';
  $handler->display->display_options['filters']['title']['expose']['label'] = 'Mother name';
  $handler->display->display_options['filters']['title']['expose']['operator'] = 'title_op';
  $handler->display->display_options['filters']['title']['expose']['identifier'] = 'mother_name';
  $handler->display->display_options['filters']['title']['expose']['remember_roles'] = array(
    2 => '2',
    1 => 0,
    3 => 0,
    4 => 0,
  );
  /* Filter criterion: Content: Clinic (field_clinic) */
  $handler->display->display_options['filters']['field_clinic_target_id']['id'] = 'field_clinic_target_id';
  $handler->display->display_options['filters']['field_clinic_target_id']['table'] = 'field_data_field_clinic';
  $handler->display->display_options['filters']['field_clinic_target_id']['field'] = 'field_clinic_target_id';
  $handler->display->display_options['filters']['field_clinic_target_id']['relationship'] = 'reverse_field_adult_node';
  $handler->display->display_options['filters']['field_clinic_target_id']['exposed'] = TRUE;
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['operator_id'] = 'field_clinic_target_id_op';
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['label'] = 'Clinic';
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['operator'] = 'field_clinic_target_id_op';
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['identifier'] = 'field_clinic_target_id';
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['remember_roles'] = array(
    2 => '2',
    1 => 0,
    4 => 0,
    3 => 0,
    5 => 0,
  );
  /* Filter criterion: COUNT(Content: Nid) */
  $handler->display->display_options['filters']['nid']['id'] = 'nid';
  $handler->display->display_options['filters']['nid']['table'] = 'node';
  $handler->display->display_options['filters']['nid']['field'] = 'nid';
  $handler->display->display_options['filters']['nid']['relationship'] = 'reverse_field_related_to_node';
  $handler->display->display_options['filters']['nid']['group_type'] = 'count';
  $handler->display->display_options['filters']['nid']['value']['value'] = '0';
  $handler->display->display_options['path'] = 'admin/content/mothers/csv';
  $handler->display->display_options['displays'] = array(
    'page_3' => 'page_3',
    'default' => 0,
    'page' => 0,
    'page_1' => 0,
    'page_2' => 0,
    'page_4' => 0,
  );
  $handler->display->display_options['sitename_title'] = 0;

  /* Display: Measurements CSV */
  $handler = $view->new_display('views_data_export', 'Measurements CSV', 'views_data_export_5');
  $handler->display->display_options['defaults']['title'] = FALSE;
  $handler->display->display_options['title'] = 'Measurements';
  $handler->display->display_options['pager']['type'] = 'none';
  $handler->display->display_options['pager']['options']['offset'] = '0';
  $handler->display->display_options['style_plugin'] = 'views_data_export_csv';
  $handler->display->display_options['style_options']['provide_file'] = 1;
  $handler->display->display_options['style_options']['filename'] = 'measurements-%timestamp-yy-%timestamp-mm-%timestamp-dd.csv';
  $handler->display->display_options['style_options']['parent_sort'] = 0;
  $handler->display->display_options['style_options']['quote'] = 1;
  $handler->display->display_options['style_options']['trim'] = 0;
  $handler->display->display_options['style_options']['replace_newlines'] = 0;
  $handler->display->display_options['style_options']['newline_token'] = '1';
  $handler->display->display_options['style_options']['header'] = 1;
  $handler->display->display_options['style_options']['keep_html'] = 0;
  $handler->display->display_options['defaults']['relationships'] = FALSE;
  /* Relationship: Session */
  $handler->display->display_options['relationships']['field_session_target_id']['id'] = 'field_session_target_id';
  $handler->display->display_options['relationships']['field_session_target_id']['table'] = 'field_data_field_session';
  $handler->display->display_options['relationships']['field_session_target_id']['field'] = 'field_session_target_id';
  $handler->display->display_options['relationships']['field_session_target_id']['ui_name'] = 'Session';
  $handler->display->display_options['relationships']['field_session_target_id']['label'] = 'Session';
  /* Relationship: Clinic */
  $handler->display->display_options['relationships']['field_clinic_target_id']['id'] = 'field_clinic_target_id';
  $handler->display->display_options['relationships']['field_clinic_target_id']['table'] = 'field_data_field_clinic';
  $handler->display->display_options['relationships']['field_clinic_target_id']['field'] = 'field_clinic_target_id';
  $handler->display->display_options['relationships']['field_clinic_target_id']['relationship'] = 'field_session_target_id';
  $handler->display->display_options['relationships']['field_clinic_target_id']['ui_name'] = 'Clinic';
  $handler->display->display_options['relationships']['field_clinic_target_id']['label'] = 'Clinic';
  $handler->display->display_options['defaults']['fields'] = FALSE;
  /* Field: Content: Nid */
  $handler->display->display_options['fields']['nid']['id'] = 'nid';
  $handler->display->display_options['fields']['nid']['table'] = 'node';
  $handler->display->display_options['fields']['nid']['field'] = 'nid';
  $handler->display->display_options['fields']['nid']['relationship'] = 'field_session_target_id';
  $handler->display->display_options['fields']['nid']['label'] = 'Session Id';
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title']['id'] = 'title';
  $handler->display->display_options['fields']['title']['table'] = 'node';
  $handler->display->display_options['fields']['title']['field'] = 'title';
  $handler->display->display_options['fields']['title']['relationship'] = 'field_clinic_target_id';
  $handler->display->display_options['fields']['title']['label'] = 'Clinic';
  $handler->display->display_options['fields']['title']['link_to_node'] = FALSE;
  /* Field: Content: Date Measured */
  $handler->display->display_options['fields']['field_date_measured']['id'] = 'field_date_measured';
  $handler->display->display_options['fields']['field_date_measured']['table'] = 'field_data_field_date_measured';
  $handler->display->display_options['fields']['field_date_measured']['field'] = 'field_date_measured';
  $handler->display->display_options['fields']['field_date_measured']['settings'] = array(
    'format_type' => 'custom',
    'custom_date_format' => 'd-m-Y',
    'fromto' => 'both',
    'multiple_number' => '',
    'multiple_from' => '',
    'multiple_to' => '',
    'show_remaining_days' => 0,
  );
  /* Field: Content: Type */
  $handler->display->display_options['fields']['type']['id'] = 'type';
  $handler->display->display_options['fields']['type']['table'] = 'node';
  $handler->display->display_options['fields']['type']['field'] = 'type';
  $handler->display->display_options['fields']['type']['label'] = 'Measurement type';
  /* Field: Content: Nurse */
  $handler->display->display_options['fields']['field_nurse']['id'] = 'field_nurse';
  $handler->display->display_options['fields']['field_nurse']['table'] = 'field_data_field_nurse';
  $handler->display->display_options['fields']['field_nurse']['field'] = 'field_nurse';
  $handler->display->display_options['fields']['field_nurse']['settings'] = array(
    'bypass_access' => 0,
    'link' => 0,
  );
  /* Field: Content: Family Planning */
  $handler->display->display_options['fields']['field_family_planning_signs']['id'] = 'field_family_planning_signs';
  $handler->display->display_options['fields']['field_family_planning_signs']['table'] = 'field_data_field_family_planning_signs';
  $handler->display->display_options['fields']['field_family_planning_signs']['field'] = 'field_family_planning_signs';
  $handler->display->display_options['fields']['field_family_planning_signs']['label'] = '';
  $handler->display->display_options['fields']['field_family_planning_signs']['exclude'] = TRUE;
  $handler->display->display_options['fields']['field_family_planning_signs']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_family_planning_signs']['delta_offset'] = '0';
  /* Field: Content: Height */
  $handler->display->display_options['fields']['field_height']['id'] = 'field_height';
  $handler->display->display_options['fields']['field_height']['table'] = 'field_data_field_height';
  $handler->display->display_options['fields']['field_height']['field'] = 'field_height';
  $handler->display->display_options['fields']['field_height']['label'] = '';
  $handler->display->display_options['fields']['field_height']['exclude'] = TRUE;
  $handler->display->display_options['fields']['field_height']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_height']['settings'] = array(
    'thousand_separator' => '',
    'decimal_separator' => '.',
    'scale' => '2',
    'prefix_suffix' => 1,
  );
  /* Field: Content: MUAC */
  $handler->display->display_options['fields']['field_muac']['id'] = 'field_muac';
  $handler->display->display_options['fields']['field_muac']['table'] = 'field_data_field_muac';
  $handler->display->display_options['fields']['field_muac']['field'] = 'field_muac';
  $handler->display->display_options['fields']['field_muac']['label'] = '';
  $handler->display->display_options['fields']['field_muac']['exclude'] = TRUE;
  $handler->display->display_options['fields']['field_muac']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_muac']['settings'] = array(
    'thousand_separator' => '',
    'decimal_separator' => '.',
    'scale' => '2',
    'prefix_suffix' => 1,
  );
  /* Field: Content: Nutrition Signs */
  $handler->display->display_options['fields']['field_nutrition_signs']['id'] = 'field_nutrition_signs';
  $handler->display->display_options['fields']['field_nutrition_signs']['table'] = 'field_data_field_nutrition_signs';
  $handler->display->display_options['fields']['field_nutrition_signs']['field'] = 'field_nutrition_signs';
  $handler->display->display_options['fields']['field_nutrition_signs']['label'] = '';
  $handler->display->display_options['fields']['field_nutrition_signs']['exclude'] = TRUE;
  $handler->display->display_options['fields']['field_nutrition_signs']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_nutrition_signs']['delta_offset'] = '0';
  /* Field: Content: Photo */
  $handler->display->display_options['fields']['field_photo']['id'] = 'field_photo';
  $handler->display->display_options['fields']['field_photo']['table'] = 'field_data_field_photo';
  $handler->display->display_options['fields']['field_photo']['field'] = 'field_photo';
  $handler->display->display_options['fields']['field_photo']['label'] = '';
  $handler->display->display_options['fields']['field_photo']['exclude'] = TRUE;
  $handler->display->display_options['fields']['field_photo']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_photo']['click_sort_column'] = 'fid';
  $handler->display->display_options['fields']['field_photo']['settings'] = array(
    'image_style' => '',
    'image_link' => '',
  );
  $handler->display->display_options['fields']['field_photo']['group_column'] = 'entity_id';
  /* Field: Content: Weight */
  $handler->display->display_options['fields']['field_weight']['id'] = 'field_weight';
  $handler->display->display_options['fields']['field_weight']['table'] = 'field_data_field_weight';
  $handler->display->display_options['fields']['field_weight']['field'] = 'field_weight';
  $handler->display->display_options['fields']['field_weight']['label'] = '';
  $handler->display->display_options['fields']['field_weight']['exclude'] = TRUE;
  $handler->display->display_options['fields']['field_weight']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_weight']['settings'] = array(
    'thousand_separator' => '',
    'decimal_separator' => '.',
    'scale' => '2',
    'prefix_suffix' => 1,
  );
  /* Field: Global: Custom text */
  $handler->display->display_options['fields']['nothing']['id'] = 'nothing';
  $handler->display->display_options['fields']['nothing']['table'] = 'views';
  $handler->display->display_options['fields']['nothing']['field'] = 'nothing';
  $handler->display->display_options['fields']['nothing']['label'] = 'Measurement value';
  $handler->display->display_options['fields']['nothing']['alter']['text'] = '[field_family_planning_signs] [field_height] [field_muac] [field_nutrition_signs] [field_photo] [field_weight]';
  $handler->display->display_options['defaults']['filter_groups'] = FALSE;
  $handler->display->display_options['defaults']['filters'] = FALSE;
  /* Filter criterion: Content: Published */
  $handler->display->display_options['filters']['status']['id'] = 'status';
  $handler->display->display_options['filters']['status']['table'] = 'node';
  $handler->display->display_options['filters']['status']['field'] = 'status';
  $handler->display->display_options['filters']['status']['value'] = 1;
  $handler->display->display_options['filters']['status']['group'] = 1;
  $handler->display->display_options['filters']['status']['expose']['operator'] = FALSE;
  /* Filter criterion: Content: Type */
  $handler->display->display_options['filters']['type']['id'] = 'type';
  $handler->display->display_options['filters']['type']['table'] = 'node';
  $handler->display->display_options['filters']['type']['field'] = 'type';
  $handler->display->display_options['filters']['type']['value'] = array(
    'family_planning' => 'family_planning',
    'height' => 'height',
    'muac' => 'muac',
    'nutrition' => 'nutrition',
    'photo' => 'photo',
    'weight' => 'weight',
  );
  $handler->display->display_options['path'] = 'admin/content/measurements/csv';
  $handler->display->display_options['displays'] = array(
    'page_4' => 'page_4',
    'default' => 0,
    'page' => 0,
    'page_1' => 0,
    'page_2' => 0,
    'page_3' => 0,
  );
  $handler->display->display_options['sitename_title'] = 0;

  /* Display: Historical Child */
  $handler = $view->new_display('page', 'Historical Child', 'historical_child');
  $handler->display->display_options['defaults']['title'] = FALSE;
  $handler->display->display_options['title'] = 'Historical Child';
  $handler->display->display_options['defaults']['group_by'] = FALSE;
  $handler->display->display_options['group_by'] = TRUE;
  $handler->display->display_options['defaults']['empty'] = FALSE;
  /* No results behavior: Global: Text area */
  $handler->display->display_options['empty']['area']['id'] = 'area';
  $handler->display->display_options['empty']['area']['table'] = 'views';
  $handler->display->display_options['empty']['area']['field'] = 'area';
  $handler->display->display_options['empty']['area']['empty'] = TRUE;
  $handler->display->display_options['empty']['area']['content'] = 'No records exist for these criteria';
  $handler->display->display_options['empty']['area']['format'] = 'filtered_html';
  $handler->display->display_options['defaults']['relationships'] = FALSE;
  /* Relationship: Entity Reference: Referenced Entity */
  $handler->display->display_options['relationships']['field_session_target_id']['id'] = 'field_session_target_id';
  $handler->display->display_options['relationships']['field_session_target_id']['table'] = 'field_data_field_session';
  $handler->display->display_options['relationships']['field_session_target_id']['field'] = 'field_session_target_id';
  $handler->display->display_options['relationships']['field_session_target_id']['label'] = 'Session';
  /* Relationship: Child */
  $handler->display->display_options['relationships']['field_person_target_id']['id'] = 'field_person_target_id';
  $handler->display->display_options['relationships']['field_person_target_id']['table'] = 'field_data_field_person';
  $handler->display->display_options['relationships']['field_person_target_id']['field'] = 'field_person_target_id';
  $handler->display->display_options['relationships']['field_person_target_id']['ui_name'] = 'Child';
  $handler->display->display_options['relationships']['field_person_target_id']['label'] = 'Person';
  /* Relationship: Entity Reference: Referenced Entity */
  $handler->display->display_options['relationships']['field_nurse_target_id']['id'] = 'field_nurse_target_id';
  $handler->display->display_options['relationships']['field_nurse_target_id']['table'] = 'field_data_field_nurse';
  $handler->display->display_options['relationships']['field_nurse_target_id']['field'] = 'field_nurse_target_id';
  $handler->display->display_options['relationships']['field_nurse_target_id']['label'] = 'Nurse';
  /* Relationship: Relationship */
  $handler->display->display_options['relationships']['reverse_field_related_to_node']['id'] = 'reverse_field_related_to_node';
  $handler->display->display_options['relationships']['reverse_field_related_to_node']['table'] = 'node';
  $handler->display->display_options['relationships']['reverse_field_related_to_node']['field'] = 'reverse_field_related_to_node';
  $handler->display->display_options['relationships']['reverse_field_related_to_node']['relationship'] = 'field_person_target_id';
  $handler->display->display_options['relationships']['reverse_field_related_to_node']['ui_name'] = 'Relationship';
  $handler->display->display_options['relationships']['reverse_field_related_to_node']['label'] = 'Relationship';
  /* Relationship: Parent/Caregiver */
  $handler->display->display_options['relationships']['field_person_target_id_1']['id'] = 'field_person_target_id_1';
  $handler->display->display_options['relationships']['field_person_target_id_1']['table'] = 'field_data_field_person';
  $handler->display->display_options['relationships']['field_person_target_id_1']['field'] = 'field_person_target_id';
  $handler->display->display_options['relationships']['field_person_target_id_1']['relationship'] = 'reverse_field_related_to_node';
  $handler->display->display_options['relationships']['field_person_target_id_1']['ui_name'] = 'Parent/Caregiver';
  $handler->display->display_options['relationships']['field_person_target_id_1']['label'] = 'Parent/Caregiver';
  $handler->display->display_options['defaults']['fields'] = FALSE;
  /* Field: Bulk operations: Content */
  $handler->display->display_options['fields']['views_bulk_operations']['id'] = 'views_bulk_operations';
  $handler->display->display_options['fields']['views_bulk_operations']['table'] = 'views_entity_node';
  $handler->display->display_options['fields']['views_bulk_operations']['field'] = 'views_bulk_operations';
  $handler->display->display_options['fields']['views_bulk_operations']['vbo_settings']['display_type'] = '0';
  $handler->display->display_options['fields']['views_bulk_operations']['vbo_settings']['enable_select_all_pages'] = 1;
  $handler->display->display_options['fields']['views_bulk_operations']['vbo_settings']['row_clickable'] = 1;
  $handler->display->display_options['fields']['views_bulk_operations']['vbo_settings']['force_single'] = 0;
  $handler->display->display_options['fields']['views_bulk_operations']['vbo_settings']['entity_load_capacity'] = '10';
  $handler->display->display_options['fields']['views_bulk_operations']['vbo_operations'] = array(
    'action::vbo_export_csv_action' => array(
      'selected' => 1,
      'postpone_processing' => 0,
      'skip_confirmation' => 0,
      'skip_permission_check' => 0,
      'override_label' => 0,
      'label' => '',
      'settings' => array(
        'strip_tags' => 1,
      ),
    ),
  );
  /* Field: Content: Nid */
  $handler->display->display_options['fields']['nid']['id'] = 'nid';
  $handler->display->display_options['fields']['nid']['table'] = 'node';
  $handler->display->display_options['fields']['nid']['field'] = 'nid';
  $handler->display->display_options['fields']['nid']['relationship'] = 'field_session_target_id';
  $handler->display->display_options['fields']['nid']['label'] = 'Session ID';
  /* Field: Content: Date Measured */
  $handler->display->display_options['fields']['field_date_measured']['id'] = 'field_date_measured';
  $handler->display->display_options['fields']['field_date_measured']['table'] = 'field_data_field_date_measured';
  $handler->display->display_options['fields']['field_date_measured']['field'] = 'field_date_measured';
  $handler->display->display_options['fields']['field_date_measured']['label'] = 'Date';
  $handler->display->display_options['fields']['field_date_measured']['element_class'] = 'nowrap';
  $handler->display->display_options['fields']['field_date_measured']['settings'] = array(
    'format_type' => 'custom',
    'custom_date_format' => 'd-m-Y',
    'fromto' => 'both',
    'multiple_number' => '',
    'multiple_from' => '',
    'multiple_to' => '',
    'show_remaining_days' => 0,
  );
  /* Field: Content: Clinic */
  $handler->display->display_options['fields']['field_clinic']['id'] = 'field_clinic';
  $handler->display->display_options['fields']['field_clinic']['table'] = 'field_data_field_clinic';
  $handler->display->display_options['fields']['field_clinic']['field'] = 'field_clinic';
  $handler->display->display_options['fields']['field_clinic']['relationship'] = 'field_session_target_id';
  $handler->display->display_options['fields']['field_clinic']['settings'] = array(
    'bypass_access' => 0,
    'link' => 0,
  );
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title_2']['id'] = 'title_2';
  $handler->display->display_options['fields']['title_2']['table'] = 'node';
  $handler->display->display_options['fields']['title_2']['field'] = 'title';
  $handler->display->display_options['fields']['title_2']['relationship'] = 'field_nurse_target_id';
  $handler->display->display_options['fields']['title_2']['label'] = 'Nurse';
  /* Field: MIN(Content: Title) */
  $handler->display->display_options['fields']['title_1']['id'] = 'title_1';
  $handler->display->display_options['fields']['title_1']['table'] = 'node';
  $handler->display->display_options['fields']['title_1']['field'] = 'title';
  $handler->display->display_options['fields']['title_1']['relationship'] = 'field_person_target_id_1';
  $handler->display->display_options['fields']['title_1']['group_type'] = 'min';
  $handler->display->display_options['fields']['title_1']['label'] = 'Parent/Caregiver Name';
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title']['id'] = 'title';
  $handler->display->display_options['fields']['title']['table'] = 'node';
  $handler->display->display_options['fields']['title']['field'] = 'title';
  $handler->display->display_options['fields']['title']['relationship'] = 'field_person_target_id';
  $handler->display->display_options['fields']['title']['label'] = 'Child Name';
  /* Field: Content: Gender */
  $handler->display->display_options['fields']['field_gender']['id'] = 'field_gender';
  $handler->display->display_options['fields']['field_gender']['table'] = 'field_data_field_gender';
  $handler->display->display_options['fields']['field_gender']['field'] = 'field_gender';
  $handler->display->display_options['fields']['field_gender']['relationship'] = 'field_person_target_id';
  /* Field: Content: Birth date */
  $handler->display->display_options['fields']['field_birth_date']['id'] = 'field_birth_date';
  $handler->display->display_options['fields']['field_birth_date']['table'] = 'field_data_field_birth_date';
  $handler->display->display_options['fields']['field_birth_date']['field'] = 'field_birth_date';
  $handler->display->display_options['fields']['field_birth_date']['relationship'] = 'field_person_target_id';
  $handler->display->display_options['fields']['field_birth_date']['settings'] = array(
    'format_type' => 'custom',
    'custom_date_format' => 'd-m-Y',
    'fromto' => 'both',
    'multiple_number' => '',
    'multiple_from' => '',
    'multiple_to' => '',
    'show_remaining_days' => 0,
  );
  /* Field: Content: Type */
  $handler->display->display_options['fields']['type']['id'] = 'type';
  $handler->display->display_options['fields']['type']['table'] = 'node';
  $handler->display->display_options['fields']['type']['field'] = 'type';
  $handler->display->display_options['fields']['type']['label'] = 'Measurement Type';
  /* Field: Content: Height */
  $handler->display->display_options['fields']['field_height']['id'] = 'field_height';
  $handler->display->display_options['fields']['field_height']['table'] = 'field_data_field_height';
  $handler->display->display_options['fields']['field_height']['field'] = 'field_height';
  $handler->display->display_options['fields']['field_height']['exclude'] = TRUE;
  $handler->display->display_options['fields']['field_height']['settings'] = array(
    'thousand_separator' => '',
    'decimal_separator' => '.',
    'scale' => '2',
    'prefix_suffix' => 0,
  );
  /* Field: Content: MUAC */
  $handler->display->display_options['fields']['field_muac']['id'] = 'field_muac';
  $handler->display->display_options['fields']['field_muac']['table'] = 'field_data_field_muac';
  $handler->display->display_options['fields']['field_muac']['field'] = 'field_muac';
  $handler->display->display_options['fields']['field_muac']['exclude'] = TRUE;
  $handler->display->display_options['fields']['field_muac']['settings'] = array(
    'thousand_separator' => '',
    'decimal_separator' => '.',
    'scale' => '2',
    'prefix_suffix' => 1,
  );
  /* Field: Content: Weight */
  $handler->display->display_options['fields']['field_weight']['id'] = 'field_weight';
  $handler->display->display_options['fields']['field_weight']['table'] = 'field_data_field_weight';
  $handler->display->display_options['fields']['field_weight']['field'] = 'field_weight';
  $handler->display->display_options['fields']['field_weight']['exclude'] = TRUE;
  $handler->display->display_options['fields']['field_weight']['settings'] = array(
    'thousand_separator' => '',
    'decimal_separator' => '.',
    'scale' => '2',
    'prefix_suffix' => 0,
  );
  /* Field: Content: Nutrition Signs */
  $handler->display->display_options['fields']['field_nutrition_signs']['id'] = 'field_nutrition_signs';
  $handler->display->display_options['fields']['field_nutrition_signs']['table'] = 'field_data_field_nutrition_signs';
  $handler->display->display_options['fields']['field_nutrition_signs']['field'] = 'field_nutrition_signs';
  $handler->display->display_options['fields']['field_nutrition_signs']['exclude'] = TRUE;
  $handler->display->display_options['fields']['field_nutrition_signs']['delta_offset'] = '0';
  /* Field: Global: Custom text */
  $handler->display->display_options['fields']['nothing']['id'] = 'nothing';
  $handler->display->display_options['fields']['nothing']['table'] = 'views';
  $handler->display->display_options['fields']['nothing']['field'] = 'nothing';
  $handler->display->display_options['fields']['nothing']['label'] = 'Measurement Value';
  $handler->display->display_options['fields']['nothing']['alter']['text'] = '[field_height][field_muac][field_weight][field_nutrition_signs] ';
  /* Field: Content: ZScore Age */
  $handler->display->display_options['fields']['field_zscore_age']['id'] = 'field_zscore_age';
  $handler->display->display_options['fields']['field_zscore_age']['table'] = 'field_data_field_zscore_age';
  $handler->display->display_options['fields']['field_zscore_age']['field'] = 'field_zscore_age';
  $handler->display->display_options['fields']['field_zscore_age']['exclude'] = TRUE;
  $handler->display->display_options['fields']['field_zscore_age']['settings'] = array(
    'thousand_separator' => '',
    'decimal_separator' => '.',
    'scale' => '2',
    'prefix_suffix' => 0,
  );
  /* Field: Content: ZScore LHFA */
  $handler->display->display_options['fields']['zscore_lhfa']['id'] = 'zscore_lhfa';
  $handler->display->display_options['fields']['zscore_lhfa']['table'] = 'node';
  $handler->display->display_options['fields']['zscore_lhfa']['field'] = 'zscore_lhfa';
  /* Field: Content: ZScore WFA */
  $handler->display->display_options['fields']['zscore_wfa']['id'] = 'zscore_wfa';
  $handler->display->display_options['fields']['zscore_wfa']['table'] = 'node';
  $handler->display->display_options['fields']['zscore_wfa']['field'] = 'zscore_wfa';
  /* Field: Content: ZScore Length */
  $handler->display->display_options['fields']['field_zscore_length']['id'] = 'field_zscore_length';
  $handler->display->display_options['fields']['field_zscore_length']['table'] = 'field_data_field_zscore_length';
  $handler->display->display_options['fields']['field_zscore_length']['field'] = 'field_zscore_length';
  $handler->display->display_options['fields']['field_zscore_length']['label'] = 'ZScore WFL';
  $handler->display->display_options['fields']['field_zscore_length']['settings'] = array(
    'thousand_separator' => '',
    'decimal_separator' => '.',
    'scale' => '2',
    'prefix_suffix' => 0,
  );
  /* Field: Content: BMI */
  $handler->display->display_options['fields']['field_bmi']['id'] = 'field_bmi';
  $handler->display->display_options['fields']['field_bmi']['table'] = 'field_data_field_bmi';
  $handler->display->display_options['fields']['field_bmi']['field'] = 'field_bmi';
  $handler->display->display_options['fields']['field_bmi']['settings'] = array(
    'thousand_separator' => '',
    'decimal_separator' => '.',
    'scale' => '2',
    'prefix_suffix' => 1,
  );
  /* Field: Content: ZScore BMI */
  $handler->display->display_options['fields']['field_zscore_bmi']['id'] = 'field_zscore_bmi';
  $handler->display->display_options['fields']['field_zscore_bmi']['table'] = 'field_data_field_zscore_bmi';
  $handler->display->display_options['fields']['field_zscore_bmi']['field'] = 'field_zscore_bmi';
  $handler->display->display_options['fields']['field_zscore_bmi']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_zscore_bmi']['settings'] = array(
    'thousand_separator' => '',
    'decimal_separator' => '.',
    'scale' => '2',
    'prefix_suffix' => 1,
  );
  /* Field: Content: Edit link */
  $handler->display->display_options['fields']['edit_node']['id'] = 'edit_node';
  $handler->display->display_options['fields']['edit_node']['table'] = 'views_entity_node';
  $handler->display->display_options['fields']['edit_node']['field'] = 'edit_node';
  $handler->display->display_options['fields']['edit_node']['label'] = 'Edit';
  $handler->display->display_options['defaults']['sorts'] = FALSE;
  /* Sort criterion: Content: Date Measured (field_date_measured) */
  $handler->display->display_options['sorts']['field_date_measured_value']['id'] = 'field_date_measured_value';
  $handler->display->display_options['sorts']['field_date_measured_value']['table'] = 'field_data_field_date_measured';
  $handler->display->display_options['sorts']['field_date_measured_value']['field'] = 'field_date_measured_value';
  /* Sort criterion: Content: Post date */
  $handler->display->display_options['sorts']['created']['id'] = 'created';
  $handler->display->display_options['sorts']['created']['table'] = 'node';
  $handler->display->display_options['sorts']['created']['field'] = 'created';
  $handler->display->display_options['sorts']['created']['order'] = 'DESC';
  /* Sort criterion: Content: Nid */
  $handler->display->display_options['sorts']['nid']['id'] = 'nid';
  $handler->display->display_options['sorts']['nid']['table'] = 'node';
  $handler->display->display_options['sorts']['nid']['field'] = 'nid';
  $handler->display->display_options['defaults']['filter_groups'] = FALSE;
  $handler->display->display_options['defaults']['filters'] = FALSE;
  /* Filter criterion: Content: Published */
  $handler->display->display_options['filters']['status']['id'] = 'status';
  $handler->display->display_options['filters']['status']['table'] = 'node';
  $handler->display->display_options['filters']['status']['field'] = 'status';
  $handler->display->display_options['filters']['status']['value'] = 1;
  $handler->display->display_options['filters']['status']['group'] = 1;
  $handler->display->display_options['filters']['status']['expose']['operator'] = FALSE;
  /* Filter criterion: Content: Type */
  $handler->display->display_options['filters']['type']['id'] = 'type';
  $handler->display->display_options['filters']['type']['table'] = 'node';
  $handler->display->display_options['filters']['type']['field'] = 'type';
  $handler->display->display_options['filters']['type']['value'] = array(
    'counseling_session' => 'counseling_session',
    'height' => 'height',
    'muac' => 'muac',
    'nutrition' => 'nutrition',
    'weight' => 'weight',
  );
  $handler->display->display_options['filters']['type']['group'] = 1;
  /* Filter criterion: Content: Nid */
  $handler->display->display_options['filters']['nid']['id'] = 'nid';
  $handler->display->display_options['filters']['nid']['table'] = 'node';
  $handler->display->display_options['filters']['nid']['field'] = 'nid';
  $handler->display->display_options['filters']['nid']['relationship'] = 'field_session_target_id';
  $handler->display->display_options['filters']['nid']['group'] = 1;
  $handler->display->display_options['filters']['nid']['exposed'] = TRUE;
  $handler->display->display_options['filters']['nid']['expose']['operator_id'] = 'nid_op';
  $handler->display->display_options['filters']['nid']['expose']['label'] = 'Session ID';
  $handler->display->display_options['filters']['nid']['expose']['operator'] = 'nid_op';
  $handler->display->display_options['filters']['nid']['expose']['identifier'] = 'nid';
  $handler->display->display_options['filters']['nid']['expose']['remember_roles'] = array(
    2 => '2',
    1 => 0,
    3 => 0,
    4 => 0,
  );
  /* Filter criterion: Content: Title */
  $handler->display->display_options['filters']['title']['id'] = 'title';
  $handler->display->display_options['filters']['title']['table'] = 'node';
  $handler->display->display_options['filters']['title']['field'] = 'title';
  $handler->display->display_options['filters']['title']['relationship'] = 'field_person_target_id';
  $handler->display->display_options['filters']['title']['operator'] = 'word';
  $handler->display->display_options['filters']['title']['group'] = 1;
  $handler->display->display_options['filters']['title']['exposed'] = TRUE;
  $handler->display->display_options['filters']['title']['expose']['operator_id'] = 'title_op';
  $handler->display->display_options['filters']['title']['expose']['label'] = 'Child';
  $handler->display->display_options['filters']['title']['expose']['operator'] = 'title_op';
  $handler->display->display_options['filters']['title']['expose']['identifier'] = 'title';
  $handler->display->display_options['filters']['title']['expose']['remember_roles'] = array(
    2 => '2',
    1 => 0,
    4 => 0,
    3 => 0,
    5 => 0,
  );
  /* Filter criterion: Content: Title */
  $handler->display->display_options['filters']['title_1']['id'] = 'title_1';
  $handler->display->display_options['filters']['title_1']['table'] = 'node';
  $handler->display->display_options['filters']['title_1']['field'] = 'title';
  $handler->display->display_options['filters']['title_1']['relationship'] = 'field_person_target_id_1';
  $handler->display->display_options['filters']['title_1']['operator'] = 'word';
  $handler->display->display_options['filters']['title_1']['group'] = 1;
  $handler->display->display_options['filters']['title_1']['exposed'] = TRUE;
  $handler->display->display_options['filters']['title_1']['expose']['operator_id'] = 'title_1_op';
  $handler->display->display_options['filters']['title_1']['expose']['label'] = 'Mother/Caregiver';
  $handler->display->display_options['filters']['title_1']['expose']['operator'] = 'title_1_op';
  $handler->display->display_options['filters']['title_1']['expose']['identifier'] = 'title_1';
  $handler->display->display_options['filters']['title_1']['expose']['remember_roles'] = array(
    2 => '2',
    1 => 0,
    4 => 0,
    3 => 0,
    5 => 0,
  );
  /* Filter criterion: Content: Clinic (field_clinic) */
  $handler->display->display_options['filters']['field_clinic_target_id']['id'] = 'field_clinic_target_id';
  $handler->display->display_options['filters']['field_clinic_target_id']['table'] = 'field_data_field_clinic';
  $handler->display->display_options['filters']['field_clinic_target_id']['field'] = 'field_clinic_target_id';
  $handler->display->display_options['filters']['field_clinic_target_id']['relationship'] = 'field_session_target_id';
  $handler->display->display_options['filters']['field_clinic_target_id']['group'] = 1;
  $handler->display->display_options['filters']['field_clinic_target_id']['exposed'] = TRUE;
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['operator_id'] = 'field_clinic_target_id_op';
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['label'] = 'Clinic';
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['operator'] = 'field_clinic_target_id_op';
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['identifier'] = 'field_clinic_target_id';
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['remember_roles'] = array(
    2 => '2',
    1 => 0,
    3 => 0,
    4 => 0,
  );
  /* Filter criterion: Content: Date Measured (field_date_measured) */
  $handler->display->display_options['filters']['field_date_measured_value']['id'] = 'field_date_measured_value';
  $handler->display->display_options['filters']['field_date_measured_value']['table'] = 'field_data_field_date_measured';
  $handler->display->display_options['filters']['field_date_measured_value']['field'] = 'field_date_measured_value';
  $handler->display->display_options['filters']['field_date_measured_value']['operator'] = 'between';
  $handler->display->display_options['filters']['field_date_measured_value']['exposed'] = TRUE;
  $handler->display->display_options['filters']['field_date_measured_value']['expose']['operator_id'] = 'field_date_measured_value_op';
  $handler->display->display_options['filters']['field_date_measured_value']['expose']['label'] = 'Date Measured';
  $handler->display->display_options['filters']['field_date_measured_value']['expose']['operator'] = 'field_date_measured_value_op';
  $handler->display->display_options['filters']['field_date_measured_value']['expose']['identifier'] = 'field_date_measured_value';
  $handler->display->display_options['filters']['field_date_measured_value']['expose']['remember_roles'] = array(
    2 => '2',
    1 => 0,
    3 => 0,
    4 => 0,
  );
  $handler->display->display_options['path'] = 'admin/measurement-historical-child';
  $handler->display->display_options['menu']['type'] = 'normal';
  $handler->display->display_options['menu']['title'] = 'Historical Child';
  $handler->display->display_options['menu']['weight'] = '0';
  $handler->display->display_options['menu']['name'] = 'main-menu';
  $handler->display->display_options['menu']['context'] = 0;
  $handler->display->display_options['menu']['context_only_inline'] = 0;

  /* Display: Historical Mother */
  $handler = $view->new_display('page', 'Historical Mother', 'historical_mother');
  $handler->display->display_options['defaults']['title'] = FALSE;
  $handler->display->display_options['title'] = 'Historical Mother';
  $handler->display->display_options['defaults']['group_by'] = FALSE;
  $handler->display->display_options['defaults']['empty'] = FALSE;
  /* No results behavior: Global: Text area */
  $handler->display->display_options['empty']['area']['id'] = 'area';
  $handler->display->display_options['empty']['area']['table'] = 'views';
  $handler->display->display_options['empty']['area']['field'] = 'area';
  $handler->display->display_options['empty']['area']['empty'] = TRUE;
  $handler->display->display_options['empty']['area']['content'] = 'No records exist for these criteria';
  $handler->display->display_options['empty']['area']['format'] = 'filtered_html';
  $handler->display->display_options['defaults']['relationships'] = FALSE;
  /* Relationship: Entity Reference: Referenced Entity */
  $handler->display->display_options['relationships']['field_session_target_id']['id'] = 'field_session_target_id';
  $handler->display->display_options['relationships']['field_session_target_id']['table'] = 'field_data_field_session';
  $handler->display->display_options['relationships']['field_session_target_id']['field'] = 'field_session_target_id';
  $handler->display->display_options['relationships']['field_session_target_id']['label'] = 'Session';
  /* Relationship: Entity Reference: Referenced Entity */
  $handler->display->display_options['relationships']['field_person_target_id']['id'] = 'field_person_target_id';
  $handler->display->display_options['relationships']['field_person_target_id']['table'] = 'field_data_field_person';
  $handler->display->display_options['relationships']['field_person_target_id']['field'] = 'field_person_target_id';
  $handler->display->display_options['relationships']['field_person_target_id']['label'] = 'Person';
  /* Relationship: Entity Reference: Referenced Entity */
  $handler->display->display_options['relationships']['field_nurse_target_id']['id'] = 'field_nurse_target_id';
  $handler->display->display_options['relationships']['field_nurse_target_id']['table'] = 'field_data_field_nurse';
  $handler->display->display_options['relationships']['field_nurse_target_id']['field'] = 'field_nurse_target_id';
  $handler->display->display_options['relationships']['field_nurse_target_id']['label'] = 'Nurse';
  $handler->display->display_options['defaults']['fields'] = FALSE;
  /* Field: Bulk operations: Content */
  $handler->display->display_options['fields']['views_bulk_operations']['id'] = 'views_bulk_operations';
  $handler->display->display_options['fields']['views_bulk_operations']['table'] = 'views_entity_node';
  $handler->display->display_options['fields']['views_bulk_operations']['field'] = 'views_bulk_operations';
  $handler->display->display_options['fields']['views_bulk_operations']['vbo_settings']['display_type'] = '0';
  $handler->display->display_options['fields']['views_bulk_operations']['vbo_settings']['enable_select_all_pages'] = 1;
  $handler->display->display_options['fields']['views_bulk_operations']['vbo_settings']['row_clickable'] = 1;
  $handler->display->display_options['fields']['views_bulk_operations']['vbo_settings']['force_single'] = 0;
  $handler->display->display_options['fields']['views_bulk_operations']['vbo_settings']['entity_load_capacity'] = '10';
  $handler->display->display_options['fields']['views_bulk_operations']['vbo_operations'] = array(
    'action::vbo_export_csv_action' => array(
      'selected' => 1,
      'postpone_processing' => 0,
      'skip_confirmation' => 0,
      'skip_permission_check' => 0,
      'override_label' => 0,
      'label' => '',
      'settings' => array(
        'strip_tags' => 1,
      ),
    ),
  );
  /* Field: Content: Nid */
  $handler->display->display_options['fields']['nid']['id'] = 'nid';
  $handler->display->display_options['fields']['nid']['table'] = 'node';
  $handler->display->display_options['fields']['nid']['field'] = 'nid';
  $handler->display->display_options['fields']['nid']['relationship'] = 'field_session_target_id';
  $handler->display->display_options['fields']['nid']['label'] = 'Session ID';
  /* Field: Content: Date Measured */
  $handler->display->display_options['fields']['field_date_measured']['id'] = 'field_date_measured';
  $handler->display->display_options['fields']['field_date_measured']['table'] = 'field_data_field_date_measured';
  $handler->display->display_options['fields']['field_date_measured']['field'] = 'field_date_measured';
  $handler->display->display_options['fields']['field_date_measured']['label'] = 'Date';
  $handler->display->display_options['fields']['field_date_measured']['element_class'] = 'nowrap';
  $handler->display->display_options['fields']['field_date_measured']['settings'] = array(
    'format_type' => 'custom',
    'custom_date_format' => 'd-m-Y',
    'fromto' => 'both',
    'multiple_number' => '',
    'multiple_from' => '',
    'multiple_to' => '',
    'show_remaining_days' => 0,
  );
  /* Field: Content: Clinic */
  $handler->display->display_options['fields']['field_clinic']['id'] = 'field_clinic';
  $handler->display->display_options['fields']['field_clinic']['table'] = 'field_data_field_clinic';
  $handler->display->display_options['fields']['field_clinic']['field'] = 'field_clinic';
  $handler->display->display_options['fields']['field_clinic']['relationship'] = 'field_session_target_id';
  $handler->display->display_options['fields']['field_clinic']['settings'] = array(
    'bypass_access' => 0,
    'link' => 0,
  );
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title']['id'] = 'title';
  $handler->display->display_options['fields']['title']['table'] = 'node';
  $handler->display->display_options['fields']['title']['field'] = 'title';
  $handler->display->display_options['fields']['title']['relationship'] = 'field_nurse_target_id';
  $handler->display->display_options['fields']['title']['label'] = 'Nurse';
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title_1']['id'] = 'title_1';
  $handler->display->display_options['fields']['title_1']['table'] = 'node';
  $handler->display->display_options['fields']['title_1']['field'] = 'title';
  $handler->display->display_options['fields']['title_1']['relationship'] = 'field_person_target_id';
  $handler->display->display_options['fields']['title_1']['label'] = 'Mother Name';
  /* Field: Content: Family Planning */
  $handler->display->display_options['fields']['field_family_planning_signs']['id'] = 'field_family_planning_signs';
  $handler->display->display_options['fields']['field_family_planning_signs']['table'] = 'field_data_field_family_planning_signs';
  $handler->display->display_options['fields']['field_family_planning_signs']['field'] = 'field_family_planning_signs';
  $handler->display->display_options['fields']['field_family_planning_signs']['exclude'] = TRUE;
  $handler->display->display_options['fields']['field_family_planning_signs']['delta_offset'] = '0';
  /* Field: Content: Type */
  $handler->display->display_options['fields']['type']['id'] = 'type';
  $handler->display->display_options['fields']['type']['table'] = 'node';
  $handler->display->display_options['fields']['type']['field'] = 'type';
  $handler->display->display_options['fields']['type']['label'] = 'Measurement Type';
  /* Field: Global: Custom text */
  $handler->display->display_options['fields']['nothing']['id'] = 'nothing';
  $handler->display->display_options['fields']['nothing']['table'] = 'views';
  $handler->display->display_options['fields']['nothing']['field'] = 'nothing';
  $handler->display->display_options['fields']['nothing']['label'] = 'Measurement Value';
  $handler->display->display_options['fields']['nothing']['alter']['text'] = '[field_family_planning_signs]';
  /* Field: Content: Edit link */
  $handler->display->display_options['fields']['edit_node']['id'] = 'edit_node';
  $handler->display->display_options['fields']['edit_node']['table'] = 'views_entity_node';
  $handler->display->display_options['fields']['edit_node']['field'] = 'edit_node';
  $handler->display->display_options['fields']['edit_node']['label'] = 'Edit';
  $handler->display->display_options['defaults']['sorts'] = FALSE;
  /* Sort criterion: Content: Date Measured (field_date_measured) */
  $handler->display->display_options['sorts']['field_date_measured_value']['id'] = 'field_date_measured_value';
  $handler->display->display_options['sorts']['field_date_measured_value']['table'] = 'field_data_field_date_measured';
  $handler->display->display_options['sorts']['field_date_measured_value']['field'] = 'field_date_measured_value';
  $handler->display->display_options['sorts']['field_date_measured_value']['order'] = 'DESC';
  /* Sort criterion: Content: Post date */
  $handler->display->display_options['sorts']['created']['id'] = 'created';
  $handler->display->display_options['sorts']['created']['table'] = 'node';
  $handler->display->display_options['sorts']['created']['field'] = 'created';
  $handler->display->display_options['sorts']['created']['order'] = 'DESC';
  /* Sort criterion: Content: Nid */
  $handler->display->display_options['sorts']['nid']['id'] = 'nid';
  $handler->display->display_options['sorts']['nid']['table'] = 'node';
  $handler->display->display_options['sorts']['nid']['field'] = 'nid';
  $handler->display->display_options['defaults']['filter_groups'] = FALSE;
  $handler->display->display_options['defaults']['filters'] = FALSE;
  /* Filter criterion: Content: Published */
  $handler->display->display_options['filters']['status']['id'] = 'status';
  $handler->display->display_options['filters']['status']['table'] = 'node';
  $handler->display->display_options['filters']['status']['field'] = 'status';
  $handler->display->display_options['filters']['status']['value'] = 1;
  $handler->display->display_options['filters']['status']['group'] = 1;
  $handler->display->display_options['filters']['status']['expose']['operator'] = FALSE;
  /* Filter criterion: Content: Type */
  $handler->display->display_options['filters']['type']['id'] = 'type';
  $handler->display->display_options['filters']['type']['table'] = 'node';
  $handler->display->display_options['filters']['type']['field'] = 'type';
  $handler->display->display_options['filters']['type']['value'] = array(
    'family_planning' => 'family_planning',
    'participant_consent' => 'participant_consent',
  );
  $handler->display->display_options['filters']['type']['group'] = 1;
  /* Filter criterion: Content: Nid */
  $handler->display->display_options['filters']['nid']['id'] = 'nid';
  $handler->display->display_options['filters']['nid']['table'] = 'node';
  $handler->display->display_options['filters']['nid']['field'] = 'nid';
  $handler->display->display_options['filters']['nid']['relationship'] = 'field_session_target_id';
  $handler->display->display_options['filters']['nid']['group'] = 1;
  $handler->display->display_options['filters']['nid']['exposed'] = TRUE;
  $handler->display->display_options['filters']['nid']['expose']['operator_id'] = 'nid_op';
  $handler->display->display_options['filters']['nid']['expose']['label'] = 'Session ID';
  $handler->display->display_options['filters']['nid']['expose']['operator'] = 'nid_op';
  $handler->display->display_options['filters']['nid']['expose']['identifier'] = 'nid';
  $handler->display->display_options['filters']['nid']['expose']['remember_roles'] = array(
    2 => '2',
    1 => 0,
    3 => 0,
    4 => 0,
  );
  /* Filter criterion: Content: Title */
  $handler->display->display_options['filters']['title_1']['id'] = 'title_1';
  $handler->display->display_options['filters']['title_1']['table'] = 'node';
  $handler->display->display_options['filters']['title_1']['field'] = 'title';
  $handler->display->display_options['filters']['title_1']['operator'] = 'word';
  $handler->display->display_options['filters']['title_1']['group'] = 1;
  $handler->display->display_options['filters']['title_1']['exposed'] = TRUE;
  $handler->display->display_options['filters']['title_1']['expose']['operator_id'] = 'title_1_op';
  $handler->display->display_options['filters']['title_1']['expose']['label'] = 'Mother/Caregiver';
  $handler->display->display_options['filters']['title_1']['expose']['operator'] = 'title_1_op';
  $handler->display->display_options['filters']['title_1']['expose']['identifier'] = 'title_1';
  $handler->display->display_options['filters']['title_1']['expose']['remember_roles'] = array(
    2 => '2',
    1 => 0,
    3 => 0,
    4 => 0,
  );
  /* Filter criterion: Content: Clinic (field_clinic) */
  $handler->display->display_options['filters']['field_clinic_target_id']['id'] = 'field_clinic_target_id';
  $handler->display->display_options['filters']['field_clinic_target_id']['table'] = 'field_data_field_clinic';
  $handler->display->display_options['filters']['field_clinic_target_id']['field'] = 'field_clinic_target_id';
  $handler->display->display_options['filters']['field_clinic_target_id']['relationship'] = 'field_session_target_id';
  $handler->display->display_options['filters']['field_clinic_target_id']['group'] = 1;
  $handler->display->display_options['filters']['field_clinic_target_id']['exposed'] = TRUE;
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['operator_id'] = 'field_clinic_target_id_op';
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['label'] = 'Clinic';
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['operator'] = 'field_clinic_target_id_op';
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['identifier'] = 'field_clinic_target_id';
  $handler->display->display_options['filters']['field_clinic_target_id']['expose']['remember_roles'] = array(
    2 => '2',
    1 => 0,
    3 => 0,
    4 => 0,
  );
  /* Filter criterion: Content: Date Measured (field_date_measured) */
  $handler->display->display_options['filters']['field_date_measured_value']['id'] = 'field_date_measured_value';
  $handler->display->display_options['filters']['field_date_measured_value']['table'] = 'field_data_field_date_measured';
  $handler->display->display_options['filters']['field_date_measured_value']['field'] = 'field_date_measured_value';
  $handler->display->display_options['filters']['field_date_measured_value']['operator'] = 'between';
  $handler->display->display_options['filters']['field_date_measured_value']['exposed'] = TRUE;
  $handler->display->display_options['filters']['field_date_measured_value']['expose']['operator_id'] = 'field_date_measured_value_op';
  $handler->display->display_options['filters']['field_date_measured_value']['expose']['label'] = 'Date Measured';
  $handler->display->display_options['filters']['field_date_measured_value']['expose']['operator'] = 'field_date_measured_value_op';
  $handler->display->display_options['filters']['field_date_measured_value']['expose']['identifier'] = 'field_date_measured_value';
  $handler->display->display_options['filters']['field_date_measured_value']['expose']['remember_roles'] = array(
    2 => '2',
    1 => 0,
    3 => 0,
    4 => 0,
  );
  $handler->display->display_options['path'] = 'admin/measurement-historical-mother';
  $handler->display->display_options['menu']['type'] = 'normal';
  $handler->display->display_options['menu']['title'] = 'Historical Mother';
  $handler->display->display_options['menu']['weight'] = '0';
  $handler->display->display_options['menu']['name'] = 'main-menu';
  $handler->display->display_options['menu']['context'] = 0;
  $handler->display->display_options['menu']['context_only_inline'] = 0;
  $export['admin_views'] = $view;

  return $export;
}
