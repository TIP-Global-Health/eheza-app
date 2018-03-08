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
  /* Relationship: Entity Reference: Referenced Entity */
  $handler->display->display_options['relationships']['field_session_target_id']['id'] = 'field_session_target_id';
  $handler->display->display_options['relationships']['field_session_target_id']['table'] = 'field_data_field_session';
  $handler->display->display_options['relationships']['field_session_target_id']['field'] = 'field_session_target_id';
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title']['id'] = 'title';
  $handler->display->display_options['fields']['title']['table'] = 'node';
  $handler->display->display_options['fields']['title']['field'] = 'title';
  $handler->display->display_options['fields']['title']['label'] = 'Name';
  $handler->display->display_options['fields']['title']['alter']['word_boundary'] = FALSE;
  $handler->display->display_options['fields']['title']['alter']['ellipsis'] = FALSE;
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title_1']['id'] = 'title_1';
  $handler->display->display_options['fields']['title_1']['table'] = 'node';
  $handler->display->display_options['fields']['title_1']['field'] = 'title';
  $handler->display->display_options['fields']['title_1']['relationship'] = 'field_mother_target_id';
  $handler->display->display_options['fields']['title_1']['label'] = 'Mother';
  /* Field: Content: Clinic */
  $handler->display->display_options['fields']['field_clinic']['id'] = 'field_clinic';
  $handler->display->display_options['fields']['field_clinic']['table'] = 'field_data_field_clinic';
  $handler->display->display_options['fields']['field_clinic']['field'] = 'field_clinic';
  $handler->display->display_options['fields']['field_clinic']['relationship'] = 'field_mother_target_id';
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
    'child' => 'child',
  );

  /* Display: Child */
  $handler = $view->new_display('page', 'Child', 'page');
  $handler->display->display_options['defaults']['fields'] = FALSE;
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title']['id'] = 'title';
  $handler->display->display_options['fields']['title']['table'] = 'node';
  $handler->display->display_options['fields']['title']['field'] = 'title';
  $handler->display->display_options['fields']['title']['label'] = 'Name';
  $handler->display->display_options['fields']['title']['alter']['word_boundary'] = FALSE;
  $handler->display->display_options['fields']['title']['alter']['ellipsis'] = FALSE;
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title_1']['id'] = 'title_1';
  $handler->display->display_options['fields']['title_1']['table'] = 'node';
  $handler->display->display_options['fields']['title_1']['field'] = 'title';
  $handler->display->display_options['fields']['title_1']['relationship'] = 'field_mother_target_id';
  $handler->display->display_options['fields']['title_1']['label'] = 'Mother';
  /* Field: Content: Clinic */
  $handler->display->display_options['fields']['field_clinic']['id'] = 'field_clinic';
  $handler->display->display_options['fields']['field_clinic']['table'] = 'field_data_field_clinic';
  $handler->display->display_options['fields']['field_clinic']['field'] = 'field_clinic';
  $handler->display->display_options['fields']['field_clinic']['relationship'] = 'field_mother_target_id';
  $handler->display->display_options['fields']['field_clinic']['settings'] = array(
    'bypass_access' => 0,
    'link' => 0,
  );
  /* Field: Content: Edit link */
  $handler->display->display_options['fields']['edit_node']['id'] = 'edit_node';
  $handler->display->display_options['fields']['edit_node']['table'] = 'views_entity_node';
  $handler->display->display_options['fields']['edit_node']['field'] = 'edit_node';
  $handler->display->display_options['fields']['edit_node']['label'] = 'Edit';
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
  /* Relationship: Mother */
  $handler->display->display_options['relationships']['reverse_field_clinic_node']['id'] = 'reverse_field_clinic_node';
  $handler->display->display_options['relationships']['reverse_field_clinic_node']['table'] = 'node';
  $handler->display->display_options['relationships']['reverse_field_clinic_node']['field'] = 'reverse_field_clinic_node';
  $handler->display->display_options['relationships']['reverse_field_clinic_node']['ui_name'] = 'Mother';
  $handler->display->display_options['relationships']['reverse_field_clinic_node']['label'] = 'Mother';
  $handler->display->display_options['defaults']['fields'] = FALSE;
  /* Field: User: Name */
  $handler->display->display_options['fields']['name']['id'] = 'name';
  $handler->display->display_options['fields']['name']['table'] = 'users';
  $handler->display->display_options['fields']['name']['field'] = 'name';
  $handler->display->display_options['fields']['name']['relationship'] = 'uid';
  $handler->display->display_options['fields']['name']['label'] = 'Created by';
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
  $handler->display->display_options['fields']['nid']['relationship'] = 'reverse_field_clinic_node';
  $handler->display->display_options['fields']['nid']['group_type'] = 'count';
  $handler->display->display_options['fields']['nid']['label'] = 'Mothers assigned';
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
  /* Filter criterion: Content: Type */
  $handler->display->display_options['filters']['type_1']['id'] = 'type_1';
  $handler->display->display_options['filters']['type_1']['table'] = 'node';
  $handler->display->display_options['filters']['type_1']['field'] = 'type';
  $handler->display->display_options['filters']['type_1']['relationship'] = 'reverse_field_clinic_node';
  $handler->display->display_options['filters']['type_1']['value'] = array(
    'mother' => 'mother',
  );
  $handler->display->display_options['filters']['type_1']['group'] = 1;
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
  $handler->display->display_options['fields']['field_scheduled_date']['settings'] = array(
    'format_type' => 'custom',
    'custom_date_format' => 'd/m/Y',
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
  $handler->display->display_options['fields']['field_scheduled_date_1']['alter']['text'] = '[field_scheduled_date] - [field_scheduled_date_1] ';
  $handler->display->display_options['fields']['field_scheduled_date_1']['settings'] = array(
    'format_type' => 'custom',
    'custom_date_format' => 'd/m/Y',
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
  $handler->display->display_options['fields']['created']['date_format'] = 'custom';
  $handler->display->display_options['fields']['created']['custom_date_format'] = 'd/m/Y';
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

  /* Display: Mother */
  $handler = $view->new_display('page', 'Mother', 'page_3');
  $handler->display->display_options['defaults']['title'] = FALSE;
  $handler->display->display_options['title'] = 'Mothers';
  $handler->display->display_options['defaults']['relationships'] = FALSE;
  /* Relationship: Child */
  $handler->display->display_options['relationships']['reverse_field_mother_node']['id'] = 'reverse_field_mother_node';
  $handler->display->display_options['relationships']['reverse_field_mother_node']['table'] = 'node';
  $handler->display->display_options['relationships']['reverse_field_mother_node']['field'] = 'reverse_field_mother_node';
  $handler->display->display_options['relationships']['reverse_field_mother_node']['ui_name'] = 'Child';
  $handler->display->display_options['relationships']['reverse_field_mother_node']['label'] = 'Child';
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
  $handler->display->display_options['fields']['title']['label'] = 'Name';
  $handler->display->display_options['fields']['title']['alter']['word_boundary'] = FALSE;
  $handler->display->display_options['fields']['title']['alter']['ellipsis'] = FALSE;
  /* Field: COUNT(DISTINCT Content: Nid) */
  $handler->display->display_options['fields']['nid']['id'] = 'nid';
  $handler->display->display_options['fields']['nid']['table'] = 'node';
  $handler->display->display_options['fields']['nid']['field'] = 'nid';
  $handler->display->display_options['fields']['nid']['relationship'] = 'reverse_field_mother_node';
  $handler->display->display_options['fields']['nid']['group_type'] = 'count_distinct';
  $handler->display->display_options['fields']['nid']['label'] = 'Children';
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title_1']['id'] = 'title_1';
  $handler->display->display_options['fields']['title_1']['table'] = 'node';
  $handler->display->display_options['fields']['title_1']['field'] = 'title';
  $handler->display->display_options['fields']['title_1']['relationship'] = 'field_clinic_target_id';
  $handler->display->display_options['fields']['title_1']['label'] = 'Clinic';
  $handler->display->display_options['fields']['title_1']['link_to_node'] = FALSE;
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
    'mother' => 'mother',
  );
  /* Filter criterion: Content: Type */
  $handler->display->display_options['filters']['type_2']['id'] = 'type_2';
  $handler->display->display_options['filters']['type_2']['table'] = 'node';
  $handler->display->display_options['filters']['type_2']['field'] = 'type';
  $handler->display->display_options['filters']['type_2']['relationship'] = 'reverse_field_mother_node';
  $handler->display->display_options['filters']['type_2']['value'] = array(
    'child' => 'child',
  );
  $handler->display->display_options['path'] = 'admin/content/mothers';
  $handler->display->display_options['menu']['type'] = 'normal';
  $handler->display->display_options['menu']['title'] = 'Mothers';
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
  /* Relationship: Content: Author */
  $handler->display->display_options['relationships']['uid']['id'] = 'uid';
  $handler->display->display_options['relationships']['uid']['table'] = 'node';
  $handler->display->display_options['relationships']['uid']['field'] = 'uid';
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
    'custom_date_format' => 'd/m/Y',
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
  /* Field: User: Name */
  $handler->display->display_options['fields']['name']['id'] = 'name';
  $handler->display->display_options['fields']['name']['table'] = 'users';
  $handler->display->display_options['fields']['name']['field'] = 'name';
  $handler->display->display_options['fields']['name']['relationship'] = 'uid';
  $handler->display->display_options['fields']['name']['label'] = 'Nurse';
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
  /* Relationship: Mother */
  $handler->display->display_options['relationships']['reverse_field_clinic_node']['id'] = 'reverse_field_clinic_node';
  $handler->display->display_options['relationships']['reverse_field_clinic_node']['table'] = 'node';
  $handler->display->display_options['relationships']['reverse_field_clinic_node']['field'] = 'reverse_field_clinic_node';
  $handler->display->display_options['relationships']['reverse_field_clinic_node']['ui_name'] = 'Mother';
  $handler->display->display_options['relationships']['reverse_field_clinic_node']['label'] = 'Mother';
  $handler->display->display_options['defaults']['fields'] = FALSE;
  /* Field: User: Name */
  $handler->display->display_options['fields']['name']['id'] = 'name';
  $handler->display->display_options['fields']['name']['table'] = 'users';
  $handler->display->display_options['fields']['name']['field'] = 'name';
  $handler->display->display_options['fields']['name']['relationship'] = 'uid';
  $handler->display->display_options['fields']['name']['label'] = 'Created by';
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
  $handler->display->display_options['fields']['nid']['relationship'] = 'reverse_field_clinic_node';
  $handler->display->display_options['fields']['nid']['group_type'] = 'count';
  $handler->display->display_options['fields']['nid']['label'] = 'Mothers assigned';
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
  $handler->display->display_options['fields']['field_scheduled_date']['label'] = '';
  $handler->display->display_options['fields']['field_scheduled_date']['exclude'] = TRUE;
  $handler->display->display_options['fields']['field_scheduled_date']['settings'] = array(
    'format_type' => 'custom',
    'custom_date_format' => 'd/m/Y',
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
  $handler->display->display_options['fields']['field_scheduled_date_1']['alter']['text'] = '[field_scheduled_date] - [field_scheduled_date_1] ';
  $handler->display->display_options['fields']['field_scheduled_date_1']['settings'] = array(
    'format_type' => 'custom',
    'custom_date_format' => 'd/m/Y',
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
  /* Relationship: Clinic */
  $handler->display->display_options['relationships']['field_clinic_target_id']['id'] = 'field_clinic_target_id';
  $handler->display->display_options['relationships']['field_clinic_target_id']['table'] = 'field_data_field_clinic';
  $handler->display->display_options['relationships']['field_clinic_target_id']['field'] = 'field_clinic_target_id';
  $handler->display->display_options['relationships']['field_clinic_target_id']['ui_name'] = 'Clinic';
  $handler->display->display_options['relationships']['field_clinic_target_id']['label'] = 'Clinic';
  /* Relationship: Child */
  $handler->display->display_options['relationships']['reverse_field_mother_node']['id'] = 'reverse_field_mother_node';
  $handler->display->display_options['relationships']['reverse_field_mother_node']['table'] = 'node';
  $handler->display->display_options['relationships']['reverse_field_mother_node']['field'] = 'reverse_field_mother_node';
  $handler->display->display_options['relationships']['reverse_field_mother_node']['ui_name'] = 'Child';
  $handler->display->display_options['relationships']['reverse_field_mother_node']['label'] = 'Child';
  $handler->display->display_options['defaults']['fields'] = FALSE;
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title']['id'] = 'title';
  $handler->display->display_options['fields']['title']['table'] = 'node';
  $handler->display->display_options['fields']['title']['field'] = 'title';
  $handler->display->display_options['fields']['title']['label'] = 'Name';
  $handler->display->display_options['fields']['title']['link_to_node'] = FALSE;
  /* Field: COUNT(DISTINCT Content: Nid) */
  $handler->display->display_options['fields']['nid']['id'] = 'nid';
  $handler->display->display_options['fields']['nid']['table'] = 'node';
  $handler->display->display_options['fields']['nid']['field'] = 'nid';
  $handler->display->display_options['fields']['nid']['relationship'] = 'reverse_field_mother_node';
  $handler->display->display_options['fields']['nid']['group_type'] = 'count_distinct';
  $handler->display->display_options['fields']['nid']['label'] = 'Children';
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title_1']['id'] = 'title_1';
  $handler->display->display_options['fields']['title_1']['table'] = 'node';
  $handler->display->display_options['fields']['title_1']['field'] = 'title';
  $handler->display->display_options['fields']['title_1']['relationship'] = 'field_clinic_target_id';
  $handler->display->display_options['fields']['title_1']['label'] = 'Clinic';
  $handler->display->display_options['fields']['title_1']['link_to_node'] = FALSE;
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
    'mother' => 'mother',
  );
  /* Filter criterion: Content: Type */
  $handler->display->display_options['filters']['type_3']['id'] = 'type_3';
  $handler->display->display_options['filters']['type_3']['table'] = 'node';
  $handler->display->display_options['filters']['type_3']['field'] = 'type';
  $handler->display->display_options['filters']['type_3']['relationship'] = 'reverse_field_mother_node';
  $handler->display->display_options['filters']['type_3']['value'] = array(
    'child' => 'child',
  );
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
  /* Relationship: Author */
  $handler->display->display_options['relationships']['uid']['id'] = 'uid';
  $handler->display->display_options['relationships']['uid']['table'] = 'node';
  $handler->display->display_options['relationships']['uid']['field'] = 'uid';
  $handler->display->display_options['relationships']['uid']['ui_name'] = 'Author';
  $handler->display->display_options['relationships']['uid']['label'] = 'Author';
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
    'custom_date_format' => 'd/m/Y',
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
  /* Field: User: Name */
  $handler->display->display_options['fields']['name']['id'] = 'name';
  $handler->display->display_options['fields']['name']['table'] = 'users';
  $handler->display->display_options['fields']['name']['field'] = 'name';
  $handler->display->display_options['fields']['name']['relationship'] = 'uid';
  $handler->display->display_options['fields']['name']['label'] = 'Nurse';
  $handler->display->display_options['fields']['name']['link_to_user'] = FALSE;
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

  /* Display: Measurements 2 */
  $handler = $view->new_display('page', 'Measurements 2', 'page_5');
  $handler->display->display_options['defaults']['title'] = FALSE;
  $handler->display->display_options['title'] = 'Historical Measurements';
  $handler->display->display_options['defaults']['group_by'] = FALSE;
  $handler->display->display_options['defaults']['relationships'] = FALSE;
  /* Relationship: Entity Reference: Referenced Entity */
  $handler->display->display_options['relationships']['field_session_target_id']['id'] = 'field_session_target_id';
  $handler->display->display_options['relationships']['field_session_target_id']['table'] = 'field_data_field_session';
  $handler->display->display_options['relationships']['field_session_target_id']['field'] = 'field_session_target_id';
  $handler->display->display_options['relationships']['field_session_target_id']['label'] = 'Session';
  /* Relationship: Entity Reference: Referenced Entity */
  $handler->display->display_options['relationships']['field_child_target_id']['id'] = 'field_child_target_id';
  $handler->display->display_options['relationships']['field_child_target_id']['table'] = 'field_data_field_child';
  $handler->display->display_options['relationships']['field_child_target_id']['field'] = 'field_child_target_id';
  $handler->display->display_options['relationships']['field_child_target_id']['label'] = 'Child';
  /* Relationship: Entity Reference: Referenced Entity */
  $handler->display->display_options['relationships']['field_mother_target_id']['id'] = 'field_mother_target_id';
  $handler->display->display_options['relationships']['field_mother_target_id']['table'] = 'field_data_field_mother';
  $handler->display->display_options['relationships']['field_mother_target_id']['field'] = 'field_mother_target_id';
  $handler->display->display_options['relationships']['field_mother_target_id']['relationship'] = 'field_child_target_id';
  $handler->display->display_options['relationships']['field_mother_target_id']['label'] = 'Mother';
  /* Relationship: Content: Author */
  $handler->display->display_options['relationships']['uid']['id'] = 'uid';
  $handler->display->display_options['relationships']['uid']['table'] = 'node';
  $handler->display->display_options['relationships']['uid']['field'] = 'uid';
  $handler->display->display_options['relationships']['uid']['label'] = 'Nurse';
  $handler->display->display_options['defaults']['fields'] = FALSE;
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
  $handler->display->display_options['fields']['field_date_measured']['settings'] = array(
    'format_type' => 'custom',
    'custom_date_format' => 'Y-m-d',
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
  /* Field: User: Name */
  $handler->display->display_options['fields']['name']['id'] = 'name';
  $handler->display->display_options['fields']['name']['table'] = 'users';
  $handler->display->display_options['fields']['name']['field'] = 'name';
  $handler->display->display_options['fields']['name']['relationship'] = 'uid';
  $handler->display->display_options['fields']['name']['label'] = 'Nurse';
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title_1']['id'] = 'title_1';
  $handler->display->display_options['fields']['title_1']['table'] = 'node';
  $handler->display->display_options['fields']['title_1']['field'] = 'title';
  $handler->display->display_options['fields']['title_1']['relationship'] = 'field_mother_target_id';
  $handler->display->display_options['fields']['title_1']['label'] = 'Mother Name';
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title']['id'] = 'title';
  $handler->display->display_options['fields']['title']['table'] = 'node';
  $handler->display->display_options['fields']['title']['field'] = 'title';
  $handler->display->display_options['fields']['title']['relationship'] = 'field_child_target_id';
  $handler->display->display_options['fields']['title']['label'] = 'Child Name';
  /* Field: Content: Date birth */
  $handler->display->display_options['fields']['field_date_birth']['id'] = 'field_date_birth';
  $handler->display->display_options['fields']['field_date_birth']['table'] = 'field_data_field_date_birth';
  $handler->display->display_options['fields']['field_date_birth']['field'] = 'field_date_birth';
  $handler->display->display_options['fields']['field_date_birth']['relationship'] = 'field_child_target_id';
  $handler->display->display_options['fields']['field_date_birth']['label'] = 'Child DOB';
  $handler->display->display_options['fields']['field_date_birth']['settings'] = array(
    'format_type' => 'custom',
    'custom_date_format' => 'Y-d-m',
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
    'prefix_suffix' => 1,
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
    'prefix_suffix' => 1,
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
  /* Field: Content: Z-Score Age */
  $handler->display->display_options['fields']['field_z_score_age']['id'] = 'field_z_score_age';
  $handler->display->display_options['fields']['field_z_score_age']['table'] = 'field_data_field_z_score_age';
  $handler->display->display_options['fields']['field_z_score_age']['field'] = 'field_z_score_age';
  $handler->display->display_options['fields']['field_z_score_age']['settings'] = array(
    'thousand_separator' => '',
    'prefix_suffix' => 1,
  );
  /* Field: Content: Z-Score Length */
  $handler->display->display_options['fields']['field_z_score_length']['id'] = 'field_z_score_length';
  $handler->display->display_options['fields']['field_z_score_length']['table'] = 'field_data_field_z_score_length';
  $handler->display->display_options['fields']['field_z_score_length']['field'] = 'field_z_score_length';
  $handler->display->display_options['fields']['field_z_score_length']['settings'] = array(
    'thousand_separator' => '',
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
    'height' => 'height',
    'muac' => 'muac',
    'nutrition' => 'nutrition',
    'weight' => 'weight',
  );
  $handler->display->display_options['path'] = 'admin/measurements2';
  $handler->display->display_options['menu']['type'] = 'normal';
  $handler->display->display_options['menu']['title'] = 'Historical';
  $handler->display->display_options['menu']['weight'] = '0';
  $handler->display->display_options['menu']['name'] = 'main-menu';
  $handler->display->display_options['menu']['context'] = 0;
  $handler->display->display_options['menu']['context_only_inline'] = 0;
  $export['admin_views'] = $view;

  return $export;
}
