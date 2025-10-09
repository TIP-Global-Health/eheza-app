<?php

/**
 * @file
 * Contains \HedleyRestfulSync.
 */

use Ramsey\Uuid\Uuid;

/**
 * Class HedleyRestfulReportsData.
 */
class HedleyRestfulReportsData extends \RestfulBase implements \RestfulDataProviderInterface {

  /**
   * Nodes synced to all devices.
   *
   * The content types and their restful handler for nodes that
   * we sync to all devices.
   */
  const HEDLEY_RESTFUL_QUERY_BATCH = 500;

  /**
   * Overrides \RestfulBase::controllersInfo().
   */
  public static function controllersInfo() {
    return [
      '' => [
        \RestfulInterface::GET => 'getData',
      ],
      '^.*$' => [
        // We do not allow any calls here.
      ],
    ];
  }

  /**
   * Implements \RestfulInterface::publicFieldsInfo().
   */
  public function publicFieldsInfo() {
    return [];
  }

  /**
   *
   * @return array
   *   A representation of the required revisions
   *
   * @throws \RestfulBadRequestException
   */
  public function getData() {
    $request = $this->getRequest();

    $app_type = $request['app_type'];
    if (empty($app_type)) {
      throw new RestfulBadRequestException('Must provide the application type parameter.');
    }

    // Note that 0 is fine, so we can't use `empty`.
    if (!isset($request['base_revision'])) {
      // Must specify the last revision which the client already knows about.
      // This can be 0 if the client knows nothing.
      throw new RestfulBadRequestException('Must provide base_revision, indicating the last revision the client already has.');
    }
    $base = $request['base_revision'];

    $province = $request['province'];
    $district = $request['district'];
    $sector = $request['sector'];
    $cell = $request['cell'];
    $village = $request['village'];
    $health_center = $request['health_center'];

    $query = new EntityFieldQuery();
    $query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'person')
      ->propertyCondition('status', NODE_PUBLISHED)
      ->propertyCondition('nid', $base, '>')
      ->fieldCondition('field_reports_data', 'value', NULL, 'IS NOT NULL')
      ->addTag('exclude_deleted');

    if (!empty($province)) {
      $query->fieldCondition('field_province', 'value', $province);
    }

    if (!empty($district)) {
      $query->fieldCondition('field_district', 'value', $district);
    }

    if (!empty($sector)) {
      $query->fieldCondition('field_sector', 'value', $sector);
    }

    if (!empty($cell)) {
      $query->fieldCondition('field_cell', 'value', $cell);
    }

    if (!empty($village)) {
      $query->fieldCondition('field_village', 'value', $village);
    }

    if (!empty($health_center)) {
      $query->fieldCondition('field_shards', 'target_id', $health_center);
    }

    $result = $query
      ->range(0, self::HEDLEY_RESTFUL_QUERY_BATCH)
      ->propertyOrderBy('nid')
      ->execute();

    if (empty($result['node'])) {
      // No more items left.
      return [];
    }

    $batch_by_node_type = [];

    foreach ($batch as $item) {
      $batch_by_node_type[$item->type][] = $item;
    }

    $output = [];
    foreach ($batch_by_node_type as $node_type => $items) {
      $handler_name = $handlers_by_types[$node_type];
      $sub_handler = restful_get_restful_handler($handler_name);
      $sub_handler->setAccount($account);

      $node_ids = [];
      foreach ($items as $item) {
        $node_ids[] = $item->nid;
      }

      $rendered_items = $sub_handler->viewWithDbSelect($node_ids);
      $output = array_merge($output, $rendered_items);
    }

    // Generate list of enabled features.
    $available_features = [
      'gps_coordinates',
      'group_education',
      'hiv_management',
      'ncda',
      'report_to_whatsapp',
      'stock_management',
      'tuberculosis_management',
    ];
    $enabled_features = array_filter(
      $available_features,
      function ($feature) {
        return variable_get("hedley_admin_feature_{$feature}_enabled", FALSE);
      }
    );

    $return = [
      'base_revision' => $base,
      'revision_count' => $count,
      'rollbar_token' => variable_get('hedley_general_rollbar_token', ''),
      'site' => variable_get('hedley_general_site_name', ''),
      'features' => implode(' ', $enabled_features),
    ];

    if (!empty($request['access_token'])) {
      $device_user_id = hedley_restful_resolve_device_by_token($request['access_token']);
      if ($device_user_id) {
        $device_user = user_load($device_user_id);
        $device_name = $device_user->name;
        $words = explode(' ', $device_name);
        if (end($words) == 'Robot') {
          array_splice($words, -1);
        }
        $return['device_name'] = implode(' ', $words);
      }
    }

    $return['batch'] = $output;

    return $return;
  }

  /**
   * Download revisions which the client does not have already.
   *
   * This is for those things that are relevant to the specified health center.
   *
   * @param string $uuid
   *   The UUID of the health center.
   *
   * @return array
   *   A representation of the required revisions
   *
   * @throws \RestfulBadRequestException
   */
  public function getForHealthCenter($uuid) {
    $request = $this->getRequest();
    $this->validateDbVersion($request['db_version']);

    $handlers_by_types = $this->entitiesForHealthCenters();

    if (isset($request['statistics'])) {
      return self::getForHealthCenterStatistics($uuid);
    }

    // Note that 0 is fine, so we can't use `empty`.
    if (!isset($request['base_revision'])) {
      // Must specify the last revision which the client already knows about.
      // This can be 0 if the client knows nothing.
      throw new RestfulBadRequestException('Must provide base_revision, indicating the last revision the client already has.');
    }

    $base = $request['base_revision'];

    $query = db_select('node', 'node');

    $query
      ->fields('node', ['type', 'nid', 'vid', 'created'])
      ->condition('node.type', array_keys($handlers_by_types), 'IN')
      ->condition('node.vid', $base, '>');

    // Filter by Shards.
    hedley_general_join_field_to_query($query, 'node', 'field_shards', FALSE);

    // And the table which will give us the UUID of the shard.
    hedley_general_join_field_to_query($query, 'node', 'field_uuid', FALSE, "field_shards.field_shards_target_id", 'field_uuid_shards');

    $query->condition('field_uuid_shards.field_uuid_value', $uuid);

    // First, get the total number of revisions that are greater than the base
    // revision. This will help the client show progress. Note that this
    // includes the revisions in the batch we will return (but not earlier
    // revisions).
    $count = $query->countQuery()->execute()->fetchField();

    // Then, get one batch worth of results.
    $batch = $query
      ->orderBy('node.vid')
      ->range(0, self::HEDLEY_RESTFUL_DB_QUERY_RANGE)
      ->execute()
      ->fetchAll();

    // Now, we wish to get a restful representation of each revision in this
    // batch. We need to represent the specific revision, rather than the
    // current revision, for the reasons noted above. (We can't be sure that
    // the client will get all batches before going offline, and if we send
    // revisions out-of-order then we might reference entities the client
    // doesn't have yet).
    $account = $this->getAccount();

    $batch_by_node_type = [];

    foreach ($batch as $item) {
      $batch_by_node_type[$item->type][] = $item;
    }

    $output = [];
    foreach ($batch_by_node_type as $node_type => $items) {
      $handler_name = $handlers_by_types[$node_type];
      $sub_handler = restful_get_restful_handler($handler_name);
      $sub_handler->setAccount($account);

      $node_ids = [];
      foreach ($items as $item) {
        $node_ids[] = $item->nid;
      }

      $rendered_items = $sub_handler->viewWithDbSelect($node_ids);
      $output = array_merge($output, $rendered_items);
    }

    return [
      'base_revision' => $base,
      'revision_count' => $count,
      'batch' => $output,
    ];
  }

  /**
   * Download statistics for Health Center.
   *
   * @param string $uuid
   *   The UUID of the health center.
   *
   * @return array
   *   A representation of the required revisions.
   */
  public function getForHealthCenterStatistics($uuid) {
    $return = [
      'batch' => [],
    ];

    $health_center_id = hedley_restful_resolve_nid_for_uuid($uuid);
    if (!$health_center_id) {
      // Can not resolve health center - return empty response.
      return $return;
    }

    $cached_hash = hedley_stats_handle_cache(HEDLEY_STATS_CACHE_GET, HEDLEY_STATS_SYNC_STATS_CACHE, $health_center_id);
    if (empty($cached_hash)) {
      // There's no cached data for health center - schedule statistics
      // calculation.
      hedley_stats_schedule_statistics_calculation_for_health_center($health_center_id);

      return $return;
    }

    $request = $this->getRequest();
    if (!isset($request['stats_cache_hash']) || $cached_hash !== $request['stats_cache_hash']) {
      // Health center statistics hash does not match that of the server, which
      // indicates that we need to send updated statistics to client.
      // So, we return what we have stored in cache, with updated hash
      // for health center statistics.
      $stats = hedley_stats_pull_stats_for_health_center($health_center_id, $cached_hash);

      // If statistics were successfully pulled, we return them.
      // Otherwise, returning an empty result.
      if (!empty($stats)) {
        $return['batch'][] = $stats;
      }
    }

    return $return;
  }

  /**
   * Handle changes sent from a client.
   *
   * @return array
   *   We don't seem to need to return anything (yet). We'll just
   *   throw an exception if we can't complete the sync, for one
   *   reason or another.
   *
   * @throws \RestfulBadRequestException
   */
  public function handleChanges() {
    watchdog('hedley_restful', 'Processing sync upload request');
    $stopper = time();
    $request = $this->getRequest();
    $this->validateDbVersion($request['db_version']);
    $handlersForTypes = $this->allEntities();
    $account = $this->getAccount();

    // Properties that require data manipulation.
    $dateFields = [
      'date_measured',
      'birth_date',
      'last_menstrual_period',
      'date_concluded',
      'expected_date_concluded',
      'appointment_confirmation',
      'immunisation_date',
      'asap_immunisation_date',
      'pediatric_visit_date',
      'contact_date',
      'last_follow_up_date',
      'execution_date',
      'resilience_start_date',
      'expiration_date',
      'positive_result_date',
      'next_visit_date',
    ];
    $multiDateFields = [
      'administration_dates',
    ];
    $multiEntitiesFields = [
      'participating_patients',
    ];
    $freeTextFields = [
      'label',
      'first_name',
      'second_name',
    ];

    // Properties cannot be set.
    // We filter them out for request to succeed.
    $ignoredFields = [
      'type',
      'status',
      'shard',
      // Node label produces notice when set through Restful, so
      // instead of passing it, it's generated by backend logic.
      'label',
      // When creating a session, we provide clinic_type, so it is
      // recorded on client. We don't actually need to pass this through,
      // so, we filter it out here.
      'clinic_type',
      // We do not support marking content as deleted from client,
      // therefore, we do not want to pass 'deleted' indication.
      // Also, most content types do not have 'field_deleted, and
      // passing 'deleted' indicator will cause error.
      'deleted',
      // Field health_centers and villages are sent when editing a nurse.
      // They fail the request, because sent as UUIDs. We could try to
      // convert them to node IDs, but since they can not be edited on
      // client, we simply ignore them.
      'health_centers',
      'villages',
      // Property sent during person creation.
      'save_gps_location',
    ];

    // We'd like this entire operation to succeed or fail as a whole, so that
    // we don't have deal with partially-successful updates. So, we create a
    // transaction.
    $transaction = db_transaction();

    try {
      foreach ($request['changes'] as $item) {
        $handler_name = $handlersForTypes[$item['type']];
        if (empty($handler_name)) {
          throw new RestfulBadRequestException("{$item['type']} is an unknown type.");
        }

        $sub_handler = restful_get_restful_handler($handler_name);
        $sub_handler->setAccount($account);

        // Do not ignore 'health center' field for person and stock_update,
        // as this is what associates the node with health center.
        $ignored = $ignoredFields;
        if (!in_array($item['type'], ['person', 'stock_update'])) {
          $ignored[] = 'health_center';
        }

        $data = [];
        foreach (array_keys($item['data']) as $key) {
          $value = $item['data'][$key];

          if (in_array($key, $ignored)) {
            continue;
          }

          // If we got so far, key should be accounted for.
          if (in_array($key, $dateFields) && !empty($value)) {
            // Restful wants date values as timestamps.
            $data[$key] = strtotime($value);
          }
          elseif (in_array($key, $multiDateFields) && !empty($value)) {
            foreach ($value as $date) {
              // Restful wants date values as timestamps.
              $data[$key][] = strtotime($date);
            }
          }
          elseif (in_array($key, $multiEntitiesFields) && !empty($value)) {
            foreach ($value as $uuid) {
              if (Uuid::isValid($uuid)) {
                $data[$key][] = hedley_restful_uuid_to_nid($uuid);
              }
            }
          }
          elseif (in_array($key, $freeTextFields) && !empty($value)) {
            // Verify there are only plain characters at patient name.
            $data[$key] = trim(preg_replace('/[^a-zA-Z0-9_ -]/s', '', $value));
          }
          elseif ($key != 'uuid' && is_string($value) && Uuid::isValid($value)) {
            $data[$key] = hedley_restful_uuid_to_nid($value);
          }
          elseif ($key == 'resilience_messages') {
            $data[$key] = json_encode($value);
          }
          else {
            $data[$key] = $value;
          }
        }

        switch ($item['method']) {
          case 'PATCH':
            $nid = hedley_restful_uuid_to_nid($item['uuid']);
            $sub_handler->patch($nid, $data);
            break;

          case 'POST':
            $nid = FALSE;
            // Check if node with provided UUID already exists.
            // If it does, we don't do anything, as this is duplicate request.
            // Otherwise, we can proceed with content creation.
            if (!empty($item['uuid'])) {
              $nid = hedley_restful_resolve_nid_for_uuid($item['uuid']);
            }
            if ($nid === FALSE) {
              $sub_handler->post('', $data);
            }
            break;
        }
      }
    }
    catch (Exception $e) {
      $transaction->rollback();

      $main = '[MAIN] -';
      $data = '[DATA] -';
      foreach ($item as $k => $v) {
        if ($k == 'data') {
          continue;
        }
        $main .= "  $k: $v";
      }
      foreach ($item['data'] as $k => $v) {
        $value = is_array($v) ? implode(', ', $v) : $v;
        $data .= "  $k: $value";
      }

      $details = $main . PHP_EOL . PHP_EOL . $data;
      watchdog('hedley_restful', $details, [], WATCHDOG_ERROR);

      // Create sync incident, only when UUID can not be resolved.
      if (strpos($e->getMessage(), 'Could not find UUID:') === 0) {
        $uuid = !empty($item['uuid']) ? $item['uuid'] : 'no-uuid';
        hedley_restful_report_sync_incident('content-upload', $uuid, $account->uid, $details);
      }

      throw $e;
    }

    $user = $account->name;
    $stopper = time() - $stopper;
    $total = count($request['changes']);
    watchdog('hedley_restful', "[$stopper sec] Sync upload by $user with $total changes was successful");

    return [];
  }

  /**
   * Validates that client originating the request is updated to latest version.
   *
   * @param string $db_version
   *   Version of local IndexedDB, passed as part of request.
   *
   * @throws \RestfulBadRequestException
   */
  public function validateDbVersion($db_version) {
    if (empty($db_version)) {
      throw new RestfulBadRequestException('Must provide db_version, indicating the version of your local IndexedDB.');
    }

    if (intval($db_version) < HEDLEY_RESTFUL_CLIENT_SIDE_INDEXEDDB_SCHEMA_VERSION) {
      throw new RestfulBadRequestException('Must update your client before syncing further.');
    }
  }

}
