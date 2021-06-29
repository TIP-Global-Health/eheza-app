<?php

/**
 * @file
 * Contains \HedleyRestfulSync.
 */

use Ramsey\Uuid\Uuid;

/**
 * Class HedleyRestfulSync.
 */
class HedleyRestfulSync extends \RestfulBase implements \RestfulDataProviderInterface {

  /**
   * Nodes synced to all devices.
   *
   * The content types and their restful handler for nodes that
   * we sync to all devices.
   */
  const HEDLEY_RESTFUL_DB_QUERY_RANGE = 500;

  /**
   * Overrides \RestfulBase::controllersInfo().
   */
  public static function controllersInfo() {
    return [
      '' => [
        \RestfulInterface::GET => 'getForAllDevices',
        \RestfulInterface::POST => 'handleChanges',
      ],
      // The UUID of the Health center.
      '^.*$' => [
        \RestfulInterface::GET => 'getForHealthCenter',
        \RestfulInterface::POST => 'handleChanges',
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
   * Entities which we send to every device (regardless of health center).
   *
   * For `session`, note that the entity merely has the scheduled dates, so it
   * is fairly small -- it's not the whole session data.
   *
   * For `child` and `mother`, it is convenient for every device to have all
   * basic info. We'll probably want to omit the actual photos, though, and
   * only provide those based on the health centres the device is assigned to.
   *
   * What we're leaving out here is:
   *
   * - `device`
   *
   *    Because devices probably don't need to know about other devies.
   *
   * - `family_planning`, `height`, `muac`, `nutrition`, `photo`, `weight`
   *
   *    Because it seems sensible to send measurements only to devices that
   *    need them, based on which health centers the device will be used with.
   *
   * @return array
   *   Array where the keys are entity machine names and the values are the
   *   machine names for the restful handler to use when syncing.
   */
  public function entitiesForAllDevices() {
    return HEDLEY_RESTFUL_ALL_DEVICES;
  }

  /**
   * Entities which we send for particular health centers.
   *
   * So far, this is basically the various measurements.
   *
   * @return array
   *   Array where the keys are entity machine names and the values are the
   *   machine names for the restful handler to use when syncing.
   */
  public function entitiesForHealthCenters() {
    return HEDLEY_RESTFUL_SHARDED;
  }

  /**
   * All entities, with the machine names of their handlers.
   *
   * Basically, entitiesForAllDevices + entitiesForHealthCenters.
   *
   * @return array
   *   Array where the keys are entity machine names and the values are the
   *   machine names for the restful handler to use when syncing.
   */
  public function allEntities() {
    return array_merge(
      $this->entitiesForAllDevices(),
      $this->entitiesForHealthCenters()
    );
  }

  /**
   * Download revisions which the client does not have already.
   *
   * This is for those things that are relevant no matter which health centres
   * the device is interested in.
   *
   * @return array
   *   A representation of the required revisions
   *
   * @throws \RestfulBadRequestException
   */
  public function getForAllDevices() {
    $request = $this->getRequest();
    $this->validateDbVersion($request['db_version']);

    // Note that 0 is fine, so we can't use `empty`.
    if (!isset($request['base_revision'])) {
      // Must specify the last revision which the client already knows about.
      // This can be 0 if the client knows nothing.
      throw new RestfulBadRequestException('Must provide base_revision, indicating the last revision the client already has.');
    }
    $base = $request['base_revision'];

    $handlers_by_types = $this->entitiesForAllDevices();

    // Start building up a query, which we'll use in a couple of ways.
    $query = db_select('node', 'node');

    $query
      ->fields('node', ['nid', 'vid', 'created', 'changed', 'type'])
      ->condition('node.type', array_keys($handlers_by_types), 'IN');

    // Get the timestamp of the last revision. We'll also get a count of
    // remaining nodes, but the timestamp of the last revision will also
    // help us display how far out-of-date the client is.
    $last_revision_query = clone $query;

    $last_revision = $last_revision_query
      ->orderBy('node.vid', 'DESC')
      ->range(0, 1)
      ->execute()
      ->fetchObject();

    $last_timestamp = $last_revision ? $last_revision->changed : 0;

    // Restrict to revisions the client doesn't already have.
    $query->condition('node.vid', $base, '>');

    // Get the total number of nodes that are greater than the base
    // revision. This will help the client show progress.
    $count = $query->countQuery()->execute()->fetchField();

    // Then, get one batch worth of results.
    $batch = $query
      ->orderBy('node.vid', 'ASC')
      ->range(0, self::HEDLEY_RESTFUL_DB_QUERY_RANGE)
      ->execute()
      ->fetchAll();

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

    $return = [
      'base_revision' => $base,
      'last_timestamp' => $last_timestamp,
      'revision_count' => $count,
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

    // Filter by Shards.
    hedley_general_join_field_to_query($query, 'node', 'field_shards', FALSE);

    // And the table which will give us the UUID of the shard.
    hedley_general_join_field_to_query($query, 'node', 'field_uuid', FALSE, "field_shards.field_shards_target_id", 'field_uuid_shards');

    $query
      ->fields('node', ['type', 'nid', 'vid', 'created', 'changed'])
      ->condition('field_uuid_shards.field_uuid_value', $uuid)
      ->condition('node.type', array_keys($handlers_by_types), 'IN');

    $query->distinct();

    // Get the timestamp of the last revision. We'll also get a count of
    // remaining revisions, but the timestamp of the last revision will also
    // help us display how far out-of-date the client is.
    $last_revision_query = clone $query;

    $last_revision = $last_revision_query
      ->orderBy('node.vid', 'DESC')
      ->range(0, 1)
      ->execute()
      ->fetchObject();

    $last_timestamp = $last_revision ? $last_revision->changed : 0;

    // Restrict to revisions the client doesn't already have.
    $query->condition('node.vid', $base, '>');

    // First, get the total number of revisions that are greater than the base
    // revision. This will help the client show progress. Note that this
    // includes the revisions in the batch we will return (but not earlier
    // revisions).
    $count_query = clone $query;
    $count = $count_query
      ->countQuery()
      ->execute()
      ->fetchField();

    // Then, get one batch worth of results.
    $batch = $query
      ->orderBy('node.vid', 'ASC')
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
      'last_timestamp' => $last_timestamp,
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
      // There's no cached data for health center - trigger statistics
      // calculation by adding an AQ item.
      hedley_general_add_task_to_advanced_queue_by_id(HEDLEY_STATS_CALCULATE_STATS, $health_center_id, [
        'health_center_nid' => $health_center_id,
      ]);
      return $return;
    }

    $request = $this->getRequest();
    if (!isset($request['stats_cache_hash']) || $cached_hash !== $request['stats_cache_hash']) {
      // Statistics hash does not match that of the server, which indicates
      // that we need to send updated statistics to client.
      // Note: we just want to pull existing data, without updating cache.
      $return['batch'][] = hedley_stats_calculate_stats_for_health_center($health_center_id, $cached_hash);
    }

    // If we got this far, we know that statistics on client are
    // up to date - return empty response.
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
    watchdog('debug', 'Processing sync upload request');
    $request = $this->getRequest();
    $this->validateDbVersion($request['db_version']);
    $handlersForTypes = $this->allEntities();
    $account = $this->getAccount();

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

        $dateFields = [
          'date_measured',
          'birth_date',
          'last_menstrual_period',
          'date_concluded',
          'expected_date_concluded',
          'appointment_confirmation',
        ];

        $data = [];
        foreach (array_keys($item['data']) as $key) {
          $value = $item['data'][$key];

          // We check if it's a valid UUID. Perhaps we ought to instead
          // check that the field is defined as an entity reference?
          if ($key != 'uuid' && is_string($value) && Uuid::isValid($value)) {
            $data[$key] = hedley_restful_uuid_to_nid($value);
          }
          elseif (in_array($key, $dateFields) && !empty($value)) {
            // Restful seems to want date values as timestamps -- should
            // investigate if there are other possibilities.
            $data[$key] = strtotime($value);
          }
          else {
            $data[$key] = $value;
          }
        }

        // Some properties cannot be set. For `type`, that's fine. For
        // `status`, we'll need to figure out how to unpublish things through
        // this route.
        $ignored = [
          'type',
          'status',
          'shard',
          // When creating a session, we provide clinic_type so it is
          // recorded on client. We don't actually need to pass this through,
          // so, we filter it out here.
          'clinic_type',
          // We do not support marking content as deleted from client,
          // therefore, we do not want to pass 'deleted' indication.
          // Also, not most content types do not have 'field_deleted, and
          // passing 'deleted' indicator will cause error.
          'deleted',
        ];

        // Do not ignore 'health center' field for person,
        // as this is what actually associates person with health center.
        if ($item['type'] != 'person') {
          $ignored[] = 'health_center';
        }

        foreach ($ignored as $i) {
          unset($data[$i]);
        }

        switch ($item['method']) {
          case 'PATCH':
            $nid = hedley_restful_uuid_to_nid($item['uuid']);
            $sub_handler->patch($nid, $data);
            break;

          case 'POST':
            $nid = hedley_restful_resolve_nid_for_uuid($item['uuid']);
            // Check if node with provided UUID already exists.
            // If it does, we don't do anything, as this is duplicate request.
            // Otherwise, we can proceed with content creation.
            if ($nid === FALSE) {
              $sub_handler->post('', $data);
            }
            break;
        }
      }
    }
    catch (Exception $e) {
      $transaction->rollback();

      $m1 = '[MAIN] - ';
      $m2 = '[DATA] - ';
      foreach ($item as $k => $v) {
        $m1 .= "  $k: $v  ||| ";
      }

      foreach ($item['data'] as $k => $v) {
        $value = is_array($v) ? implode(', ', $v) : $v;

        $m2 .= "  $k: $value";
      }

      watchdog('debug', $m1, [], WATCHDOG_ERROR);
      watchdog('debug', $m2, [], WATCHDOG_ERROR);

      $details = $m1 . PHP_EOL . PHP_EOL . $m2;
      hedley_restful_report_sync_incident('content-upload', $item['uuid'], $account->uid, $details);

      throw $e;
    }

    $user = $account->name;
    $total = count($request['changes']);
    watchdog('debug', "Sync upload by $user with $total changes was successful");

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
