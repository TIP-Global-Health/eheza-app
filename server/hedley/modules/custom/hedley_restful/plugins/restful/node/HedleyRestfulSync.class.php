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
    return [
      'catchment_area' => 'catchment_areas',
      'clinic' => 'clinics',
      'counseling_schedule' => 'counseling-schedule',
      'counseling_topic' => 'counseling-topics',
      'health_center' => 'health_centers',
      'nurse' => 'nurses',
      'participant_form' => 'participants-form',
      'person' => 'people',
      'pmtct_participant' => 'pmtct-participants',
      'relationship' => 'relationships',
      'session' => 'sessions',
    ];
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
    return [
      'attendance' => 'attendances',
      'counseling_session' => 'counseling-sessions',
      'family_planning' => 'family-plannings',
      'height' => 'heights',
      'muac' => 'muacs',
      'nutrition' => 'nutritions',
      'participant_consent' => 'participants-consent',
      'photo' => 'photos',
      'weight' => 'weights',
    ];
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
    $handlers_by_types = $this->entitiesForAllDevices();

    // Note that 0 is fine, so we can't use `empty`.
    if (!isset($request['base_revision'])) {
      // Must specify the last revision which the client already knows about.
      // This can be 0 if the client knows nothing.
      throw new RestfulBadRequestException('Must provide base_revision, indicating the last revision the client already has.');
    }

    $base = $request['base_revision'];

    // Check database version on client side ... refuse to send stuff until
    // they upgrade.
    if (!isset($request['db_version'])) {
      throw new RestfulBadRequestException('Must provide db_version, indicating the version of your local IndexedDB.');
    }

    $db_version = intval($request['db_version']);

    if ($db_version < HEDLEY_RESTFUL_CLIENT_SIDE_INDEXEDDB_SCHEMA_VERSION) {
      throw new RestfulBadRequestException('Must update your client before syncing further.');
    }

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

    // Check if we have HC UUID and then check if we need to send statistics for
    // the Dashboard of that HC.
    if (!empty($request['health_center_uuid'])) {
      // If we have HC then check the cache hash ID.
      if ($health_center_nid = hedley_restful_resolve_nid_for_uuid($request['health_center_uuid'])) {
        $cache_data = hedley_stats_handle_cache(HEDLEY_STATS_CACHE_GET, HEDLEY_STATS_SYNC_STATS_CACHE, $health_center_nid);

        $calculate_stats = FALSE;
        // Check if we need to calculate the statistics for this HC.
        if (!empty($cache_data) && !empty($request['stats_cache_hash'])) {
          if ($cache_data != $request['stats_cache_hash']) {
            // Cache hash from the frontend doesn't match the one we have in the
            // backend, this means that stats has been changed because it's only
            // reset when a measurement has changed.
            $calculate_stats = TRUE;
          }
        }
        else {
          // We either don't have cache hash yet or this frontend doesn't have
          // it, in any case, we will calculate the statistics and send it to
          // the frontend.
          // We don't cache here because each statistic has its own cache inside
          // the function.
          $calculate_stats = TRUE;
        }

        if ($calculate_stats) {
          // First check if the HC has "FBF" clinics, only those have stats for
          // the dashboards and we shouldn't send any stats from a HC without
          // FBF clinics.
          // Get all clinics of type 'FBF' for the HC.
          $fbf_clinic = hedley_health_center_get_clinics_by_health_center($health_center_nid, 'fbf', 1);
          if (empty($fbf_clinic)) {
            // We send empty stats so we don't upset the decoder in elm.
            $stats = [
              'type' => 'statistics',
              // UUID of the health center.
              'uuid' => $request['health_center_uuid'],
              'case_management' => [],
              'children_beneficiaries' => [],
              'completed_program' => [],
              'completed_program_count' => 0,
              'good_nutrition' => NULL,
              'family_planning' => [],
              'malnourished_beneficiaries' => [],
              'missed_sessions' => [],
              'missed_sessions_count' => 0,
              'total_beneficiaries' => NULL,
              'total_beneficiaries_incidence' => NULL,
              'total_encounters' => NULL,
            ];
          }
          else {
            // Calculate the stats.
            list($completed_program, $missed_sessions) = hedley_stats_get_session_attendance_stats_by_health_center($health_center_nid);
            $stats = [
              'type' => 'statistics',
              // UUID of the health center.
              'uuid' => $request['health_center_uuid'],
              'case_management' => hedley_stats_get_case_management($health_center_nid),
              'children_beneficiaries' => hedley_stats_get_children_beneficiaries_stats_by_period($health_center_nid, HEDLEY_STATS_PERIOD_THREE_MONTHS),
              'completed_program' => $completed_program,
              'completed_program_count' => 0,
              'good_nutrition' => hedley_stats_get_good_nutrition($health_center_nid),
              'family_planning' => hedley_stats_get_family_planning_stats_by_period($health_center_nid, HEDLEY_STATS_PERIOD_THREE_MONTHS),
              'malnourished_beneficiaries' => hedley_stats_get_malnourished_beneficiaries_stats_by_period($health_center_nid, HEDLEY_STATS_PERIOD_THREE_MONTHS),
              'missed_sessions' => $missed_sessions,
              'missed_sessions_count' => 0,
              'total_beneficiaries' => hedley_stats_get_fbf_beneficiaries_graphs_data($health_center_nid, HEDLEY_STATS_SYNC_TOTAL_BENEFICIARIES_GRAPH),
              'total_beneficiaries_incidence' => hedley_stats_get_fbf_beneficiaries_graphs_data($health_center_nid, HEDLEY_STATS_SYNC_TOTAL_BENEFICIARIES_INCIDENCE_GRAPH),
              'total_encounters' => hedley_stats_get_total_encounters($health_center_nid),
            ];

            $stats['stats_cache_hash'] = md5(serialize($stats));

            // Store in cache only the hash, inner statistics has their own
            // cache.
            hedley_stats_handle_cache(HEDLEY_STATS_CACHE_SET, HEDLEY_STATS_SYNC_STATS_CACHE, $health_center_nid, NULL, $stats['stats_cache_hash']);
          }

          $output[] = $stats;
        }
      }
    }

    return [
      'base_revision' => $base,
      'last_timestamp' => $last_timestamp,
      'revision_count' => $count,
      'batch' => $output,
    ];
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
    $handlers_by_types = $this->entitiesForHealthCenters();

    // Note that 0 is fine, so we can't use `empty`.
    if (!isset($request['base_revision'])) {
      // Must specify the last revision which the client already knows about.
      // This can be 0 if the client knows nothing.
      throw new RestfulBadRequestException('Must provide base_revision, indicating the last revision the client already has.');
    }

    $base = $request['base_revision'];

    // Check database version on client side ... refuse to send stuff until
    // they upgrade.
    if (!isset($request['db_version'])) {
      throw new RestfulBadRequestException('Must provide db_version, indicating the version of your local IndexedDB.');
    }

    $db_version = intval($request['db_version']);

    if ($db_version < HEDLEY_RESTFUL_CLIENT_SIDE_INDEXEDDB_SCHEMA_VERSION) {
      throw new RestfulBadRequestException('Must update your client before syncing further.');
    }

    $query = db_select('node', 'node');

    // Filter by Health center.
    hedley_restful_join_field_to_query($query, 'node', 'field_shards');

    // And the table which will give us the UUID of the shard.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_shards.field_shards_target_id");

    $query
      ->fields('node', ['type', 'nid', 'vid', 'created', 'changed'])
      ->condition('field_uuid.field_uuid_value', $uuid)
      ->condition('node.type', array_keys($handlers_by_types), 'IN');

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

        $data = [];
        foreach (array_keys($item['data']) as $key) {
          $value = $item['data'][$key];

          // We check if it's a valid UUID. Perhaps we ought to instead
          // check that the field is defined as an entity reference?
          if ($key != 'uuid' && is_string($value) && Uuid::isValid($value)) {
            $data[$key] = hedley_restful_uuid_to_nid($value);
          }
          elseif ($key == 'date_measured' && !empty($value)) {
            // Restful seems to want date values as timestamps -- should
            // investigate if there are other possibilities.
            $data[$key] = strtotime($value);
          }
          elseif ($key == 'birth_date' && !empty($value)) {
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
        ];

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

      throw $e;
    }

    $user = $account->name;
    $total = count($request['changes']);
    watchdog('debug', "Sync upload by $user with $total changes was successful");

    return [];
  }

}
