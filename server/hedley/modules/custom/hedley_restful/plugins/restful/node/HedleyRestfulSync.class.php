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
   * Overrides \RestfulBase::controllersInfo().
   */
  public static function controllersInfo() {
    return [
      '' => [
        \RestfulInterface::GET => 'getForAllDevices',
        \RestfulInterface::POST => 'handleChanges',
      ],
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
      'prenatal_participant' => 'prenatal-participants',
      'prenatal_encounter' => 'prenatal-encounters',
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
      'last_menstrual_period' => 'last-menstrual-periods',
      'medical_history' => 'medical-histories',
      'muac' => 'muacs',
      'nutrition' => 'nutritions',
      'obstetric_history' => 'obstetric-histories',
      'participant_consent' => 'participants-consent',
      'social_history' => 'social-histories',
      'photo' => 'photos',
      'vitals' => 'vitals',
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
    $handlersForTypes = $this->entitiesForAllDevices();

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

    if ($db_version < 2) {
      throw new RestfulBadRequestException('Must update your client before syncing further.');
    }

    // Start building up a query, which we'll use in a couple of ways.
    $query = db_select('node_revision', 'nr');
    $query->join('node', 'n', 'n.nid = nr.nid');

    $query
      ->fields('nr', ['nid', 'vid', 'timestamp'])
      ->fields('n', ['type'])
      ->condition('n.type', array_keys($handlersForTypes), 'IN');

    // Get the timestamp of the last revision. We'll also get a count of
    // remaining revisions, but the timestamp of the last revision will also
    // help us display how far out-of-date the client is.
    $last_revision_query = clone $query;

    $last_revision = $last_revision_query
      ->orderBy('nr.vid', 'DESC')
      ->range(0, 1)
      ->execute()
      ->fetchObject();

    $last_timestamp = $last_revision ? $last_revision->timestamp : 0;

    // Restrict to revisions the client doesn't already have.
    $query->condition('nr.vid', $base, '>');

    // Get the total number of revisions that are greater than the base
    // revision. This will help the client show progress. Note that this
    // includes the revisions in the batch we will return (but not earlier
    // revisions).
    $count = $query->countQuery()->execute()->fetchField();

    // Then, get one batch worth of results.
    $batch = $query
      ->orderBy('nr.vid', 'ASC')
      ->range(0, $this->getRange())
      ->execute()
      ->fetchAll();

    // As an optimization, if the same node ID occurs multiple times in this
    // batch, just send the last one. In principle, it would be nice to enable
    // this optimization across multiple batches as well. That is, it would be
    // nice to avoid sending a node now if it will be sent again in a later
    // batch. However, the difficulty is that we can't be sure that the client
    // will actually get to the later batch before being offline again. We
    // could immediately send the later revision, but then we'd be sending
    // changes out-of-order, which might have strange implications.  (For
    // instance, a field might reference an entity the client doesn't have
    // yet).  So, at least for the moment, it's better to do this optimization
    // within a single batch only. (Items can be out-of-order within the batch,
    // but that should be manageable, since we can at least be sure that the
    // client will get the whole batch or nothing).
    $optimized = [];
    foreach ($batch as $item) {
      $optimized[$item->nid] = $item;
    }
    $optimized = array_values($optimized);

    // Adjust the count if we've removed any items with our optimization.
    $count = $count - count($batch) + count($optimized);

    // Now, we wish to get a restful representation of each revision in this
    // batch. We need to represent the specific revision, rather than the
    // current revision, for the reasons noted above. (We can't be sure that
    // the client will get all batches before going offline, and if we send
    // revisions out-of-order then we might reference entities the client
    // doesn't have yet).
    $account = $this->getAccount();

    $output = [];
    foreach ($optimized as $item) {
      $handler_name = $handlersForTypes[$item->type];
      $sub_handler = restful_get_restful_handler($handler_name);
      $sub_handler->setAccount($account);
      $rendered = $sub_handler->viewNodeRevision($item->nid, $item->vid);

      // Also add in the timestamp.
      $rendered['timestamp'] = $item->timestamp;
      $output[] = $rendered;
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
    $handlersForTypes = $this->entitiesForHealthCenters();

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

    if ($db_version < 2) {
      throw new RestfulBadRequestException('Must update your client before syncing further.');
    }

    // Start building up a query, which we'll use in a couple of ways.
    $query = db_select('node_revision', 'nr');
    $query->join('node', 'n', 'n.nid = nr.nid');

    // Join the table that tracks which shards the node should be sent to.
    $query->join(
      'field_revision_field_shards',
      's',
      's.entity_id = nr.nid AND s.revision_id = nr.vid'
    );

    // And the table which will give us the UUID of the shard.
    $query->join(
      'field_data_field_uuid',
      'u',
      'u.entity_id = s.field_shards_target_id'
    );

    $query
      ->fields('nr', ['nid', 'vid', 'timestamp'])
      ->fields('n', ['type'])
      ->condition('u.field_uuid_value', $uuid)
      ->condition('n.type', array_keys($handlersForTypes), 'IN');

    // Get the timestamp of the last revision. We'll also get a count of
    // remaining revisions, but the timestamp of the last revision will also
    // help us display how far out-of-date the client is.
    $last_revision_query = clone $query;

    $last_revision = $last_revision_query
      ->orderBy('nr.vid', 'DESC')
      ->range(0, 1)
      ->execute()
      ->fetchObject();

    $last_timestamp = $last_revision ? $last_revision->timestamp : 0;

    // Restrict to revisions the client doesn't already have.
    $query->condition('nr.vid', $base, '>');

    // First, get the total number of revisions that are greater than the base
    // revision. This will help the client show progress. Note that this
    // includes the revisions in the batch we will return (but not earlier
    // revisions).
    $count = $query->countQuery()->execute()->fetchField();

    // Then, get one batch worth of results.
    $batch = $query
      ->orderBy('nr.vid', 'ASC')
      ->range(0, $this->getRange())
      ->execute()
      ->fetchAll();

    // As an optimization, if the same node ID occurs multiple times in this
    // batch, just send the last one. In principle, it would be nice to enable
    // this optimization across multiple batches as well. That is, it would be
    // nice to avoid sending a node now if it will be sent again in a later
    // batch. However, the difficulty is that we can't be sure that the client
    // will actually get to the later batch before being offline again. We
    // could immediately send the later revision, but then we'd be sending
    // changes out-of-order, which might have strange implications.  (For
    // instance, a field might reference an entity the client doesn't have
    // yet).  So, at least for the moment, it's better to do this optimization
    // within a single batch only. (Items can be out-of-order within the batch,
    // but that should be manageable, since we can at least be sure that the
    // client will get the whole batch or nothing).
    $optimized = [];
    foreach ($batch as $item) {
      $optimized[$item->nid] = $item;
    }
    $optimized = array_values($optimized);

    // Adjust the count if we've removed any items with our optimization.
    $count = $count - count($batch) + count($optimized);

    // Now, we wish to get a restful representation of each revision in this
    // batch. We need to represent the specific revision, rather than the
    // current revision, for the reasons noted above. (We can't be sure that
    // the client will get all batches before going offline, and if we send
    // revisions out-of-order then we might reference entities the client
    // doesn't have yet).
    $account = $this->getAccount();

    $output = [];
    foreach ($optimized as $item) {
      $handler_name = $handlersForTypes[$item->type];
      $sub_handler = restful_get_restful_handler($handler_name);
      $sub_handler->setAccount($account);
      $rendered = $sub_handler->viewNodeRevision($item->nid, $item->vid);

      // Also add in the timestamp.
      $rendered['timestamp'] = $item->timestamp;
      $output[] = $rendered;
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
            $sub_handler->post('', $data);
            break;
        }
      }
    }
    catch (Exception $e) {
      $transaction->rollback();
      throw $e;
    }

    return [];
  }

}
