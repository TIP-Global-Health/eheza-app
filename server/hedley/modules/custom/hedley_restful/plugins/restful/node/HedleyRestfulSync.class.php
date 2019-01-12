<?php

/**
 * @file
 * Contains \HedleyRestfulSync.
 */

/**
 * Class HedleyRestfulSync.
 */
class HedleyRestfulSync extends \RestfulBase implements \RestfulDataProviderInterface {

  // The number of revisions we'll send in a single batch.
  const BATCH_SIZE = 50;

  /**
   * Overrides \RestfulBase::controllersInfo().
   */
  public static function controllersInfo() {
    return [
      '' => [
        \RestfulInterface::GET => 'getForAllDevices',
      ],
      '^.*$' => [
        \RestfulInterface::GET => 'getForHealthCenter',
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
      'child' => 'children',
      'clinic' => 'clinics',
      'health_center' => 'health_centers',
      'mother' => 'mothers',
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
      'family_planning' => 'family-plannings',
      'height' => 'heights',
      'muac' => 'muacs',
      'nutrition' => 'nutritions',
      'photo' => 'photos',
      'weight' => 'weights',
    ];
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

    $query = db_select('node_revision', 'nr');
    $query->join('node', 'n', 'n.nid = nr.nid');

    $query
      ->fields('nr', ['nid', 'vid'])
      ->fields('n', ['type'])
      ->orderBy('nr.vid', 'ASC')
      ->condition('nr.vid', $base, '>')
      ->condition('n.type', array_keys($handlersForTypes), 'IN');

    // First, get the total number of revisions that are greater than the base
    // revision. This will help the client show progress. Note that this
    // includes the revisions in the batch we will return (but not earlier
    // revisions).
    $count = $query->countQuery()->execute()->fetchField();

    // Then, get one batch worth of results.
    $batch = $query->range(0, self::BATCH_SIZE)->execute()->fetchAll();

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

    // Temporary, until we have an authentication plugin for this that checks
    // devices and authorizes a "sync" user.
    $account = user_load(1);

    $output = [];
    foreach ($optimized as $item) {
      $handler_name = $handlersForTypes[$item->type];
      $sub_handler = restful_get_restful_handler($handler_name);
      $sub_handler->setAccount($account);
      $output[] = $sub_handler->viewNodeRevision($item->nid, $item->vid);
    }

    return [
      'base_revision' => $base,
      'revision_count' => $count,
      'batch' => $output,
    ];
  }

  /**
   * Download revisions which the client does not have already.
   *
   * This is for those things that are relevant to the specified health center.
   *
   * @param int $id
   *   The ID of the health center.
   *
   * @return array
   *   A representation of the required revisions
   *
   * @throws \RestfulBadRequestException
   */
  public function getForHealthCenter($id) {
    $request = $this->getRequest();
    $handlersForTypes = $this->entitiesForHealthCenters();

    // Note that 0 is fine, so we can't use `empty`.
    if (!isset($request['base_revision'])) {
      // Must specify the last revision which the client already knows about.
      // This can be 0 if the client knows nothing.
      throw new RestfulBadRequestException('Must provide base_revision, indicating the last revision the client already has.');
    }

    $base = $request['base_revision'];

    $query = db_select('node_revision', 'nr');
    $query->join('node', 'n', 'n.nid = nr.nid');

    // To query for things relevant to a particular health center, we need to
    // go through the mother. We've got two ways to do that ... either directly
    // from mother measurements, or via the child. So, we'll start with the
    // child ...
    $query->leftJoin(
      'field_revision_field_child',
      'child',
      'child.entity_id = n.nid AND child.revision_id = nr.vid'
    );

    // Then, the OR below is getting the mother either directly from the
    // mesaurement (for mother measurements), or from the child (for child
    // measurements). Note that from this point on we're switching to
    // `field_data_...` rather than `field_revision_...`. This means that we're
    // looking at current data, rather than point in time, as we follow these
    // relationships. The alternative would be very complex, because the Drupal
    // tables have a `field_child_target_id` but no `field_child_revision_id`.
    // So, we don't have an easy way to know exactly which revision of the
    // child we're looking at. In principle, I suppose it's the highest
    // revision for that child less that nr.vid. A query could be written to do
    // that, but it probably would be overkill. This ought to be sufficient in
    // practice.
    $query->leftJoin(
      'field_data_field_mother',
      'm',
      'm.entity_id = child.field_child_target_id OR m.entity_id = n.nid'
    );

    // From the mother, we can get the clinic.
    $query->leftJoin(
      'field_data_field_clinic',
      'clinic',
      'm.field_mother_target_id = clinic.entity_id'
    );

    // And, from the clinic, the health center.
    $query->leftJoin(
      'field_data_field_health_center',
      'hc',
      'hc.entity_id = clinic.field_clinic_target_id'
    );

    $query
      ->fields('nr', ['nid', 'vid'])
      ->fields('n', ['type'])
      ->orderBy('nr.vid', 'ASC')
      ->condition('hc.field_health_center_target_id', $id)
      ->condition('nr.vid', $base, '>')
      ->condition('n.type', array_keys($handlersForTypes), 'IN');

    // First, get the total number of revisions that are greater than the base
    // revision. This will help the client show progress. Note that this
    // includes the revisions in the batch we will return (but not earlier
    // revisions).
    $count = $query->countQuery()->execute()->fetchField();

    // Then, get one batch worth of results.
    $batch = $query->range(0, self::BATCH_SIZE)->execute()->fetchAll();

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

    // Temporary, until we have an authentication plugin for this that checks
    // devices and authorizes a "sync" user.
    $account = user_load(1);

    $output = [];
    foreach ($optimized as $item) {
      $handler_name = $handlersForTypes[$item->type];
      $sub_handler = restful_get_restful_handler($handler_name);
      $sub_handler->setAccount($account);
      $output[] = $sub_handler->viewNodeRevision($item->nid, $item->vid);
    }

    return [
      'base_revision' => $base,
      'revision_count' => $count,
      'batch' => $output,
    ];
  }

}
