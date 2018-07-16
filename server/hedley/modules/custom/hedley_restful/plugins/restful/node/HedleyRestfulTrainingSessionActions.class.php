<?php

/**
 * @file
 * Contains \HedleyRestfulTrainingSessionActions.
 */

/**
 * Class HedleyRestfulTrainingSessionActions.
 *
 * Encapsulates some action which we want to take on training sessions as a
 * whole. So far, the actions are:
 *
 * - create_all
 *
 *   Creates a training session for each clinic for today, unless the clinic
 *   already has one.
 *
 * - delete_all
 *
 *   Delete all the training sessions.
 *
 * The distinction between a "training" session and a regular session is only
 * that "delete_all" will delete training sessions.  This makes it possible (on
 * the sandbox site) to:
 *
 * - manually create some permanent, historical sessions (which are not
 *   automatically deleted);
 *
 * - use "create_all" create some sessions to be used for training
 *
 * - use "delete_all" to reset the training sessions (leaving the permanent,
 *   manually created, sessions alone)
 *
 * The resource which we are modelling here is an "action" rather than a
 * "session". So, when you POST, you are creating an action. At the moment, we
 * perform that action immediately. However, you could imagine queueing the
 * action in some way. If we did that, we would return a representation of the
 * action with an ID, so that you could subsequently GET the action (to see
 * what its status was), or DELETE the action (to cancel it).
 */
class HedleyRestfulTrainingSessionActions extends HedleyRestfulEntityBaseNode {

  /**
   * Overrides \RestfulDataProviderEFQ::controllersInfo().
   *
   * For now, we just have POST, since we immediately perform the action upon
   * creating it. If we were queuing actions, we could also support GET, PUT
   * and DELETE (to get the status of the action, alter the action, or cancel
   * the action).
   */
  public static function controllersInfo() {
    return [
      '' => [
        \RestfulInterface::POST => 'performAction',
      ],
    ];
  }

  /**
   * Handle the action for creating/deleting taining sessions.
   *
   * @return array
   *   A representation of the action.
   *
   * @throws \RestfulBadRequestException
   */
  public function performAction() {
    $request = $this->getRequest();

    if (!isset($request['action']) || empty($request['action'])) {
      throw new RestfulBadRequestException('Action is not specified');
    }

    if ($request['action'] == 'create_all') {
      $this->createTrainingEntities();
    }
    elseif ($request['action'] == 'delete_all') {
      $this->deleteTrainingEntities();
    }
    else {
      throw new RestfulBadRequestException('Action is invalid');
    }

    // Conceptually, we've created an "action" and might queue it for
    // execution. For now, we just excecute it immediately, and return a
    // simulation of what we'd return if the action were itself an entity.
    return [
      0 => [
        'action' => $request['action'],
      ],
    ];
  }

  /**
   * Create training sessions for all existing clinics.
   *
   * This would be used by administrators on the sandbox website for creating
   * training sessions for all the existing clinics which are scheduled for the
   * day of creating them, meaning they will be open for the same day the admin
   * clicks on the "Create all training sessions" button.
   *
   * @throws \RestfulForbiddenException
   */
  protected function createTrainingEntities() {
    $this->checkTrainingSessionsAccess();

    // Get all clinics.
    $query = new EntityFieldQuery();
    $query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'clinic')
      ->propertyCondition('status', NODE_PUBLISHED);

    $scheduled_date = date('Y-m-d', REQUEST_TIME);

    // Eventually, should switch to queuing the action, or use db_select etc.
    hedley_restful_query_in_batches($query, 50, function ($offset, $count, $clinic_nids) use ($scheduled_date) {
      foreach ($clinic_nids as $clinic_nid) {
        if (hedley_schedule_clinic_has_sessions($clinic_nid, $scheduled_date)) {
          // Clinic has an active session for today, no need to create a session
          // for it.
          continue;
        }

        $node = entity_create('node', [
          'type' => 'session',
        ]);

        $wrapper = entity_metadata_wrapper('node', $node);

        $wrapper->field_clinic->set($clinic_nid);
        $wrapper->field_training->set(TRUE);
        $wrapper->field_scheduled_date->set([
          'value' => $scheduled_date,
          'value2' => $scheduled_date,
        ]);

        $wrapper->save();
      }
    });
  }

  /**
   * Delete all existing training sessions.
   *
   * This would be used by administrators on the sandbox website for deleting
   * all training sessions for meaning they will be all deleted when admin
   * clicks on the "Delete all training sessions" button, it will delete all
   * training session even if it's an old session opened on a past day.
   *
   * @throws \RestfulForbiddenException
   */
  public function deleteTrainingEntities() {
    $this->checkTrainingSessionsAccess();

    // Get all sessions.
    $query = new EntityFieldQuery();
    $query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'session')
      ->propertyCondition('status', NODE_PUBLISHED)
      ->fieldCondition('field_training', 'value', TRUE);

    // Eventually, should switch to queuing the action, or use db_select and
    // db_delete.
    hedley_restful_query_in_batches($query, 50, function ($offset, $count, $session_nids) {
      entity_delete_multiple('node', $session_nids);
    });
  }

  /**
   * Check if the current user has permissions to administer nodes.
   *
   * @throws \RestfulForbiddenException
   */
  protected function checkTrainingSessionsAccess() {
    if (!user_access('administer nodes', $this->getAccount())) {
      throw new \RestfulForbiddenException('You are not permitted to administer training sessions.');
    }
  }

}
