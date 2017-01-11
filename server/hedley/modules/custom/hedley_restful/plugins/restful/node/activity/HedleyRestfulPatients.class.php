<?php

/**
 * @file
 * Contains HedleyRestfulPatients.
 */

/**
 * Class HedleyRestfulPatients.
 */
class HedleyRestfulPatients extends RestfulEntityBaseMultipleBundles {

  /**
   * Overrides \RestfulDataProviderEFQ::controllersInfo().
   */
  public static function controllersInfo() {
    return [
      '' => [
        // GET returns a list of entities.
        \RestfulInterface::GET => 'getList',
      ],
      '^.*$' => [
        \RestfulInterface::GET => 'viewEntities',
        \RestfulInterface::HEAD => 'viewEntities',
      ],
    ];
  }

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  public function viewEntities($ids_string) {
    $ids = array_unique(array_filter(explode(',', $ids_string)));
    $output = array();

    // Use the correct RESTful resource to view the IDs.
    // See \RestfulEntityBaseMultipleBundles::getList().
    // @todo: Move this into RESTful.

    $account = $this->getAccount();
    $request = $this->getRequest();

    // Pre-load all entities.
    $entity_type = $this->entityType;
    $entities = entity_load($entity_type, $ids);

    $return = [];

    $handlers = [];
    $resources_info = $this->getBundles();

    foreach ($entities as $entity) {
      // Call each handler by its registered bundle.
      list($id,, $bundle) = entity_extract_ids($this->getEntityType(), $entity);
      if (empty($handlers[$bundle])) {
        $version = $this->getVersion();
        $handlers[$bundle] = restful_get_restful_handler($resources_info[$bundle], $version['major'], $version['minor']);
      }

      $bundle_handler = $handlers[$bundle];
      $bundle_handler->setAccount($account);
      $bundle_handler->setRequest($request);
      $output[] = $bundle_handler->viewEntity($id);
    }

    return $output;
  }

}
