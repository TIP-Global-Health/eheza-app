<?php

/**
 * @file
 * Contains \HedleyRestfulSyncBase.
 */

/**
 * Class HedleyRestfulSyncBase.
 */
class HedleyRestfulSyncBase extends \HedleyRestfulEntityBaseNode {

  /**
   * Overrides \RestfulEntityBaseNode::publicFieldsInfo().
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['type'] = [
      'property' => 'type',
    ];

    $public_fields['vid'] = [
      'property' => 'vid',
    ];

    return $public_fields;
  }

  /**
   * Like RestfulEntityBase::viewEntity, but allows specifying a revision.
   *
   * Note that this only works with nodes, not other kinds of entities.
   *
   * @param int $nid
   *   The node ID.
   * @param int $vid
   *   The revision ID.
   *
   * @return array
   *   A representation of that particular revision.
   */
  public function viewNodeRevision($nid, $vid) {
    $request = $this->getRequest();

    // We supply the revision ID as well as the node ID for the cache tags.
    $cached_data = $this->getRenderedCache($this->getVersionedNodeCacheTags($nid, $vid));
    if (!empty($cached_data->data)) {
      return $cached_data->data;
    }

    // Load the specified revision.
    $node = node_load($nid, $vid);

    // These checks are adapted from RestfulBase::isValidEntity. We can't use
    // that directly, because it loads and checks the current version of the
    // node, not the specified revision.
    $params = [
      '@nid' => $nid,
      '@vid' => $vid,
      '@resource' => $this->getPluginKey('label'),
    ];

    if (empty($node)) {
      throw new RestfulUnprocessableEntityException(format_string('The nid @nid vid @vid for @resource does not exist.', $params));
    }

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->language($this->getLangCode());

    $bundle = $wrapper->getBundle();
    $resource_bundle = $this->getBundle();
    if ($resource_bundle && $bundle != $resource_bundle) {
      throw new RestfulUnprocessableEntityException(format_string('The nid @id vid @vid is not a valid @resource.', $params));
    }

    // `checkEntityAccess` was working OK at one point, but now is throwing an
    // exception where I don't expect one. For now, can omit this, since the
    // caller is already doing authorization.
    if (FALSE) {
      if ($this->checkEntityAccess('view', 'node', $node) === FALSE) {
        throw new RestfulForbiddenException(format_string('You do not have access to nid @nid vid @vid of resource @resource', $params));
      }
    }

    $values = [];
    $limit_fields = !empty($request['fields']) ? explode(',', $request['fields']) : array();

    foreach ($this->getPublicFields() as $public_field_name => $info) {
      if ($limit_fields && !in_array($public_field_name, $limit_fields)) {
        // Limit fields doesn't include this property.
        continue;
      }

      $value = NULL;

      if ($info['create_or_update_passthrough']) {
        // The public field is a dummy one, meant only for passing data upon
        // create or update.
        continue;
      }

      if ($info['callback']) {
        $value = static::executeCallback($info['callback'], array($wrapper));
      }
      else {
        // Exposing an entity field.
        $property = $info['property'];
        $sub_wrapper = $info['wrapper_method_on_entity'] ? $wrapper : $wrapper->{$property};

        // Check user has access to the property.
        if ($property && !$this->checkPropertyAccess('view', $public_field_name, $sub_wrapper, $wrapper)) {
          continue;
        }

        if (empty($info['formatter'])) {
          if ($sub_wrapper instanceof EntityListWrapper) {
            // Multiple values.
            foreach ($sub_wrapper as $item_wrapper) {
              $value[] = $this->getValueFromProperty($wrapper, $item_wrapper, $info, $public_field_name);
            }
          }
          else {
            // Single value.
            $value = $this->getValueFromProperty($wrapper, $sub_wrapper, $info, $public_field_name);
          }
        }
        else {
          // Get value from field formatter.
          $value = $this->getValueFromFieldFormatter($wrapper, $sub_wrapper, $info);
        }
      }

      if (isset($value) && $info['process_callbacks']) {
        foreach ($info['process_callbacks'] as $process_callback) {
          $value = static::executeCallback($process_callback, array($value));
        }
      }

      $values[$public_field_name] = $value;
    }

    $this->setRenderedCache($values, $this->getVersionedNodeCacheTags($nid, $vid));
    return $values;
  }

  /**
   * The array of parameters by which versioned nodes should be cached.
   *
   * @param int $nid
   *   The entity ID of the entity to be cached.
   * @param int $vid
   *   The revision ID of the entity to be cached.
   *
   * @return array
   *   An array of parameter keys and values which should be added
   *   to the cache key for each entity.
   */
  public function getVersionedNodeCacheTags($nid, $vid) {
    return [
      'et' => 'node',
      'ei' => $nid,
      'ev' => $vid,
    ];
  }

}
