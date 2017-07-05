<?php

/**
 * @file
 * Contains HedleyRestfulChildren.
 */

/**
 * Class HedleyRestfulChildren.
 */
class HedleyRestfulExamination extends HedleyRestfulEntityBaseNode {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['group'] = [
      'property' => 'field_group',
    ];

    $public_fields['last_assessment'] = [
      'property' => 'field_last_assessment',
      'process_callbacks' => [
        [$this, 'convertTimestampToIso8601'],
      ],
    ];

    return $public_fields;
  }

}
