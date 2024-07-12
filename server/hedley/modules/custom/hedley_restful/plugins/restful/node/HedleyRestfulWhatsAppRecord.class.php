<?php

/**
 * @file
 * Contains HedleyRestfulWhatsAppRecord.
 */

/**
 * Class HedleyRestfulWhatsAppRecord.
 */
class HedleyRestfulWhatsAppRecord extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    // The label is purely decorative.
    unset($public_fields['label']);

    $public_fields['person'] = [
      'property' => 'field_person',
      'sub_property' => 'field_uuid',
    ];

    $public_fields['date_measured'] = [
      'property' => 'field_date_measured',
      'process_callbacks' => [
        [$this, 'renderDate2'],
      ],
    ];

    $public_fields['language'] = [
      'property' => 'field_language',
    ];

    $public_fields['report_type'] = [
      'property' => 'field_report_type',
    ];

    $public_fields['phone_number'] = [
      'property' => 'field_phone_number',
    ];

    $public_fields['screenshot'] = [
      'property' => 'field_screenshot',
      'process_callbacks' => [
        [$this, 'imageProcess'],
      ],
    ];

    $public_fields['date_concluded'] = [
      'property' => 'field_date_concluded',
      'process_callbacks' => [
        [$this, 'renderDate2'],
      ],
    ];

    return $public_fields;
  }

}
