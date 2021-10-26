<?php

/**
 * @file
 * Contains HedleyRestfulAcuteIllnessTraceContact.
 */

/**
 * Class HedleyRestfulAcuteIllnessTraceContact.
 */
class HedleyRestfulAcuteIllnessTraceContact extends HedleyRestfulAcuteIllnessActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_first_name',
    'field_second_name',
    'field_phone_number',
    'field_contact_date',
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_contact_date',
  ];

}
