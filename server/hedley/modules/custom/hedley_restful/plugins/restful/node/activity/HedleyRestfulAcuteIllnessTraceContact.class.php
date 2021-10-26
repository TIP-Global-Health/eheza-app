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
    'field_date_concluded',
  ];

  /**
   * {@inheritdoc}
   */
  protected $entityFields = [
    'field_referred_person',
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_contact_date',
    'field_date_concluded',
    'field_date_concluded',
  ];

}
