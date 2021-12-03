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
    'field_referred_person',
    'field_first_name',
    'field_second_name',
    'field_phone_number',
    'field_contact_date',
    'field_date_concluded',
    'field_gender',
    'field_trace_outcome',
    'field_last_follow_up_date',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_symptoms_general',
    'field_symptoms_respiratory',
    'field_symptoms_gi',
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
    'field_last_follow_up_date',
  ];

}
