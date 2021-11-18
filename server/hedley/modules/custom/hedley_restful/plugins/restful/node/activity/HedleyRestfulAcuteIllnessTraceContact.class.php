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
    'field_village',
    'field_trace_outcome',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_symptoms_general',
    'field_symptoms_respiratory',
    'field_symptoms_respiratory',
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
    'field_expected',
  ];

}
