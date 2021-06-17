<?php

/**
 * @file
 * hedley_well_child.features.field_base.inc
 */

/**
 * Implements hook_field_default_field_bases().
 */
function hedley_well_child_field_default_field_bases() {
  $field_bases = array();

  // Exported field_base: 'field_ecd_signs'.
  $field_bases['field_ecd_signs'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_ecd_signs',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'respont-to-sound-with-sound' => 'RespontToSoundWithSound',
        'turn-head-when-called' => 'TurnHeadWhenCalled',
        'sit-without-support' => 'SitWithoutSupport',
        'smile-back' => 'SmileBack',
        'roll-tummy-to-back' => 'RollTummyToBack',
        'reach-for-toys' => 'ReachForToys',
        'use-simple-gestures' => 'UseSimpleGestures',
        'stand-on-their-own' => 'StandOnTheirOwn',
        'copy-during-play' => 'CopyDuringPlay',
        'say-mama-dada' => 'SayMamaDada',
        'can-hold-small-objects' => 'CanHoldSmallObjects',
        'looks-when-pointed-at' => 'LooksWhenPointedAt',
        'use-single-words' => 'UseSingleWords',
        'walk-without-help' => 'WalkWithoutHelp',
        'play-pretend' => 'PlayPretend',
        'point-to-things-of-interest' => 'PointToThingsOfInterest',
        'use-short-phrases' => 'UseShortPhrases',
        'interested-in-other-children' => 'InterestedInOtherChildren',
        'follow-simlpe-instructions' => 'FollowSimlpeInstructions',
        'kick-ball' => 'KickBall',
        'point-at-named-objects' => 'PointAtNamedObjects',
        'dress-themselves' => 'DressThemselves',
        'wash-hands-go-to-toiled' => 'WashHandsGoToToiled',
        'knows-colors-and-numbers' => 'KnowsColorsAndNumbers',
        'use-medium-phrases' => 'UseMediumPhrases',
        'play-make-believe' => 'PlayMakeBelieve',
        'follow-three-step-instructions' => 'FollowThreeStepInstructions',
        'stand-on-one-foot-five-seconds' => 'StandOnOneFootFiveSeconds',
        'use-long-phrases' => 'UseLongPhrases',
        'share-with-other-children' => 'ShareWithOtherChildren',
        'count-to-ten' => 'CountToTen',
        'none' => 'None of these',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_well_child_encounter'.
  $field_bases['field_well_child_encounter'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_well_child_encounter',
    'indexes' => array(
      'target_id' => array(
        0 => 'target_id',
      ),
    ),
    'locked' => 0,
    'module' => 'entityreference',
    'settings' => array(
      'handler' => 'base',
      'handler_settings' => array(
        'behaviors' => array(
          'views-select-list' => array(
            'status' => 0,
          ),
        ),
        'sort' => array(
          'type' => 'none',
        ),
        'target_bundles' => array(
          'well_child_encounter' => 'well_child_encounter',
        ),
      ),
      'target_type' => 'node',
    ),
    'translatable' => 0,
    'type' => 'entityreference',
  );

  return $field_bases;
}
