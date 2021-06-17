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
        'respont_to_sound_with_sound' => 'RespontToSoundWithSound',
        'turn_head_when_called' => 'TurnHeadWhenCalled',
        'sit_without_support' => 'SitWithoutSupport',
        'smile_back' => 'SmileBack',
        'roll_tummy_to_back' => 'RollTummyToBack',
        'reach_for_toys' => 'ReachForToys',
        'use_simple_gestures' => 'UseSimpleGestures',
        'stand_on_their_own' => 'StandOnTheirOwn',
        'copy_during_play' => 'CopyDuringPlay',
        'say_mama_dada' => 'SayMamaDada',
        'can_hold_small_objects' => 'CanHoldSmallObjects',
        'looks_when_pointed_at' => 'LooksWhenPointedAt',
        'use_single_words' => 'UseSingleWords',
        'walk_without_help' => 'WalkWithoutHelp',
        'play_pretend' => 'PlayPretend',
        'point_to_things_of_interest' => 'PointToThingsOfInterest',
        'use_short_phrases' => 'UseShortPhrases',
        'interested_in_other_children' => 'InterestedInOtherChildren',
        'follow_simlpe_instructions' => 'FollowSimlpeInstructions',
        'kick_ball' => 'KickBall',
        'point_at_named_objects' => 'PointAtNamedObjects',
        'dress_themselves' => 'DressThemselves',
        'wash_hands_go_to_toiled' => 'WashHandsGoToToiled',
        'knows_colors_and_numbers' => 'KnowsColorsAndNumbers',
        'use_medium_phrases' => 'UseMediumPhrases',
        'play_make_believe' => 'PlayMakeBelieve',
        'follow_three_step_instructions' => 'FollowThreeStepInstructions',
        'stand_on_one_foot_five_seconds' => 'StandOnOneFootFiveSeconds',
        'use_long_phrases' => 'UseLongPhrases',
        'share_with_other_children' => 'ShareWithOtherChildren',
        'count_to_ten' => 'CountToTen',
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
