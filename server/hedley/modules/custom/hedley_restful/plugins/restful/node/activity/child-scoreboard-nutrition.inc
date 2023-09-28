<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Child Scoreboard Nutrition'),
  'resource' => 'child-scoreboard-nutrition',
  'name' => 'child-scoreboard-nutrition',
  'entity_type' => 'node',
  'bundle' => 'child_scoreboard_nutrition',
  'description' => t('Exports the Child Scoreboard Nutrition bundle.'),
  'class' => 'HedleyRestfulChildScoreboardNutrition',
  'authentication_types' => TRUE,
);
