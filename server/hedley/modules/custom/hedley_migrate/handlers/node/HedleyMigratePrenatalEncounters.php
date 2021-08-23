<?php

/**
 * @file
 * Contains \HedleyMigratePrenatalEncounters.
 */

/**
 * Class HedleyMigratePrenatalEncounters.
 */
class HedleyMigratePrenatalEncounters extends HedleyMigrateIndividualEncounterBase {

  /**
   * {@inheritdoc}
   */
  protected $entityType = 'node';

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'prenatal_encounter';

}
