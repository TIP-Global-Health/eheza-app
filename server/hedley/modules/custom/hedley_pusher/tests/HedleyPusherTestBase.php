<?php

/**
 * Class HedlyTestsBase.
 *
 * Base class for the tests.
 *
 * todo: find a better location.
 */
class HedleyPusherTestBase extends DrupalWebTestCase {

  /**
   * The profile name.
   *
   * @var string
   */
  protected $profile = 'hedley';

  /**
   * Overrides DrupalWebTestCase::setUp().
   */
  protected function setUp() {
    parent::setUp(['hedley_tests']);
    $this->drupalCreateContentType(['type' => 'hedley_tests_node_type']);
  }

}
