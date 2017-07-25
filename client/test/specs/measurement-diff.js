'use strict';

var assert = require('assert');

describe('when updating a measurement form', function() {
  /**
   * Adjust the input of the current form.
   *
   * @param value
   *   The new value to set.
   */
  const adjustFormValue = () => (value) {
    browser.setValue('input[type="number"]', value);
  }

  /**
   * Waiting for the up arrow to be shown.
   */
  const waitForGainedIndication = () => {
    browser.waitForVisible('.label-up .icon-up');
  }

  /**
   * Waiting for the down arrow to be shown.
   */
  const waitForLostIndication = () => {
    browser.waitForVisible('.label-down .icon-down');
  }

  /**
   * Get the indication message of the 'gained' value.
   *
   * @returns {String|String[]}
   *   The indication message.
   */
  const getDiffFromGainedIndication = () => {
    return browser.getText('.label-up');
  }

  /**
   * Get the indication message of the 'lost' value.
   *
   * @returns {String|String[]}
   *   The indication message.
   */
  const getDiffFromLostIndication = () => {
    return browser.getText('.label-down');
  }

  before(() => {
    browser.loginAndViewPatientsPage('aya');
    browser.visitChildWithTodoTasks();
  })

  it('should display an indication when weight is gained', () => {
    const tab = 'Weight';

    // Select tab.
    browser.click('a=' + tab);
    browser.waitForVisible('h3=' + tab + ':');

    adjustFormValue(50);
    waitForGainedIndication();
    const result = getDiffFromGainedIndication();
    assert.equal(result, 'Gained\n46 kg', 'Indication for the gained weight is incorrect.');
  })

  it('should display an indication when weight is lost', () => {
    adjustFormValue(1);
    waitForLostIndication();
    const result = getDiffFromLostIndication();
    assert.equal(result, 'Lost\n3 kg', 'Indication for the lost weight is incorrect.');
  })

  it('should display an indication when height is gained', () => {
    const tab = 'Height';

    // Select tab.
    browser.click('a=' + tab);
    browser.waitForVisible('h3=' + tab + ':');

    adjustFormValue(100);
    waitForGainedIndication();
    const result = getDiffFromGainedIndication();
    assert.equal(result, 'Gained\n50 cm', 'Indication for the gained height is incorrect.');
  })

  it('should display an indication when height is lost', () => {
    adjustFormValue(10);
    waitForLostIndication();
    const result = getDiffFromLostIndication();
    assert.equal(result, 'Lost\n40 cm', 'Indication for the lost height is incorrect.');
  })

  it('should display an indication when MUAC is gained', () => {
    const tab = 'MUAC';

    // Select tab.
    browser.click('a=' + tab);
    browser.waitForVisible('h3=Mid Upper Arm Circumference (MUAC):');

    adjustFormValue(50);
    waitForGainedIndication();
    const result = getDiffFromGainedIndication();
    assert.equal(result, 'Gained\n37 cm', 'Indication for the gained MUAC is incorrect.');
  })

  it('should display an indication when MUAC is lost', () => {
    adjustFormValue(5);
    waitForLostIndication();
    const result = getDiffFromLostIndication();
    assert.equal(result, 'Lost\n8 cm', 'Indication for the lost MUAC is incorrect.');
  })

  after(() => browser.logout());

});
