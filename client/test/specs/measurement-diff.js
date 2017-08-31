var assert = require('assert');

describe('when updating a measurement form', function() {

  /**
   * Adjust the input of the current form.
   *
   * @param value
   *   The new value to set.
   */
  var adjustFormValue = function (value) {
    browser.setValue('input[type="text"]', value);
  };

  /**
   * Waiting for the up arrow to be shown.
   */
  var waitForGainedIndication = function () {
    browser.waitForVisible('.label-with-icon .icon-up');
  };

  /**
   * Waiting for the down arrow to be shown.
   */
  var waitForLostIndication = function () {
    browser.waitForVisible('.label-with-icon .icon-down');
  };

  /**
   * Get difference indication message.
   *
   * @returns {String|String[]}
   *   The indication message.
   */
  var getDiffFromIndication = function () {
    return browser.getText('.label-with-icon');
  };

  /**
   * Waiting for muac indication to be shown.
   */
  var waitForMuacIndication = function (expected) {
    browser.waitForVisible('.label-form.label-' + expected);
  }

  /**
   * Get text of muacIndication
   */
  var getMuacIndicationText = function (expected) {
    return browser.getText('.label-form.label-' + expected);
  };

  before(() => {
    browser.loginAndViewParticipantsPage('aya');
    browser.visitChildWithTodoTasks();
  })

  it('should display an indication when weight is gained', () => {
    const tab = 'Weight';

    // Select tab.
    browser.click('a=' + tab);
    browser.waitForVisible('h3=' + tab + ':');

    adjustFormValue(50);
    waitForGainedIndication();
    const result = getDiffFromIndication();
    assert.equal(result, '46 kg', 'Indication for the gained weight is incorrect.');
  })

  it('should display an indication when weight is lost', () => {
    adjustFormValue(1);
    waitForLostIndication();
    const result = getDiffFromIndication();
    assert.equal(result, '3 kg', 'Indication for the lost weight is incorrect.');
  })

  it('should display an indication when height is gained', () => {
    const tab = 'Height';

    // Select tab.
    browser.click('a=' + tab);
    browser.waitForVisible('h3=' + tab + ':');

    adjustFormValue(100);
    waitForGainedIndication();
    const result = getDiffFromIndication();
    assert.equal(result, '50 cm', 'Indication for the gained height is incorrect.');
  })

  it('should display an indication when height is lost', () => {
    adjustFormValue(10);
    waitForLostIndication();
    const result = getDiffFromIndication();
    assert.equal(result, '40 cm', 'Indication for the lost height is incorrect.');
  })

  it('should display an indication when MUAC is green', () => {
    const tab = 'MUAC';
    const expected = 'green'

    // Select tab.
    browser.click('a=' + tab);
    browser.waitForVisible('h3=Mid Upper Arm Circumference (MUAC):');

    adjustFormValue(50);
    waitForMuacIndication(expected);
    const result = getMuacIndicationText(expected);
    assert.equal(result, expected.toUpperCase(), 'Indication for the' + expected + ' MUAC is incorrect.');
  })

  it('should display an indication when MUAC is red', () => {
    const expected = 'red'

    adjustFormValue(5);
    waitForMuacIndication(expected);
    const result = getMuacIndicationText(expected);
    assert.equal(result, expected.toUpperCase(), 'Indication for the' + expected + ' MUAC is incorrect.');
  })

  after(() => browser.logout());

});
