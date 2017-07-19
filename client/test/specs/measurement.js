var assert = require('assert');

describe('measurement module', function() {
  it('should save weight to server', function() {
    const tab = 'Weight';

    browser.login('aya');

    // see note in form-switch for patient selection
    browser.url('/#patient/41');
    browser.waitForVisible('.header.activities');

    // select tab
    browser.click('a=' + tab);
    browser.waitForVisible('span=' + tab + ':');

    // click save button
    browser.click('span=' + tab + ':button=Save');
  })
})
