var assert = require('assert');

describe('measurement module', function() {
  it('should save height to server', function() {
    const heightTab = 'Height';

    browser.login('aya');

    // see note in form-switch for patient selection
    browser.url('/#patient/55');
    browser.waitForVisible('.header.activities');

    // select height tab
    browser.click('a=' + heightTab);
    browser.waitForVisible('span=' + heightTab + ':');

    // click save button
    browser.click('span=' + heightTab + ':button=Save');
  })
})
