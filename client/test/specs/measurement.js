var assert = require('assert');

describe('measurement module', function() {
  it('should save weight to server', function() {
    const tab = 'Weight';

    browser.login('aya');

    // see note in form-switch for patient selection
    browser.url('/#patient/41');
    browser.waitForVisible('.ui.tasks.segment');

    // select tab
    browser.click('a=' + tab);
    browser.waitForVisible('h3=' + tab + ':');

    // click save button
    browser.click('div.weight button');
  })
})
