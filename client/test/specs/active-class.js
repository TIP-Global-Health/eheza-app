'use strict';
var assert = require('assert');

describe('Testing the active class appearance', () => {

  before(() => {
    browser.loginAndViewPatientsPage('aya');

    browser.visitChildWithTodoTasks();
  });

  it('Switching between sections', () => {
    if (browser.isExisting('.column.active')) {
      throw "The class active should not appear on the screen.";
    }

    browser.click("a=Weight");
    browser.waitForVisible('//div[contains(@class, "column active")]//a[.="Weight"]');

    assert.ok(!browser.isExisting('//div[contains(@class, "column active")]//a[.="Height"]'), "The active class should not appear on the height element.");

    browser.click("a=Height");
    browser.waitForVisible('//div[contains(@class, "column active")]//a[.="Height"]');

    assert.ok(!browser.isExisting('//div[contains(@class, "column active")]//a[.="Weight"]'), "The active class should not appear on the weight element.");

  });

});
