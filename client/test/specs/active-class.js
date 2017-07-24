var assert = require('assert');
var Chance = require('chance');
var chance = new Chance();

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

    if (!browser.isExisting('//div[contains(@class, "column active")]//a[.="Weight"]')) {
      throw "The active class should appear on the weight element but it did not.";
    }

    if (browser.isExisting('//div[contains(@class, "column active")]//a[.="Height"]')) {
      throw "The active class should not appear on the height element but it did.";
    }

    browser.click("a=Height");
    browser.waitForVisible('//div[contains(@class, "column active")]//a[.="Height"]');

    if (browser.isExisting('//div[contains(@class, "column active")]//a[.="Weight"]')) {
      throw "The active class should not appear on the weight element but it did.";
    }

    if (!browser.isExisting('//div[contains(@class, "column active")]//a[.="Height"]')) {
      throw "The active class should not appear on the height element but it did.";
    }
  });

});
