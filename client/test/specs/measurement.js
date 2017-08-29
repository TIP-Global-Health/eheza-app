var assert = require('assert');

describe('measurement module', function() {
  it('should save the weight via the Save button', () => {
    const tab = 'Weight';

    browser.login('aya');
    browser.visitChildWithTodoTasks();

    // Select tab.
    browser.click('a=' + tab);

    // Wait for the tab.
    browser.waitForVisible('h3=' + tab + ':');

    // The Save button is disabled by default.
    const classesBefore = browser.getAttribute('.weight .button', 'class');
    assert.notEqual(classesBefore.indexOf('disabled'), -1, 'The button is not disabled anymore');

    // Filling the value.
    browser.setValueSafe('.weight input', '2');

    // Then it becomes enabled.
    const classesAfter = browser.getAttribute('.weight .button', 'class');
    assert.equal(classesAfter.indexOf('disabled'), -1, 'The button is not disabled anymore');

    // Click save button.
    browser.click('div.weight button');
  })

  it('should save the nutrition signs via the Save button', () => {
    const tab = 'Nutrition signs';

    browser.visitChildWithTodoTasks();

    // Select tab.
    browser.click('a=' + tab);

    // Wait for the tab.
    browser.waitForVisible('h3=Nutrition:');

    // The Save button is disabled by default.
    const classesBefore = browser.getAttribute('.nutrition .button', 'class');
    assert.notEqual(classesBefore.indexOf('disabled'), -1, 'The button is disabled at start');

    // Filling the value.
    browser.click('#dry-skin');

    // Then it becomes enabled.
    const classesAfter = browser.getAttribute('.nutrition .button', 'class');
    assert.equal(classesAfter.indexOf('disabled'), -1, 'The button is not disabled anymore');

    // Click save button.
    browser.click('div.nutrition button');

    browser.waitForVisible('h3=Photo:');
  })

  it('should save the muac via the Save button', () => {
    const tab = 'MUAC';

    // Select tab.
    browser.click('a=MUAC');
    browser.waitForVisible('h3=Mid Upper Arm Circumference (MUAC):');

    // The Save button is disabled by default.
    const classesBefore = browser.getAttribute('.muac .button', 'class');
    assert.notEqual(classesBefore.indexOf('disabled'), -1, 'The button is not disabled anymore');

    // Filling the value.
    browser.setValueSafe('.muac input', '2');

    // Then it becomes enabled.
    const classesAfter = browser.getAttribute('.muac .button', 'class');
    assert.equal(classesAfter.indexOf('disabled'), -1, 'The button is not disabled anymore');

    // Click save button.
    browser.click('div.muac button');
  });
})
