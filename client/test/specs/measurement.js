var assert = require('assert');

describe('measurement module', function() {
  it('should save the weight via the Save button', () => {
    const tab = 'Weight';

    browser.login('aya');
    browser.visitChildWithTodoTasks();

    // Select tab.
    browser.click('a=' + tab);
    browser.waitForVisible('h3=' + tab + ':');

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
})
