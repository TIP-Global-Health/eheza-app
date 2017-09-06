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
  });

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

  it('should verify checkbox selection logic Nutrition Form', () => {
    const tab = 'Nutrition signs';

    browser.visitChildWithTodoTasks();

    // Select tab.
    browser.click('a=' + tab);

    // Wait for the tab.
    browser.waitForVisible('h3=Nutrition:');

    // Select 2 signs and make sure they're checked.
    browser.click('#dry-skin');
    browser.verifyCheckboxChecked('#dry-skin');
    browser.click('#edema');
    browser.verifyCheckboxChecked('#edema');

    // Select 'None of these'.
    browser.click('#none-of-these');
    browser.verifyCheckboxChecked('#none-of-these');

    // Verify that previously selected signs are not checked.
    browser.verifyCheckboxChecked('#dry-skin', false);
    browser.verifyCheckboxChecked('#edema', false);

    // Select another sign.
    browser.click('#apathy');

    // Verify that 'None of these' is not checked.
    browser.verifyCheckboxChecked('#none-of-these', false);
  });

});
