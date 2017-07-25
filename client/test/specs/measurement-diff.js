var assert = require('assert');

describe('measurement module', function() {

  before(() => {
    browser.loginAndViewPatientsPage('aya');
    browser.visitChildWithTodoTasks();
  })

  it('should display an indication when weight is gained', () => {
    const tab = 'Weight';

    // Select tab.
    browser.click('a=' + tab);
    browser.waitForVisible('h3=' + tab + ':');

    browser.setValue('input[type="number"]', 50);
    browser.waitForVisible('.label-up .icon-up');
    const result = browser.getText('.label-up');
    assert.equal(result, 'Gained\n46 kg', 'Indication for the gained weight is incorrect.');
  })

  it('should display an indication when weight is lost', () => {
    browser.setValue('input[type="number"]', 1);
    browser.waitForVisible('.label-down .icon-down');
    const result = browser.getText('.label-down');
    assert.equal(result, 'Lost\n3 kg', 'Indication for the lost weight is incorrect.');
  })

  it('should display an indication when height is gained', () => {
    const tab = 'Height';

    // Select tab.
    browser.click('a=' + tab);
    browser.waitForVisible('h3=' + tab + ':');

    browser.setValue('input[type="number"]', 100);
    browser.waitForVisible('.label-up .icon-up');
    const result = browser.getText('.label-up');
    assert.equal(result, 'Gained\n50 cm', 'Indication for the gained height is incorrect.');
  })

  it('should display an indication when height is lost', () => {
    browser.setValue('input[type="number"]', 10);
    browser.waitForVisible('.label-down .icon-down');
    const result = browser.getText('.label-down');
    assert.equal(result, 'Lost\n40 cm', 'Indication for the lost height is incorrect.');
  })

  it('should display an indication when MUAC is gained', () => {
    const tab = 'MUAC';

    // Select tab.
    browser.click('a=' + tab);
    browser.waitForVisible('h3=Mid Upper Arm Circumference (MUAC):');

    browser.setValue('input[type="number"]', 50);
    browser.waitForVisible('.label-up .icon-up');
    const result = browser.getText('.label-up');
    assert.equal(result, 'Gained\n37 cm', 'Indication for the gained MUAC is incorrect.');
  })

  it('should display an indication when MUAC is lost', () => {
    browser.setValue('input[type="number"]', 5);
    browser.waitForVisible('.label-down .icon-down');
    const result = browser.getText('.label-down');
    assert.equal(result, 'Lost\n8 cm', 'Indication for the lost MUAC is incorrect.');
  })

  after(() => browser.logout());

});
