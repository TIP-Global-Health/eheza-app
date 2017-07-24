'use strict';
const assert = require('assert');

describe('The Photo form', () => {
  before(() => {
    browser.login('aya');
    browser.waitForVisible('#patients-table');

    browser.visitChildWithTodoTasks();

    // Follow the photo form.
    browser.element('a=Photo').click();
  });

  after(() => browser.logout());

  it('should not allow to Save without an image', () => {
    // The Save button is enabled.
    const classesBefore = browser.getAttribute('#save-form', 'class');
    assert.notEqual(classesBefore.indexOf('disabled'), -1, 'The Save button is disabled');

  });

  it('should allow to upload an image to the dropzone', () => {
    // Add and then check the image.
    browser.addTestImage('Testfile1');
    browser.checkImageBasename('.dropzone .dz-complete > div.dz-image:nth-child(1) img', 'Testfile1');
  });

});
