'use strict';
const assert = require('assert');

describe('The Photo form', () => {
  before(() => {
    browser.loginAndViewParticipantsPage('aya');
    browser.visitChildWithTodoTasks();
    // Follow the photo form.
    browser.element('a=Photo').click();
  });

  after(() => browser.logout());

  it('should display the default message of the dropzone', () => {
    browser.waitForVisible('.dz-message');
    const dzDefaultMessage = browser.getText('.dz-message span');
    assert.equal(dzDefaultMessage, 'Touch here to take a photo, or drop a photo file here.', 'The default message of the dropzone is incorrect.');
  });

  it('should not allow to Save without an image', () => {
    // The Save button is disabled.
    const classesBefore = browser.getAttribute('#save-form', 'class');
    assert.notEqual(classesBefore.indexOf('disabled'), -1, 'The Save button is disabled');
  });

  it('should allow to upload an image to the dropzone', () => {
    // Add and then check the image.
    browser.addTestImage('Testfile1');
    browser.checkImageBasename('.dropzone .dz-complete > div.dz-image:nth-child(1) img', 'Testfile1');
  });

  it('should not allow to upload more than one image to the dropzone', () => {
    browser.addTestImage('Testfile2');
    // Validate the error by the class name.
    browser.checkImageBasename('.dropzone .dz-error > div.dz-image > img', 'Testfile2');
  });

});
