'use strict';
const assert = require('assert');

describe('Image Upload workflow', () => {
  after(() => browser.logout());

  it('should allow to upload an image to the dropzone on the photo form.', () => {

    browser.login('aya');
    browser.waitForVisible('#patients-table');

    // Proceeding to the patient where activities are pending.
    browser.url('/#patient/41');
    browser.waitUntil(() => browser.isVisible('.ui.header.mother'));

    // Follow the photo form.
    browser.element('a=Photo').click();

    // Add and then check the image.
    browser.addTestImage('Testfile1');
    browser.checkImageBasename('.dropzone .dz-complete > div.dz-image:nth-child(1) img', 'Testfile1');

  });

  it('should reset the dropzone control.', () => {

    browser.login('aya');
    browser.waitForVisible('#patients-table');

    // Proceeding to the patient where activities are pending.
    browser.url('/#patient/41');
    browser.waitUntil(() => browser.isVisible('.ui.header.mother'));

    // Follow the photo form.
    browser.element('a=Photo').click();

    // Add and then check the image.
    browser.addTestImage('Testfile1');
    browser.checkImageBasename('.dropzone .dz-complete > div.dz-image:nth-child(1) img', 'Testfile1');

    browser.click('button*=Retake');
    var imageCount = browser.elements('.dropzone .dz-complete > div.dz-image').length();

    Console.log('imageCount', imageCount);

    // assert.equal(0, imageCount);

  });

});
