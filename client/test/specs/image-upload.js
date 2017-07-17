'use strict';
const assert = require('assert');

describe('Image Upload workflow', () => {
  after(() => browser.logout());

  it('should allow to upload an image to the dropzone on the photo form.', () => {

    browser.login('aya');
    browser.waitForVisible('#patients-table');

    // Following the first patient page.
    browser.element('#patients-table tbody tr td a').click();
    browser.waitUntil(() => browser.isVisible('#mother-info'));

    // Follow the photo form.
    browser.element('a=Take pictures (Child)').click();

    // Add and then check the image.
    browser.addTestImage('Testfile1');
    browser.checkImageBasename('.dropzone .dz-complete > div.dz-image:nth-child(1) img', 'Testfile1');

  });

});
