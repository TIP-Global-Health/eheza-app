'use strict';
const assert = require('assert');
const path = require('path');

module.exports = function (browser, capabilities, specs) {

  browser.addCommand('login', (user) => {
    assert(user, "login command must be passed a username")
    browser.url('/#login');
    browser.waitForVisible('.login-form');
    browser.setValueSafe('[name="username"]', user);
    browser.setValueSafe('[name="password"]', user);
    browser.submitForm('.login-form');
    browser.waitForVisible('.link-back');
  });

  browser.addCommand('logout', () => {
    browser.url('/#');
  });

  /**
   * After a user login, the displayed page should contain the participants table.
   */
  browser.addCommand('loginAndViewParticipantsPage', (user) => {
    browser.login(user);
    browser.waitForVisible('.wrap.page-participants');
  });

  /**
   * Click the back button.
   */
  browser.addCommand('goBack', () => {
    browser.click('.link-back');
  });

  /**
   * Add an image to the dropzone.
   *
   * @param {filename} filename The name for the added file.
   */
  browser.addCommand('addTestImage', (filename) => {
      // Based on http://blog.fermium.io/how-to-send-files-to-a-dropzone-js-element-in-selenium/
      browser.execute(
        "var myZone, blob, base64Image; myZone = Dropzone.forElement('div.dropzone');" +
        "base64Image = '" +
        "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" +
        "';" +
        "function base64toBlob(r,e,n){e=e||\"\",n=n||512;for(var t=atob(r),a=[],o=0;o<t.length;o+=n){for(var l=t.slice(o,o+n),h=new Array(l.length),b=0;b<l.length;b++)h[b]=l.charCodeAt(b);var v=new Uint8Array(h);a.push(v)}var c=new Blob(a,{type:e});return c}" +
        "blob = base64toBlob(base64Image, 'image / png');" +
        "blob.name = '" +
        filename +
        ".png';" +
        "myZone.addFile(blob);"
      );

      browser.waitForVisible('.dz-preview.dz-image-preview.dz-complete');
  });

  /**
   * Check name of file starts with a certain string.
   *
   * @param {selector}  to check.
   * @param {filename} filename expected.
   */
  browser.addCommand('checkImageBasename', (selector, filename) => {
    const imgElement = browser.element(selector);
    const imgSrc = imgElement.getAttribute('alt');
    const imgFilename = path.basename(imgSrc, 'png').split('.').shift();

    // Note that if the test run failed on first attempt and is repeated we
    // encounter a renamed file.
    // This test is not idempotent, work around that by removing underscores.
    // This browser command wont work with filenames using underscores
    const filenameWithoutUnderscores = imgFilename.replace(/_./g, '');
    assert.equal(filename, filenameWithoutUnderscores);
  });

  /**
   * Visits a Child with all activities pending.
   */
  browser.addCommand('visitChildWithTodoTasks', () => {
    // We generate 20 of every content-types, and we generated Children
    // in the third step, that's how we picked Child 41.
    // The first Child is also special, it has all the dates
    // in the past for activities.
    // @see server/scripts/helper-functions.sh
    browser.url('/#participant/41');
    browser.waitForVisible('.ui.task.segment');
  });

  /**
   * Recursive function to ensure the correct text.
   *
   * This command is created in order to compensate the setValue() bug.
   * The method (setValue) does not always set the correct value,
   * sometimes it just misses some characters.
   * This function sets each character at a time and recursively validates
   * that the character is actually entered.
   *
   * @param {String} selector
   *   The selector string to grab the element by.
   * @param {String} text
   *   The text that we want to set as a value.
   */
  browser.addCommand('setValueSafe', (selector, text) => {

    // Get the ID of the selected elements WebElement JSON object.
    const elementId = browser.element(selector).value.ELEMENT;

    /**
     * Tackle the even weirder decision of WebDriver.io trim the spaces
     * of every property value. Even the "value" property's value.
     * I understand this for class or href properties but not for value.
     * You can see it here : https://github.com/webdriverio/webdriverio/blob/acdd79bff797b295d2196b3616facc9005b6f17d/lib/webdriverio.js#L463
     *
     * @param {String} elementId
     *   ID of a WebElement JSON object of the current element.
     *
     * @return {String}
     *   The value of the `value` attribute.
     */
    const getActualText = elementId =>
      browser
        .elementIdAttribute(elementId, 'value')
        .value;

    let expected = ''

    // Clear the input before entering new value.
    browser.elementIdClear(elementId);

    while (text) {
      const actual = getActualText(elementId);
      if (actual === expected) {

        const currentChar = text[0];
        expected += currentChar;
        text = text.slice(1);

        // Set next character.
        browser.elementIdValue(elementId, currentChar);

      } else if (expected.indexOf(actual) !== 0) {
        // Actual is not substring of expected, suggests the input has been
        // changed since starting to set value.
        // Start again.

        // Reset text to starting value.
        text = expected + text;

        // Reset progress.
        expected = '';

        // Clear input before entering new value.
        browser.elementIdClear(elementId);
      }
    }
  });

  /**
   * Find and click.
   *
   * @param {String} selector The css selector of the element to click.
   */
  browser.addCommand('findAndClick', (selector) => {
    assert(selector, 'Selector must be provided to findAndClick function');

    let visibleElements = null

    browser.waitUntil(() => {
      // Find all elements that match selector
      const elements = browser.elements(selector);

      // If cannot find elements
      if (!elements.value) {
        return false;
      }

      // Remove non-visible elements.
      visibleElements = elements.value.filter(elem => {
        // Scroll to the element to make sure it's in view
        browser.moveTo(elem.value.ELEMENT);

        const visible = browser.elementIdDisplayed(elem.value.ELEMENT);
        return visible.value;
      })

      return visibleElements.length > 0
    });

    // Only want to click one element.
    const element = visibleElements[0]
    browser.elementIdClick(element.value.ELEMENT);
  });

  /**
   * Verifies if checkbox is clicked or not.
   *
   * @param {String} selector The css selector of the element to click.
   * @param {Bool} state True, if verifying that checkbox should be checked.
   */
  browser.addCommand('verifyCheckboxChecked', (selector, state = true) => {
    const checkboxClass = browser.getAttribute(selector, 'class');

    if (state === true) {
      assert.equal(checkboxClass, "checked", 'Checkbox with selector ' + selector + ' is not checked!');
    }
    else {
      assert.notEqual(checkboxClass, "checked", 'Checkbox with selector ' + selector + ' is checked!');
    }
  });

  // Set the window size to avoid clipping things off.
  browser.windowHandleSize({
    width: 1500,
    height: 900
  });

}
