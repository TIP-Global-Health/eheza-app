'use strict';
const assert = require('assert');

module.exports = function (browser, capabilities, specs) {

  browser.addCommand('login', (user) => {
    assert(user, "login command must be passed a username")
    browser.url('/#login');
    browser.waitForVisible('.login-form');
    browser.setValueSafe('[name="username"]', user);
    browser.setValueSafe('[name="password"]', user);
    browser.submitForm('.login-form');
    browser.waitForVisible('a=Sign Out');
  });

  browser.addCommand('logout', () => {
    browser.url('/#');
    browser.findAndClick('a=Sign Out')
    browser.waitForVisible('.login-form');
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

  // Set the window size to avoid clipping things off.
  browser.windowHandleSize({
    width: 1500,
    height: 900
  });

}

