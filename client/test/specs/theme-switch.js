var assert = require('assert');

describe('Theme switcher', () => {
    // The background color of the 'body' element by the dark theme.
    const darkBackgroundColor = '#181d23';
    // The background color of the 'body' element by the light theme.
    const lightBackgroundColor = '#f2f2f2';

    const themeSwitcherSelector = '#theme-switcher';

    var body, bodyBackgroundColor, switcher;

    before(() => {
        browser.login('aya');
        body = browser.element('body');

        browser.waitForVisible(themeSwitcherSelector);
        switcher = browser.element(themeSwitcherSelector);
    });

    it('initial theme should be the \'light\' theme.', () => {
        bodyBackgroundColor = body.getCssProperty('background');
        assert.equal(lightBackgroundColor, bodyBackgroundColor.parsed.hex);
    });

    it('should allow a user to switch to the \'dark\' theme.', () => {
        switcher.click();
        bodyBackgroundColor = body.getCssProperty('background');
        assert.equal(darkBackgroundColor, bodyBackgroundColor.parsed.hex);
    });

    it('should allow a user to switch back to the \'light\' theme.', () => {
        switcher.click();
        bodyBackgroundColor = body.getCssProperty('background');
        assert.equal(lightBackgroundColor, bodyBackgroundColor.parsed.hex);
    });

});
