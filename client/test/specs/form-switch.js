var assert = require('assert');

describe('assesment pages', function() {
    it('should allow a user to switch between forms', function() {
        const firstUsedTab = 'Weight';
        const secondUsedTab = 'Height';

        browser.login('aya');
        browser.url('/#patient/55');
        browser.waitForVisible('.header.activities');

        // Switching to Weight.
        browser.click('a=' + firstUsedTab);
        browser.waitForVisible('span=' + firstUsedTab + ':');
        browser.waitForVisible('span=' + secondUsedTab + ':', 500, true);

        // Switching to Height.
        browser.click('a=' + secondUsedTab);
        browser.waitForVisible('span=' + firstUsedTab + ':', 500, true);
        browser.waitForVisible('span=' + secondUsedTab + ':');
    });
});
