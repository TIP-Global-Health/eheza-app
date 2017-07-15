var assert = require('assert');

describe('assesment pages', () => {
    it('should allow a user to switch between forms', () => {
        const firstUsedTab = 'Weight';
        const secondUsedTab = 'Height';

        browser.login('aya');
        // We generate 20 of every content-types, and we generated Children
        // in the third step, that's how we picked Child 55.
        // @see server/scripts/helper-functions.sh
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
