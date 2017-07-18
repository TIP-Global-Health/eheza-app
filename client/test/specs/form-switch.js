var assert = require('assert');

describe('assesment pages', () => {
    it('should allow a user to switch between forms', () => {
        const firstUsedTab = 'Weight';
        const secondUsedTab = 'Height';

        browser.login('aya');
        // We generate 20 of every content-types, and we generated Children
        // in the third step, that's how we picked Child 41.
        // The first Child is also special, it has most of the dates
        // in the past.
        // @see server/scripts/helper-functions.sh
        browser.url('/#patient/41');
        browser.waitForVisible('.ui.tasks.segment');

        // Switching to Weight.
        browser.click('a=' + firstUsedTab);
        browser.waitForVisible('div=' + firstUsedTab + ':');
        browser.waitForVisible('div=' + secondUsedTab + ':', 500, true);

        // Switching to Height.
        browser.click('a=' + secondUsedTab);
        browser.waitForVisible('div=' + firstUsedTab + ':', 500, true);
        browser.waitForVisible('div=' + secondUsedTab + ':');
    });
});
