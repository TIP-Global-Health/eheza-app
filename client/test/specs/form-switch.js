var assert = require('assert');

describe('assesment pages', () => {
    it('should allow a user to switch between forms', () => {
        const firstUsedTab = 'Weight';
        const secondUsedTab = 'Height';

        browser.login('aya');
        browser.visitTodoChild();

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
