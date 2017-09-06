var assert = require('assert');

describe('assesment pages', () => {

    before(() => {
        browser.loginAndViewParticipantsPage('aya');
    });

    beforeEach(() => {
        browser.visitChildWithTodoTasks();
    });

    it('should allow a user to switch between forms', () => {
        const firstUsedTab = 'Weight';
        const secondUsedTab = 'Height';

        // Switching to Weight.
        browser.click('a=' + firstUsedTab);
        browser.waitForVisible('h3=' + firstUsedTab + ':');
        browser.waitForVisible('h3=' + secondUsedTab + ':', 500, true);

        // Switching to Height.
        browser.click('a=' + secondUsedTab);
        browser.waitForVisible('h3=' + firstUsedTab + ':', 500, true);
        browser.waitForVisible('h3=' + secondUsedTab + ':');
    });

    it('should preserve Nutrition Signs data while the user is switching between forms', () => {
        const firstUsedTab = 'Nutrition';
        const secondUsedTab = 'Photo';

        // Switching to Nutrition Signs, fill data.
        browser.click('a=' + firstUsedTab);
        browser.waitForVisible('h3=' + secondUsedTab + ':', 500, true);
        assert(!browser.isSelected('#apathy'), 'Apathy is not selected');
        assert(!browser.isSelected('#brittle-hair'), 'Brittle Hair is not selected');
        assert(!browser.isSelected('#edema'), 'Edema is not selected');

        browser.click('#apathy');
        browser.click('#brittle-hair');

        // Click save button.
        browser.click('div.nutrition button');

        // Switching to Photo.
        browser.click('a=' + secondUsedTab);
        browser.waitForVisible('h3=' + firstUsedTab + ':', 500, true);
        browser.waitForVisible('h3=' + secondUsedTab + ':');

        // Switching back to Nutrition Signs, check existence of the previous selection.
        browser.click('#completed-tab');
        browser.click('a=' + firstUsedTab);
        assert(browser.isSelected('#apathy'), 'Apathy is still selected');
        assert(browser.isSelected('#brittle-hair'), 'Brittle Hair is still selected');
        assert(!browser.isSelected('#edema'), 'Edema is not selected');
    });
});
