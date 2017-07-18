var assert = require('assert');

describe('Auto transform between measurement forms.', () => {

    before(() => {
        browser.login('aya');
        browser.waitForVisible('#patients-table');

        // Following the first patient page.
        browser.element('#patients-table tbody tr td a').click();
        browser.waitUntil(() => browser.isVisible('#mother-info'));
    });

    it('Saving the Weight form should lead to the Height form.', () => {
        // Follow the Weight form.
        browser.element('a=Weight').click();
        browser.element('#save-form').click();
        browser.waitForVisible('span=Ask the mother to hold the baby’s head at the end of the measuring board. Move the slider to the baby’s heel and pull their leg straight.');
    });

});
