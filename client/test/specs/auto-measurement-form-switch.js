var assert = require('assert');

describe('Auto transform between measurement forms.', () => {

    before(() => {
        browser.loginAndViewPatientsPage('aya');

        // Following the first patient (child) page.
        browser.element('#patients-table tbody tr td a.child').click();
        browser.waitForVisible('#mother-info');

        // In case the Weight is already completed we should switch to the
        // Completed tab.
        if (!browser.isVisible('a=Weight') && browser.isVisible('#pending-tab.active')) {
            browser.element('#completed-tab').click();
        }
        // Initially follow the Photo form.
        browser.element('a=Weight').click();
    });

    it('Saving the Weight form should lead to the Height form.', () => {
        browser.element('#save-form').click();
        // The help text of the Height form.
        browser.waitForVisible("p=Ask the mother to hold the baby’s head at the end of the measuring board. Move the slider to the baby’s heel and pull their leg straight.");
    });

    it('Saving the Height form should lead to the MUAC form.', () => {
        browser.element('#save-form').click();
        // The help text of the MUAC form.
        browser.waitForVisible("p=Make sure to measure at the center of the baby’s upper arm.");
    });

    it('Saving the MUAC form should lead to the Nutrition Signs form.', () => {
        browser.element('#save-form').click();
        // The help text of the Nutrition Signs form.
        browser.waitForVisible("p=Explain to the mother how to check the malnutrition signs for their own child.");
    });

});
