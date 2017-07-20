var assert = require('assert');

describe('Auto transform between measurement forms.', () => {

    before(() => {
        browser.loginAndViewPatientsPage('aya');

        // Following the first patient (child) page.
        browser.element('#patients-table tbody tr td a.child').click();
        browser.waitForVisible('#mother-info');

        // In case the Photo is already completed we should switch to the
        // Completed tab.
        if (!browser.isVisible('a=Photo') && browser.isVisible('#pending-tab.active')) {
            browser.element('#completed-tab').click();
        }
        // Initially follow the Photo form.
        browser.element('a=Photo').click();
    });

    it('Saving the Photo form should lead to the Weight form.', () => {
        browser.addTestImage('Testfile1');
        browser.element('#save-form').click();
        // The help text of the Weight form.
        browser.waitForVisible("p=Calibrate the scale before taking the first baby's weight. Place baby in harness with no clothes on.");
    });

    it('Saving the Weight form should lead to the Height form.', () => {
        browser.setValue('.weight .form input', 4 + Math.random());
        browser.element('#save-form').click();
        // The help text of the Height form.
        browser.waitForVisible("p=Ask the mother to hold the baby’s head at the end of the measuring board. Move the slider to the baby’s heel and pull their leg straight.");
    });

    it('Saving the Height form should lead to the MUAC form.', () => {
        browser.setValue('.height .form input', 56 + Math.random());
        browser.element('#save-form').click();
        // The help text of the MUAC form.
        browser.waitForVisible("p=Make sure to measure at the center of the baby’s upper arm.");
    });

    it('Saving the MUAC form should lead to the Nutrition Signs form.', () => {
        browser.setValue('.muac .form input', 4 + Math.random());
        browser.element('#save-form').click();
        // The help text of the Nutrition Signs form.
        browser.waitForVisible("p=Explain to the mother how to check the malnutrition signs for their own child.");
    });

});
