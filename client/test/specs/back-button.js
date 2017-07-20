var assert = require('assert');

describe('Clicking the Back Button.', () => {

    before(() => {
        browser.loginAndViewPatientsPage('aya');
    });

    it('Clicking the back button from the patients page should lead to the activities page.', () => {
        browser.validateCurrentPageIs('patients');
        browser.goBack();
        browser.validateCurrentPageIs('activities');
    });

    it('Clicking the back button from the activities page should lead to the patients page.', () => {
        browser.validateCurrentPageIs('activities');
        browser.goBack();
        browser.validateCurrentPageIs('patients');
    });

    it('Clicking the back button from a child page should lead to the patients page.', () => {
        // Following the first patient (child) page.
        browser.element('#patients-table tbody tr td a.child').click();
        browser.validateCurrentPageIs('child');
        browser.goBack();
        browser.validateCurrentPageIs('patients');
    });

    it('Clicking the back button from a mother page should lead to the patients page.', () => {
        // Following the first patient (mother) page.
        browser.element('#patients-table tbody tr td a.mother').click();
        browser.validateCurrentPageIs('mother');
        browser.goBack();
        browser.validateCurrentPageIs('patients');
    });

});
