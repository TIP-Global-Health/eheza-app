var assert = require('assert');

describe('Clicking the Back Button.', () => {

    before(() => {
        browser.loginAndViewPatientsPage('aya');
    });

    it('should lead to the activities page from the patients page.', () => {
        browser.validateCurrentPageIs('patients');
        browser.goBack();
        browser.validateCurrentPageIs('activities');
    });

    it('should lead to the patients page from the activities page.', () => {
        browser.validateCurrentPageIs('activities');
        browser.goBack();
        browser.validateCurrentPageIs('patients');
    });

    it('should lead to the patients page from a child page.', () => {
        // Following the first patient (child) page.
        browser.element('#patients-table tbody tr td a.child').click();
        browser.validateCurrentPageIs('child');
        browser.goBack();
        browser.validateCurrentPageIs('patients');
    });

    it('should lead to the patients page from a mother page.', () => {
        // Following the first patient (mother) page.
        browser.element('#patients-table tbody tr td a.mother').click();
        browser.validateCurrentPageIs('mother');
        browser.goBack();
        browser.validateCurrentPageIs('patients');
    });

});
