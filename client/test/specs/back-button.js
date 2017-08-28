var assert = require('assert');

describe('Clicking the Back Button', () => {

    before(() => {
        browser.loginAndViewParticipantsPage('aya');
    });

    it.skip('should lead to the activities page from the participants page', () => {
        browser.validateCurrentPageIs('participants');
        browser.goBack();
        browser.validateCurrentPageIs('activities');
    });

    it.skip('should lead to the participants page from the activities page', () => {
        browser.validateCurrentPageIs('activities');
        browser.goBack();
        browser.validateCurrentPageIs('participants');
    });

    it.skip('should lead to the participants page from a child page', () => {
        // Following the first participant (child) page.
        browser.element('#participants-table tbody tr td a.child').click();
        browser.validateCurrentPageIs('child');
        browser.goBack();
        browser.validateCurrentPageIs('participants');
    });

    it.skip('should lead to the participants page from a mother page', () => {
        // Following the first participant (mother) page.
        browser.element('#participants-table tbody tr td a.mother').click();
        browser.validateCurrentPageIs('mother');
        browser.goBack();
        browser.validateCurrentPageIs('participants');
    });

});
