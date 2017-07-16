var assert = require('assert');

describe('login page', function() {
    it('should allow a user to login', function() {
        const loginForm = '.login-container .form';
        browser.url('/#login');

        browser.waitForVisible(loginForm);
        browser.setValueSafe('[name="username"]', 'admin');
        browser.setValueSafe('[name="password"]', 'admin');
        browser.submitForm(loginForm);
        browser.waitForVisible('h1=Patients');

        // Logout session.
        browser.click('#sign-out');
        browser.waitForVisible('[name="username"]');
    });

    it('should not allow an anonymous user with wrong credentials to login', function() {
        const loginForm = '.login-container .form';
        browser.url('/#login');

        browser.waitForVisible(loginForm);
        browser.setValueSafe('[name="username"]', 'wrong-name');
        browser.setValueSafe('[name="password"]', 'wrong-pass');
        browser.submitForm(loginForm);

        browser.waitForVisible('.login-container .ui.error.message');
    });
});
