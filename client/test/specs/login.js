var assert = require('assert');

describe('login page', function() {

    it('should not allow an anonymous user with wrong credentials to login', function() {
        const loginForm = '.login .form';
        browser.url('/#login');

        browser.waitForVisible(loginForm);
        browser.setValueSafe('[name="username"]', 'wrong-name');
        browser.setValueSafe('[name="password"]', 'wrong-pass');
        browser.submitForm(loginForm);

        browser.waitForVisible('.login .ui.error.message');
    });

    it('should allow a user to login', function() {
        const loginForm = '.login .form';
        browser.url('/#login');

        browser.waitForVisible(loginForm);
        browser.setValueSafe('[name="username"]', 'fabrice');
        browser.setValueSafe('[name="password"]', 'fabrice');
        browser.submitForm(loginForm);
        browser.waitForVisible('h1=Clinics');
    });

});
