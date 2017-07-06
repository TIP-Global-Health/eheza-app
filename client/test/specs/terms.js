var assert = require('assert');
var wdioConf = require('../../wdio.conf.js');

describe('taxonomies', function() {

  before(function() {
    browser.drupalLogin('john');
  });

  it('Testing the vocabulary created', function() {

    browser.url(wdioConf.config.drupalUrl + "/admin/structure/taxonomy/nutrition_health_signs");

    browser.waitForVisible('#taxonomy');
    if (browser.isExisting('#taxonomy a')) {
      throw 'There are no terms';
    }
  });

});
