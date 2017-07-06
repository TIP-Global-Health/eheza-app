var assert = require('assert');
var wdioConf = require('../../wdio.conf.js');

describe('taxonomies', function() {

  before(function() {
    browser.drupalLogin('john');
  });

  it('Testing the vocabulary created', function() {

    browser.url(wdioConf.config.drupalUrl + "/admin/structure/taxonomy/nutrition_health_signs");

    if (browser.isExisting("//table[@id='taxonomy']//a[.='Add term']")) {
      throw 'There are no terms';
    }
  });

});
