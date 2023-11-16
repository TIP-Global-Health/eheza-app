/**
 * @file
 *  Elm applications.
 */

(function ($, Drupal) {

  /**
   * Adds Elm application.
   */
  Drupal.behaviors.ElmApps = {
    attach: function (context, settings) {
      var elmApps = settings.elm_apps;
      // Iterate over the apps.
      Object.keys(elmApps).forEach(function (appName) {
        // appName with unique css ID, e.g. `elm-app--2`.
        var node = document.getElementById(appName);
        if (!node) {
          // We haven't found the div, so prevent an error.
          return;
        }
        // The current app's settings.
        const appSettings = elmApps[appName];

        // Initiate ELM application.
        const app = Elm.Main.init({node: node, flags: {
          appData: appSettings.data,
          page: appSettings.page,
        }});

      });
    }
  };

})(jQuery, Drupal);
