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
      const elmApps = settings.elm_apps;
      // Iterate over the apps.
      Object.keys(elmApps).forEach(function (appName) {
        // appName with unique css ID, e.g. `elm-app--2`.
        const node = document.getElementById(appName);
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
          themePath: appSettings.theme_path,
        }});

        if (appSettings.page === 'reports-results') {
          app.ports.downloadCsv.subscribe(function (data) {
            const filename = data[0];
            const content = data[1];
            const element = document.createElement('a');
            element.setAttribute('href', 'data:text/csv;charset=utf-8,' + encodeURIComponent(content));
            element.setAttribute('download', filename);
            element.style.display = 'none';
            document.body.appendChild(element);
            element.click();
            document.body.removeChild(element);
          });
        }
      });
    }
  };

})(jQuery, Drupal);
