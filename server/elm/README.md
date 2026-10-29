# E-Heza backend Elm app

The Elm application behind the admin report pages under `admin/reports`. Drupal
mounts it through `hedley_general_build_elm_app()` and tells it which page to
render.

Both build outputs are committed, so rebuild them after changing the sources.

## JavaScript bundle

```bash
elm make src/Main.elm --output ../hedley/modules/custom/hedley_general/js/elm-main.js
```

The build is not optimized, and CI compares the rebuilt bundle to the committed
one byte for byte.

## Dashboard CSS

The HealthyStart dashboards are styled with Tailwind. It reads
`css/dashboards.css`, scans the dashboard sources for the utilities they name,
and writes the stylesheet Drupal serves:

```bash
npm install   # once, on the host: the Tailwind CLI needs Node 20 or newer
npm run css   # or `npm run css:watch` while working on a dashboard
```
