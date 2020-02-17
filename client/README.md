## Prerequisites

Make sure the following are installed:

* NodeJs (and npm) - known working version: v8.10.0 - `nvm use 8.10.0`
* Elm (e.g. `npm install -g elm@~0.18.0`)
* Compass (for SASS) (`gem update --system && gem install compass`)
* Elm Format (`npm install -g elm-format@0.8.1`), not strictly required for the development, but the standard must be followed, as Travis checks that. Therefore it's highly suggested to run Elm Format upon save at your IDE (https://github.com/avh4/elm-format#editor-integration).

## Installation

* `npm install`
* `bower install`
* `elm-package install -y`
* `cp src/elm/LocalConfig.Example.elm src/elm/LocalConfig.elm`

You may need to update `src/elm/LocalConfig.elm` if your local URLs are different from the default setting.

## Usage

1. Serve locally, and watch file changes: `gulp`
2. Prepare file for publishing (e.g. minify, and rev file names): `gulp publish`
3. Deploy to GitHub's pages (`gh-pages` branch of your repository): `gulp deploy`

## Unit Tests
The unit tests are written in Elm via [Elm Test](https://github.com/elm-community/elm-test) and it's invoked at the Travis builds.

### Local execution

```
npm test
```

Note that the `npm test` automatically does a `gulp zscore` to setup the JSON
data needed to test our Z-Score code.

## WebdriverIO tests

1. Run `gulp`
1. Follow [steps 2-4](http://webdriver.io/guide.html)
1. `cp wdio.local.conf.js.example wdio.local.conf.js` and set your local configurations.
1. Execute tests with `./node_modules/.bin/wdio wdio.local.conf.js`

Beware that you do not need to (should not) execute a standalone Selenium Server alongside WDIO to run the tests.
To simulate Travis test execution, see `../ci-scripts/README.md`

## Z-Scores

Our `gulpfile.js` has a task `gulp zscore` which converts the raw Z-Score tables we
downloaded from the WHO web site into three formats:

- A JSON representation the client can download via an HTTP request (and
  cache).
- A JSON representation that the backend can load (to calculate Z-Scores on the
  backend.
- An Elm module which contains the JSON representation as a string, so we can
  unit-test the Elm code.

This should all happen automatically when you run `gulp`.
