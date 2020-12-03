## Prerequisites

Make sure the following are installed:

1. [ddev](https://ddev.readthedocs.io/) should be the only requirement, and 
every operation should happen inside ddev's containers. For example, one should
not ever need to execute commands such as `composer install` from the host
machine. Instead we have `ddev composer install`. The advantage is that we have
a consistent, reproducible and shareable environment, so developers don't have
to lose time over configuration of their host machine.
2. Elm Format (`npm install -g elm-format@0.8.1`), not strictly required for the development, but the standard must be followed, as Travis checks that. Therefore it's highly suggested to run Elm Format upon save at your IDE (https://github.com/avh4/elm-format#editor-integration).

## Installation

* ddev client-install
* `cp src/elm/LocalConfig.Example.elm src/elm/LocalConfig.elm`

You may need to update `src/elm/LocalConfig.elm` if your local URLs are different from the default setting.

## Usage

1. Serve locally, and watch file changes: `ddev gulp`
2. Prepare file for publishing (e.g. minify, and rev file names): `ddev gulp publish`
3. Deploy to GitHub's pages (`gh-pages` branch of your repository): `ddev gulp deploy`

## Z-Scores

Our `gulpfile.js` has a task `ddev gulp zscore` which converts the raw Z-Score tables we
downloaded from the WHO web site into three formats:

- A JSON representation the client can download via an HTTP request (and
  cache).
- A JSON representation that the backend can load (to calculate Z-Scores on the
  backend.
- An Elm module which contains the JSON representation as a string, so we can
  unit-test the Elm code.

This should all happen automatically when you run `ddev gulp`.
