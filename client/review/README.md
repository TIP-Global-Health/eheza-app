# Elm Review Configuration

This directory contains the configuration for [elm-review](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/), a tool for static code analysis of Elm code.

## What is elm-review?

elm-review is a tool that analyzes your Elm code to find patterns that may indicate bugs, code style issues, or opportunities for simplification. It can also automatically fix some of these issues.

## Running elm-review

### Locally

From the `client` directory:

```bash
npx elm-review
```

To automatically fix issues (requires confirmation for each fix):

```bash
npx elm-review --fix
```

To automatically fix all issues (requires single confirmation for all fixes):

```bash
npx elm-review --fix-all
````

Note: before running this, make sure to comment out rules in `ReviewConfig.elm`
that appear under `Rules to comment out when working with --fix-all`.


### In CI

elm-review runs automatically in the CI pipeline as part of the `lint_elm_review` job. See `.circleci/config.yml` for details.

## Adding New Rules

To add new review rules:

1. Navigate to the review directory:
   ```bash
   cd client/review
   ```

2. Install the new package:
   ```bash
   npx elm-json install author/packagename
   ```

3. Add the rule to `src/ReviewConfig.elm`:
   ```elm
   import NewRule
   
   rules : List Rule
   rules =
       [ -- existing rules
       , NewRule.rule
       ]
   ```

## Learn More

- [elm-review documentation](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/)
- [Available review packages](https://package.elm-lang.org/packages/search?q=elm-review)
- [Creating custom rules](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule)
