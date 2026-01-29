# Elm Review Configuration

This directory contains the configuration for [elm-review](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/), a tool for static code analysis of Elm code.

## What is elm-review?

elm-review is a tool that analyzes your Elm code to find patterns that may indicate bugs, code style issues, or opportunities for simplification. It can also automatically fix some of these issues.

## Current Rules

The configuration includes the following rule packages:

- **elm-review** (2.15.0): Core review framework
- **elm-review-code-style** (1.1.5): Code style rules like `NoConfusingPrefixOperator`, `NoSimpleLetBody`, `NoPrematureLetComputation`
- **elm-review-common** (1.3.3): Common rules like `NoExposingEverything`, `NoImportingEverything`
- **elm-review-debug** (1.0.9): Rules to catch debug code like `NoDebug.Log`
- **elm-review-documentation** (2.0.4): Documentation quality rules like `Docs.ReviewAtDocs`
- **elm-review-simplify** (2.1.5): Rules to simplify code with the `Simplify` rule
- **elm-review-unused** (1.2.2): Rules to detect unused code like `NoUnused.Variables`, `NoUnused.Exports`, etc.

## Running elm-review

### Locally

From the `client` directory:

```bash
npx elm-review
```

To automatically fix issues:

```bash
npx elm-review --fix
```

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

### Recommended Additional Packages

Consider adding these packages for additional checks:

- **jfmengels/elm-review-performance**: Performance optimization rules
- **SiriusStarr/elm-review-no-single-pattern-case**: Simplify single-pattern case expressions
- **truqu/elm-review-nolazy**: Check for unnecessary lazy evaluation
- **jfmengels/elm-review-the-elm-architecture**: Rules for TEA patterns

## Ignored Directories and Files

The following directories are ignored by elm-review:
- `src/generated`: Generated code
- `src/elm/Error`: Error handling code
- `src/elm/Gizra`: Third-party Gizra code
- `src/elm/Restful`: RESTful utilities
- `src/elm/Utils`: Utility functions

The following files are ignored:
- `src/elm/AssocList.elm`: Third-party implementation

These can be modified in the `ignoredDirectories` and `ignoredFiles` lists in `src/ReviewConfig.elm`.

## Learn More

- [elm-review documentation](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/)
- [Available review packages](https://package.elm-lang.org/packages/search?q=elm-review)
- [Creating custom rules](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule)
