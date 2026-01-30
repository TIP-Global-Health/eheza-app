module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Docs.NoMissing exposing (exposedModules, onlyExposed)
import Docs.ReviewAtDocs
import Docs.ReviewLinksAndSections
import Docs.UpToDateReadmeLinks
import NoConfusingPrefixOperator
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeExpose
import NoPrematureLetComputation
import NoSimpleLetBody
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)
import Simplify


config : List Rule
config =
    List.map
        (Rule.ignoreErrorsForDirectories ignoredDirectories
            >> Rule.ignoreErrorsForFiles ignoredFiles
        )
        rules


rules : List Rule
rules =
    [ Docs.UpToDateReadmeLinks.rule
    , Docs.ReviewAtDocs.rule
    , Docs.UpToDateReadmeLinks.rule
    , Docs.NoMissing.rule { document = onlyExposed, from = exposedModules }
    , NoConfusingPrefixOperator.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
        |> Rule.ignoreErrorsForFiles
            [ "src/Pages/Completion/View.elm"
            , "src/Pages/CompletionMenu/View.elm"
            , "src/Pages/Reports/View.elm"
            , "src/Pages/ReportsMenu/View.elm"
            , "src/Pages/Scoreboard/View.elm"
            , "src/Pages/ScoreboardMenu/View.elm"
            ]
    , NoExposingEverything.rule
    , NoImportingEverything.rule
        [ "Backend.Entities"
        , "Html"
        , "Html.Attributes"
        , "Html.Events"
        , "Svg"
        , "Svg.Attributes"
        , "Backend.Completion.Model"
        , "Backend.ScoreboardMenu.Model"
        , "Utils.GeoLocation"
        ]
    , NoMissingTypeAnnotation.rule
        |> Rule.ignoreErrorsForFiles
            [ "src/Pages/Completion/View.elm"
            , "src/Backend/Utils.elm"
            ]
    , NoMissingTypeExpose.rule
    , NoSimpleLetBody.rule
    , NoPrematureLetComputation.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , Simplify.rule Simplify.defaults
    ]


ignoredDirectories : List String
ignoredDirectories =
    [ "src/Gizra"
    , "src/Utils"
    , "src/Error"
    ]


ignoredFiles : List String
ignoredFiles =
    [ "src/Icons.elm"
    ]
