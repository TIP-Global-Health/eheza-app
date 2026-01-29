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
import NoMissingTypeAnnotationInLetIn
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
        |> Rule.ignoreErrorsForFiles
            [ "src/elm/Pages/Prenatal/ProgressReport/View.elm"
            , "src/elm/Pages/Prenatal/Activity/View.elm"
            , "src/elm/Pages/Prenatal/Activity/Utils.elm"
            , "src/elm/Measurement/View.elm"
            ]
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
        |> Rule.ignoreErrorsForFiles
            [ "src/elm/ZScore/Test.elm"
            , "src/elm/SyncManager/View.elm"
            , "src/elm/Pages/Dashboard/GraphUtils.elm"
            , "src/elm/Pages/Dashboard/View.elm"
            , "src/elm/Pages/Prenatal/Activity/View.elm"
            ]
    , NoExposingEverything.rule

    -- , NoImportingEverything.rule []
    -- , NoMissingTypeAnnotation.rule
    -- , NoMissingTypeAnnotationInLetIn.rule
    -- , NoMissingTypeExpose.rule
    -- , NoSimpleLetBody.rule
    -- , NoPrematureLetComputation.rule
    -- , NoUnused.CustomTypeConstructors.rule []
    -- , NoUnused.CustomTypeConstructorArgs.rule
    -- , NoUnused.Dependencies.rule
    -- , NoUnused.Exports.rule
    -- , NoUnused.Parameters.rule
    -- , NoUnused.Patterns.rule
    -- , NoUnused.Variables.rule
    -- , Simplify.rule Simplify.defaults
    ]


ignoredDirectories : List String
ignoredDirectories =
    [ "src/generated"
    , "src/elm/Error"
    , "src/elm/Gizra"
    , "src/elm/Restful"
    , "src/elm/Utils"
    ]


ignoredFiles : List String
ignoredFiles =
    [ "src/elm/AssocList.elm"
    ]
