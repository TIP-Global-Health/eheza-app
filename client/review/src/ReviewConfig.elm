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
import NoLeftPizza
import NoMissingSubscriptionsCall
import NoMissingTypeAnnotation
import NoMissingTypeExpose
import NoPrematureLetComputation
import NoRedundantlyQualifiedType
import NoSimpleLetBody
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import NoUselessSubscriptions
import Review.Rule as Rule exposing (Rule)
import Simplify
import Validate.Regexes


config : List Rule
config =
    List.map
        (Rule.ignoreErrorsForDirectories ignoredDirectories
            >> Rule.ignoreErrorsForFiles ignoredFiles
        )
        rules


rules : List Rule
rules =
    [ NoRedundantlyQualifiedType.rule
    , NoSimpleLetBody.rule
    , Docs.UpToDateReadmeLinks.rule
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
    , NoMissingTypeAnnotation.rule
        |> Rule.ignoreErrorsForFiles
            [ "src/elm/SyncManager/Decoder.elm"
            , "src/elm/Pages/WellChild/ProgressReport/View.elm"
            , "src/elm/Pages/Report/Svg.elm"
            , "src/elm/Measurement/Utils.elm"
            , "src/elm/Main.elm"
            , "src/elm/Pages/Prenatal/Utils.elm"
            , "src/elm/Backend/Utils.elm"
            ]
    , NoMissingTypeExpose.rule
    , NoSimpleLetBody.rule
    , NoPrematureLetComputation.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
        |> Rule.ignoreErrorsForFiles
            [ "src/elm/LocalConfig.Example.elm"
            , "src/elm/Config.Deploy.elm"
            , "src/elm/App/Model.elm"
            ]
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
        |> Rule.ignoreErrorsForFiles
            [ "src/elm/Backend/Measurement/Model.elm"
            , "src/elm/Backend/NutritionEncounter/Utils.elm"
            , "src/elm/Backend/NutritionEncounter/Utils.elm"
            , "src/elm/App/Model.elm"
            ]
    , Simplify.rule Simplify.defaults
        |> Rule.ignoreErrorsForFiles
            [ "src/elm/GeoLocation/Utils.elm"
            , "src/elm/Measurement/Utils.elm"
            ]
    , NoMissingSubscriptionsCall.rule
    , NoUselessSubscriptions.rule
    , NoLeftPizza.rule NoLeftPizza.Redundant
    , Validate.Regexes.rule

    -- Rules to comment out when working with --fix-all.
    , NoImportingEverything.rule
        [ "Backend.Entities"
        , "Backend.Measurement.Model"
        , "Html"
        , "Html.Attributes"
        , "Html.Events"
        , "Svg"
        , "Svg.Attributes"
        , "SyncManager.Model"
        ]
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
