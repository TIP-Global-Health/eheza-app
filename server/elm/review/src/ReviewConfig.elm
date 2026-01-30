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
        |> Rule.ignoreErrorsForFiles [ "src/Pages/Completion/View.elm", "src/Backend/Utils.elm" ]
    , NoMissingTypeExpose.rule
    , NoSimpleLetBody.rule
    , NoPrematureLetComputation.rule

    -- , NoUnused.CustomTypeConstructors.rule []
    --     |> Rule.ignoreErrorsForFiles
    --         [ "src/elm/ZScore/Model.elm"
    --         , "src/elm/SyncManager/Model.elm"
    --         , "src/elm/Pages/WellChild/ProgressReport/Model.elm"
    --         , "src/elm/Pages/WellChild/ProgressReport/Model.elm"
    --         , "src/elm/Pages/Prenatal/Activity/Model.elm"
    --         , "src/elm/Pages/GlobalCaseManagement/Model.elm"
    --         , "src/elm/Pages/ChildScoreboard/Encounter/Model.elm"
    --         , "src/elm/Pages/AcuteIllness/Encounter/Model.elm"
    --         , "src/elm/Measurement/Model.elm"
    --         , "src/elm/App/Model.elm"
    --         , "src/elm/Activity/Model.elm"
    --         ]
    -- , NoUnused.CustomTypeConstructorArgs.rule
    --     |> Rule.ignoreErrorsForFiles
    --         [ "src/elm/Backend/Model.elm"
    --         , "src/elm/SyncManager/Model.elm"
    --         ]
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule

    -- , Simplify.rule Simplify.defaults
    --     |> Rule.ignoreErrorsForFiles
    --         [ "src/elm/GeoLocation/Utils.elm"
    --         , "src/elm/Measurement/Utils.elm"
    --         ]
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
