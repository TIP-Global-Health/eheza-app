module Pages.Components.Utils exposing
    ( isSyncComplete
    , populationSelectionOptionFromString
    , populationSelectionOptionToString
    , viewSyncingPlaceholder
    )

import App.Types exposing (Language)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Pages.Components.Types exposing (PopulationSelectionOption(..))
import Translate exposing (translate)


populationSelectionOptionToString : PopulationSelectionOption -> String
populationSelectionOptionToString selectionOption =
    case selectionOption of
        SelectionOptionGlobal ->
            "all"

        SelectionOptionDemographics ->
            "demographics"

        SelectionOptionHealthCenter ->
            "hc"


populationSelectionOptionFromString : String -> Maybe PopulationSelectionOption
populationSelectionOptionFromString selectionOption =
    case selectionOption of
        "all" ->
            Just SelectionOptionGlobal

        "demographics" ->
            Just SelectionOptionDemographics

        "hc" ->
            Just SelectionOptionHealthCenter

        _ ->
            Nothing


{-| Returns the (status, progress) pair shown in the top-right
download-status badge.

Takes the running count of records the page has accumulated so far
(Reports / Scoreboard derive that from `List.length data.records`;
Completion sums the ten encounter-list lengths) and the current
`remainingForDownload`.

-}
syncStatusAndProgress : Int -> Maybe Int -> ( String, String )
syncStatusAndProgress downloaded =
    Maybe.map
        (\remainingForDownload ->
            ( if remainingForDownload == 0 then
                "COMPLETED"

              else
                "IN PROCESS"
            , String.fromInt downloaded ++ " / " ++ String.fromInt (downloaded + remainingForDownload)
            )
        )
        >> Maybe.withDefault ( "PENDING", "0 / 0" )


{-| True iff the page's paginated sync has finished downloading every record.

The sync loop in `Backend.Components.Sync` populates `remainingForDownload`:

  - `Nothing` -- initial state, no batch has come back yet (PENDING).
  - `Just n` where `n > 0` -- batches in flight (IN PROCESS).
  - `Just 0` -- final batch returned, every record is local (COMPLETED).

Pages use this predicate to gate input controls so the user can't pick a
report against a half-loaded dataset.

-}
isSyncComplete : Maybe Int -> Bool
isSyncComplete remaining =
    remaining == Just 0


{-| Centered placeholder shown in place of a page's input + content area
while sync is in progress.

Renders the same status / progress strings that the top-right badge
would (so the user has a single, prominent indicator while syncing) plus
a translated explanatory line.

-}
viewSyncingPlaceholder : Language -> Int -> Maybe Int -> Html msg
viewSyncingPlaceholder language downloaded maybeRemaining =
    let
        ( syncStatus, progress ) =
            syncStatusAndProgress downloaded maybeRemaining
    in
    div [ class "sync-placeholder" ]
        [ div [ class "sync-status" ] [ text syncStatus ]
        , div [ class "explanation" ] [ text <| translate language Translate.DownloadingExplanation ]
        , div [ class "progress" ] [ text progress ]
        ]
