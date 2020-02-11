module Pages.Utils exposing (calculatePercentage, filterDependentNoResultsMessage, matchFilter, matchMotherAndHerChildren, monthList, normalizeFilter, viewNameFilter)

import Backend.Entities exposing (PersonId)
import Backend.Person.Model exposing (Person)
import Backend.Session.Model exposing (OfflineSession)
import Backend.Session.Utils exposing (getChildren)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Month(..))
import Translate exposing (Language, TranslationId, translate)


{-| Calculate percentage.
-}
calculatePercentage : Int -> Int -> Float
calculatePercentage total unique =
    -- Avoid dividing by zero and getting "NaN", just return 0.
    -- Besides, if the total is 0, then we don't need to calculate anything here.
    if total == 0 then
        0

    else
        (toFloat unique / toFloat total) * 100


filterDependentNoResultsMessage : Language -> String -> TranslationId -> String
filterDependentNoResultsMessage language filter message =
    if String.isEmpty filter then
        translate language message

    else
        translate language Translate.NoMatchesFound


matchFilter : String -> String -> Bool
matchFilter filter filteredValue =
    if String.isEmpty filter then
        True

    else
        filteredValue
            |> String.toLower
            |> String.contains filter


monthList : List Month
monthList =
    [ Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ]


matchMotherAndHerChildren : String -> OfflineSession -> PersonId -> Person -> Bool
matchMotherAndHerChildren filter offlineSession motherId mother =
    let
        motherContainsFilter =
            matchFilter filter mother.name

        -- A function, rather than value, to preserve the
        -- short-circuiting benefits of the `||` below.
        childrenContainsFilter _ =
            getChildren motherId offlineSession
                |> List.any
                    (\( _, child ) ->
                        matchFilter filter child.name
                    )
    in
    motherContainsFilter || childrenContainsFilter ()


normalizeFilter : String -> String
normalizeFilter filterInput =
    filterInput
        |> String.toLower
        |> String.trim


viewNameFilter : Language -> String -> (String -> msg) -> Html msg
viewNameFilter language filterInput setFilterMsg =
    div
        [ class "ui action input small" ]
        [ input
            [ placeholder <| translate language Translate.FilterByName
            , type_ "text"
            , onInput setFilterMsg
            , value filterInput
            ]
            []
        , button
            [ classList
                [ ( "ui button primary", True )
                , ( "disabled", String.isEmpty <| normalizeFilter filterInput )
                ]
            , onClick <| setFilterMsg ""
            ]
            [ text <| translate language Translate.ShowAll ]
        ]
