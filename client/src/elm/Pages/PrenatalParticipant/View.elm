module Pages.PrenatalParticipant.View exposing (view)

import App.Model
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter)
import Backend.PrenatalParticipant.Model exposing (EncounterType(..), PrenatalParticipant)
import EveryDict
import EveryDictList exposing (EveryDictList)
import Gizra.Html exposing (divKeyed, emptyNode, keyed, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate, formatYYYYMMDD)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalParticipant.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> PersonId -> ModelIndexedDb -> Html App.Model.Msg
view language currentDate id db =
    let
        prenatalSessions =
            EveryDict.get id db.prenatalParticipantsByPerson
                |> Maybe.withDefault NotAsked
    in
    div
        [ class "wrap wrap-alt-2 page-prenatal-participant" ]
        [ viewHeader language id
        , div
            [ class "ui full segment" ]
            [ viewWebData language (viewPrenatalActions language currentDate id db) identity prenatalSessions
            ]
        ]


viewHeader : Language -> PersonId -> Html App.Model.Msg
viewHeader language id =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.PrenatalEncounter ]
        , a
            [ class "link-back"
            , onClick <| App.Model.SetActivePage <| UserPage <| EncounterTypesPage id
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewPrenatalActions : Language -> NominalDate -> PersonId -> ModelIndexedDb -> EveryDictList PrenatalParticipantId PrenatalParticipant -> Html App.Model.Msg
viewPrenatalActions language currentDate id db prenatalSessions =
    let
        -- Person prenatal session.
        maybeSessionId =
            prenatalSessions
                |> EveryDictList.toList
                |> List.filter (\( _, session ) -> isNothing session.endDate)
                |> List.head
                |> Maybe.map Tuple.first

        -- Person active prenatal encounter. There should not be more than one.
        maybeEncounterId =
            maybeSessionId
                |> Maybe.andThen
                    (\sessionId ->
                        EveryDict.get sessionId db.prenatalEncountersByParticipant
                            |> Maybe.withDefault NotAsked
                            |> RemoteData.map
                                (EveryDictList.toList
                                    >> List.filter (\( _, encounter ) -> isNothing encounter.endDate)
                                    >> List.head
                                    >> Maybe.map Tuple.first
                                )
                            |> RemoteData.withDefault Nothing
                    )

        -- Wither first prenatal encounter for person is in process.
        -- This is True when there's only one encounter, and it's active.
        firstEncounterInProcess =
            maybeSessionId
                |> Maybe.map
                    (\sessionId ->
                        EveryDict.get sessionId db.prenatalEncountersByParticipant
                            |> Maybe.withDefault NotAsked
                            |> RemoteData.map
                                (EveryDictList.values
                                    >> (\encounters ->
                                            let
                                                activeEncounters =
                                                    encounters
                                                        |> List.filter (.endDate >> isNothing)
                                            in
                                            List.length encounters == 1 && List.length activeEncounters == 1
                                       )
                                )
                            |> RemoteData.withDefault False
                    )
                |> Maybe.withDefault False

        firstVisitAction =
            -- If first encounter is in process, navigate to it.
            if firstEncounterInProcess then
                maybeEncounterId
                    |> unwrap
                        []
                        navigateToEncounterAction

            else
                maybeSessionId
                    |> Maybe.map
                        -- If prenatal session exists, create new encounter for it.
                        (\sessionId ->
                            [ PrenatalEncounter sessionId currentDate Nothing
                                |> Backend.Model.PostPrenatalEncounter
                                |> App.Model.MsgIndexedDb
                                |> onClick
                            ]
                        )
                    -- If prenatal session does not exist, create it.
                    |> Maybe.withDefault
                        [ PrenatalParticipant id AntenatalEncounter currentDate Nothing
                            |> Backend.Model.PostPrenatalSession
                            |> App.Model.MsgIndexedDb
                            |> onClick
                        ]

        subsequentVisitAction =
            maybeEncounterId
                |> unwrap
                    -- When there's no encounter, we'll create new one.
                    (maybeSessionId
                        |> Maybe.map
                            (\sessionId ->
                                [ PrenatalEncounter sessionId currentDate Nothing
                                    |> Backend.Model.PostPrenatalEncounter
                                    |> App.Model.MsgIndexedDb
                                    |> onClick
                                ]
                            )
                        |> Maybe.withDefault []
                    )
                    -- When there's an encounrer, we'll view it.
                    navigateToEncounterAction

        navigateToEncounterAction id =
            [ Pages.Page.PrenatalEncounterPage id
                |> UserPage
                |> App.Model.SetActivePage
                |> onClick
            ]

        navigateToPregnancyOutcomeAction =
            maybeSessionId
                |> Maybe.map
                    (\sessionId ->
                        [ Pages.Page.PregnancyOutcomePage sessionId
                            |> UserPage
                            |> App.Model.SetActivePage
                            |> onClick
                        ]
                    )
                |> Maybe.withDefault []

        firstVisitButtonDisabeld =
            isJust maybeSessionId && not firstEncounterInProcess
    in
    div []
        [ p [ class "label-antenatal-visit" ] [ text <| translate language Translate.SelectAntenatalVisit ]
        , button
            (classList
                [ ( "ui primary button", True )
                , ( "disabled", firstVisitButtonDisabeld )
                ]
                :: firstVisitAction
            )
            [ span [ class "text" ] [ text <| translate language Translate.FirstAntenatalVisit ]
            , span [ class "icon-back" ] []
            ]
        , button
            (classList
                [ ( "ui primary button", True )
                , ( "disabled", not firstVisitButtonDisabeld )
                ]
                :: subsequentVisitAction
            )
            [ span [ class "text" ] [ text <| translate language Translate.SubsequentAntenatalVisit ]
            , span [ class "icon-back" ] []
            ]
        , div [ class "separator" ] []
        , p [ class "label-pregnancy-concluded" ] [ text <| translate language Translate.PregnancyConcludedLabel ]
        , button
            (classList
                [ ( "ui primary button", True )
                , ( "disabled", isNothing maybeSessionId )
                ]
                :: navigateToPregnancyOutcomeAction
            )
            [ span [ class "text" ] [ text <| translate language Translate.RecordPregnancyOutcome ]
            , span [ class "icon-back" ] []
            ]
        ]
