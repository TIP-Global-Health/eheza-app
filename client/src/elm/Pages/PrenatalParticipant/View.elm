module Pages.PrenatalParticipant.View exposing (view)

import App.Model
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter)
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
            EveryDict.get id db.individualParticipantsByPerson
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
            , onClick <| App.Model.SetActivePage <| UserPage <| IndividualEncounterParticipantsPage AntenatalEncounter
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewPrenatalActions : Language -> NominalDate -> PersonId -> ModelIndexedDb -> EveryDictList IndividualEncounterParticipantId IndividualEncounterParticipant -> Html App.Model.Msg
viewPrenatalActions language currentDate id db prenatalSessions =
    let
        maybeSessionId =
            prenatalSessions
                |> EveryDictList.toList
                |> List.filter (\( _, session ) -> isNothing session.endDate)
                |> List.head
                |> Maybe.map Tuple.first

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

        firstVisitAction =
            maybeSessionId
                |> Maybe.map
                    (\sessionId ->
                        [ PrenatalEncounter sessionId currentDate Nothing
                            |> Backend.Model.PostPrenatalEncounter
                            |> App.Model.MsgIndexedDb
                            |> onClick
                        ]
                    )
                |> Maybe.withDefault
                    [ IndividualEncounterParticipant id AntenatalEncounter currentDate Nothing
                        |> Backend.Model.PostIndividualSession
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
                    (\encounterId ->
                        [ onClick <|
                            App.Model.SetActivePage <|
                                UserPage <|
                                    Pages.Page.PrenatalEncounterPage encounterId
                        ]
                    )
    in
    div []
        [ p [ class "label-antenatal-visit" ] [ text <| translate language Translate.SelectAntenatalVisit ]
        , button
            (classList
                [ ( "ui primary button", True )
                , ( "disabled", isJust maybeSessionId )
                ]
                :: firstVisitAction
            )
            [ span [ class "text" ] [ text <| translate language Translate.FirstAntenatalVisit ]
            , span [ class "icon-back" ] []
            ]
        , button
            (classList
                [ ( "ui primary button", True )
                , ( "disabled", isNothing maybeSessionId )
                ]
                :: subsequentVisitAction
            )
            [ span [ class "text" ] [ text <| translate language Translate.SubsequentAntenatalVisit ]
            , span [ class "icon-back" ] []
            ]
        , div [ class "separator" ] []
        , p [ class "label-pregnancy-concluded" ] [ text <| translate language Translate.PregnancyConcludedLabel ]
        , button
            [ classList
                [ ( "ui primary button", True )
                , ( "disabled", isNothing maybeSessionId )
                ]
            ]
            [ span [ class "text" ] [ text <| translate language Translate.RecordPregnancyOutcome ]
            , span [ class "icon-back" ] []
            ]
        ]
