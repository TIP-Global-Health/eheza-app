module Pages.HealthyStart.Participant.View exposing (view)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.HealthyStartEncounter.Model
import Backend.IndividualEncounterParticipant.Model
    exposing
        ( IndividualEncounterParticipant
        , emptyIndividualEncounterParticipant
        )
import Backend.IndividualEncounterParticipant.Utils exposing (isDailyEncounterActive)
import Backend.Model exposing (ModelIndexedDb)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isNothing)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> HealthCenterId -> PersonId -> ModelIndexedDb -> Html App.Model.Msg
view language currentDate selectedHealthCenter id db =
    let
        sessions =
            Dict.get id db.individualParticipantsByPerson
                |> Maybe.withDefault NotAsked
    in
    div
        [ class "wrap wrap-alt-2 page-participant individual healthy-start" ]
        [ viewHeader language
        , div
            [ class "ui full segment" ]
            [ viewWebData language (viewActions language currentDate selectedHealthCenter id db) identity sessions
            ]
        ]


viewHeader : Language -> Html App.Model.Msg
viewHeader language =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterLabel
                        Backend.IndividualEncounterParticipant.Model.HealthyStartEncounter
                        False
            ]
        ]


viewActions :
    Language
    -> NominalDate
    -> HealthCenterId
    -> PersonId
    -> ModelIndexedDb
    -> Dict IndividualEncounterParticipantId IndividualEncounterParticipant
    -> Html App.Model.Msg
viewActions language currentDate selectedHealthCenter id db participants =
    div []
        [ p [ class "label-visit" ]
            [-- text <|
             --    translate language <|
             --        Translate.IndividualEncounterSelectVisitLabel
             --            Backend.IndividualEncounterParticipant.Model.HealthyStartEncounter
            ]
        , div [ class "ui unstackable items" ] <|
            viewAction language currentDate selectedHealthCenter id participants db
        ]


viewAction :
    Language
    -> NominalDate
    -> HealthCenterId
    -> PersonId
    -> Dict IndividualEncounterParticipantId IndividualEncounterParticipant
    -> ModelIndexedDb
    -> List (Html App.Model.Msg)
viewAction language currentDate selectedHealthCenter id participants db =
    []
