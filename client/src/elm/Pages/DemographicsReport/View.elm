module Pages.DemographicsReport.View exposing (view, viewHeader, viewItemContent, viewItemHeading)

import App.Model exposing (Msg(..))
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter)
import Backend.PrenatalParticipant.Model exposing (PrenatalParticipant)
import EveryDict
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.WebData exposing (viewWebData)


type alias FetchedData =
    { encounter : PrenatalEncounter
    , participant : PrenatalParticipant
    , person : Person
    , id : PrenatalEncounterId
    }


view : Language -> NominalDate -> PrenatalEncounterId -> ModelIndexedDb -> Html Msg
view language currentDate prenatalEncounterId db =
    let
        encounter =
            EveryDict.get prenatalEncounterId db.prenatalEncounters
                |> Maybe.withDefault NotAsked

        participant =
            encounter
                |> RemoteData.andThen
                    (\encounter ->
                        EveryDict.get encounter.participant db.prenatalParticipants
                            |> Maybe.withDefault NotAsked
                    )

        person =
            participant
                |> RemoteData.andThen
                    (\participant ->
                        EveryDict.get participant.person db.people
                            |> Maybe.withDefault NotAsked
                    )

        data =
            RemoteData.map FetchedData encounter
                |> RemoteData.andMap participant
                |> RemoteData.andMap person
                |> RemoteData.andMap (Success prenatalEncounterId)

        header =
            viewHeader language prenatalEncounterId Translate.DemographicsReport

        content =
            viewWebData language (viewContent language currentDate) identity data
    in
    div [ class "page-demographics-report" ] <|
        [ header
        , content
        ]


viewHeader : Language -> PrenatalEncounterId -> TranslationId -> Html Msg
viewHeader language prenatalEncounterId label =
    div
        [ class "ui basic segment head" ]
        [ a
            [ class "icon-back"
            , onClick <| SetActivePage <| UserPage <| PrenatalEncounterPage prenatalEncounterId
            ]
            []
        , h1 [ class "ui header" ]
            [ text <| translate language label ]
        ]


viewContent : Language -> NominalDate -> FetchedData -> Html Msg
viewContent language currentDate data =
    let
        maybeString value =
            value |> Maybe.withDefault ""
    in
    div [ class "ui unstackable items" ]
        [ viewItemHeading language Translate.PatientInformation "gray"
        , viewItemContent language Translate.FirstName data.person.firstName
        , viewItemContent language Translate.SecondName data.person.secondName
        , viewItemContent language Translate.NationalIdNumber (maybeString data.person.nationalIdNumber)
        ]


viewItemHeading : Language -> TranslationId -> String -> Html Msg
viewItemHeading language label color =
    div [ class <| "pane-heading " ++ color ]
        [ text <| translate language label ]


viewItemContent : Language -> TranslationId -> String -> Html Msg
viewItemContent language label value =
    div [ class "pane-content" ]
        [ span [ class "label" ] [ text <| translate language label ++ ":" ]
        , span [ class "value" ] [ text value ]
        ]
