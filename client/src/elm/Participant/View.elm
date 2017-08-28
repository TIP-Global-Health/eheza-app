module Participant.View
    exposing
        ( getParticipantAge
        , renderParticipantAge
        , viewParticipantTypeFilter
        )

import Date exposing (Date)
import Date.Extra.Duration
import Date.Extra.Period
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, onWithOptions)
import Participant.Model exposing (AgeDay, Participant, ParticipantType(..), ParticipantTypeFilter(..))
import Translate as Trans exposing (translate, Language)


{- Calculates the age of a participant.
   To get current time, see App/Model.currentDate
-}


getParticipantAge : Participant -> Date -> AgeDay
getParticipantAge participant now =
    let
        birthDate =
            case participant.info of
                ParticipantChild child ->
                    child.birthDate

                ParticipantMother mother ->
                    mother.birthDate

        diff =
            Date.Extra.Period.diff birthDate now
    in
        Participant.Model.AgeDay diff.day


renderParticipantAge : Participant -> Date -> String
renderParticipantAge participant now =
    let
        birthDate =
            case participant.info of
                ParticipantChild child ->
                    child.birthDate

                ParticipantMother mother ->
                    mother.birthDate

        diff =
            Date.Extra.Duration.diff birthDate now

        days =
            diff.day

        months =
            diff.month
    in
        if (months == 0) then
            translate language Trans.AgeDays days
        else if (months == 1) then
            translate language Trans.AgeSingleMonth months days
        else if (days == 1) then
            translate language Trans.AgeSingleDay months days
        else
            translate language Trans.Age months days


viewParticipantTypeFilter : Language -> (String -> msg) -> ParticipantTypeFilter -> Html msg
viewParticipantTypeFilter language msg participantTypeFilter =
    div []
        [ select
            [ class "ui dropdown"
            , value <| toString participantTypeFilter
            , onInput msg
            ]
            (List.map
                (\filterType ->
                    option
                        [ value <| toString filterType ]
                        [ text <| toString filterType ]
                )
                [ All, Children, Mothers ]
            )
        ]
