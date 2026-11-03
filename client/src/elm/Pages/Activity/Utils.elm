module Pages.Activity.Utils exposing (filterParticipantsByName, resolveChildToLoad, resolveSelectedParticipant, viewChildMeasurements, viewMotherMeasurements)

import Activity.Model exposing (ChildActivity, CompletedAndPending, MotherActivity)
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (resolvePreviousValuesSetForChild)
import Backend.Person.Model exposing (Person)
import Backend.Session.Model exposing (EditableSession)
import Backend.Session.Utils exposing (getChild, getChildMeasurementData, getMother, getMotherMeasurementData)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (Html)
import LocalData
import Maybe.Extra
import Measurement.Model
import Measurement.Utils exposing (getChildForm, getMotherForm)
import Measurement.View
import Pages.Activity.Model exposing (Model, Msg(..), Tab(..))
import Pages.Session.Model
import Pages.Utils exposing (matchFilter, normalizeFilter)
import SyncManager.Model exposing (Site)
import Translate exposing (Language)
import ZScore.Model


{-| These are conveniences for the way the code was structured.
Ideally, we'd have smaller capabilities in `Participant` that
this could be built on more generically, but this will do for now.
-}
viewChildMeasurements :
    Language
    -> NominalDate
    -> Site
    -> ZScore.Model.Model
    -> Bool
    -> ModelIndexedDb
    -> PersonId
    -> ChildActivity
    -> Pages.Session.Model.Model
    -> EditableSession
    -> Html (Msg PersonId Measurement.Model.MsgChild)
viewChildMeasurements language currentDate site zscores isChw db childId activity pages session =
    let
        form =
            getChildForm site childId pages session
    in
    getChildMeasurementData childId session
        |> LocalData.unwrap
            emptyNode
            (\measurements ->
                getChild childId session.offlineSession
                    |> Maybe.map
                        (\child ->
                            resolvePreviousValuesSetForChild currentDate site childId db
                                |> Measurement.View.viewChild language currentDate site isChw ( childId, child ) activity measurements zscores session db form
                                |> Html.map MsgMeasurement
                        )
                    |> Maybe.withDefault emptyNode
            )


viewMotherMeasurements : Language -> PersonId -> MotherActivity -> Pages.Session.Model.Model -> EditableSession -> Html (Msg PersonId Measurement.Model.MsgMother)
viewMotherMeasurements language motherId activity pages session =
    let
        form =
            getMotherForm motherId pages session
    in
    getMotherMeasurementData motherId session
        |> LocalData.unwrap
            emptyNode
            (\measurements ->
                getMother motherId session.offlineSession
                    |> Maybe.map
                        (\_ ->
                            Measurement.View.viewMother language activity session.offlineSession.session.clinicType measurements form
                                |> Html.map MsgMeasurement
                        )
                    |> Maybe.withDefault emptyNode
            )


{-| The child whose individual data the session fetch loads on a child
activity page: the child the page shows. `children` are the activity's
children, before the name filter.
-}
resolveChildToLoad : Model PersonId -> CompletedAndPending (Dict PersonId Person) -> Maybe PersonId
resolveChildToLoad model children =
    filterParticipantsByName .name (normalizeFilter model.filter) children
        |> resolveSelectedParticipant model


{-| Keeps the participants whose name matches the filter typed on the page,
given as `normalizeFilter` returns it.
-}
filterParticipantsByName : (participant -> String) -> String -> CompletedAndPending (Dict id participant) -> CompletedAndPending (Dict id participant)
filterParticipantsByName getName filter participants =
    let
        nameMatches _ participant =
            getName participant
                |> matchFilter filter
    in
    { pending = Dict.filter nameMatches participants.pending
    , completed = Dict.filter nameMatches participants.completed
    }


{-| The participant the page shows: the selected one while it is on the open
tab, otherwise the first one on that tab.
-}
resolveSelectedParticipant : Model id -> CompletedAndPending (Dict id participant) -> Maybe id
resolveSelectedParticipant model participants =
    let
        onTab =
            case model.selectedTab of
                Completed ->
                    participants.completed

                Pending ->
                    participants.pending
    in
    model.selectedParticipant
        |> Maybe.Extra.filter (\participantId -> Dict.member participantId onTab)
        |> Maybe.Extra.orElse (Dict.keys onTab |> List.head)
