module Pages.Activity.Utils exposing (filterParticipantsByName, resolveChildToLoad, summarizeForActivity, viewChildMeasurements, viewMotherMeasurements)

import Activity.Model exposing (ChildActivity, CompletedAndPending, MotherActivity)
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (resolvePreviousValuesSetForChild)
import Backend.Person.Model exposing (Person)
import Backend.Session.Model exposing (EditableSession)
import Backend.Session.Utils exposing (getChild, getChildActivityMeasurementData, getMother, getMotherActivityMeasurementData)
import EverySet exposing (EverySet)
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
import Participant.Model exposing (Participant)
import SyncManager.Model exposing (Site, SiteFeature)
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
    getChildActivityMeasurementData activity childId session
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
    getMotherActivityMeasurementData activity motherId session
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


{-| The checked-in participants the activity expects, split into completed and
pending, before the name filter.
-}
summarizeForActivity :
    Participant id value activity msg NominalDate
    -> NominalDate
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> Bool
    -> activity
    -> EditableSession
    -> ModelIndexedDb
    -> CompletedAndPending (Dict id value)
summarizeForActivity config currentDate zscores features isChw activity session db =
    LocalData.unwrap
        { completed = Dict.empty
        , pending = Dict.empty
        }
        (config.summarizeParticipantsForActivity currentDate zscores features activity session.offlineSession isChw db)
        session.checkedIn


{-| The participant an activity page shows and the session fetch loads: the
selected one while on the open tab, otherwise its first. `children` come
before the name filter.
-}
resolveChildToLoad : Model PersonId -> CompletedAndPending (Dict PersonId Person) -> Maybe PersonId
resolveChildToLoad model children =
    let
        filtered =
            filterParticipantsByName .name (normalizeFilter model.filter) children

        onTab =
            case model.selectedTab of
                Completed ->
                    filtered.completed

                Pending ->
                    filtered.pending
    in
    model.selectedParticipant
        |> Maybe.Extra.filter (\participantId -> Dict.member participantId onTab)
        |> Maybe.Extra.orElse (Dict.keys onTab |> List.head)


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
