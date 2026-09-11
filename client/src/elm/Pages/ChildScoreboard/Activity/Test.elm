module Pages.ChildScoreboard.Activity.Test exposing (all)

import App.Model
import Backend.ChildScoreboardEncounter.Model
import Backend.Measurement.Model
    exposing
        ( AdministrationNote(..)
        , VaccinationValue
        , VaccineDose(..)
        )
import Backend.Model exposing (emptyModelIndexedDb)
import Date
import EverySet
import Expect
import Measurement.Model exposing (emptyVaccinationForm)
import Pages.ChildScoreboard.Activity.Model exposing (Msg(..), emptyModel)
import Pages.ChildScoreboard.Activity.Update exposing (update)
import Pages.Page exposing (Page(..), UserPage(..))
import Restful.Endpoint exposing (toEntityUuid)
import SyncManager.Model exposing (Site(..))
import Test exposing (Test, describe, test)
import Time


all : Test
all =
    describe "Pages.ChildScoreboard.Activity"
        [ dtpStandaloneSaveTests
        ]


{-| The DTP-standalone task keeps its own form (`dtpStandaloneForm`), separate
from the DTP form (`dtpForm`). Saving it must store what `dtpStandaloneForm`
holds.
-}
dtpStandaloneSaveTests : Test
dtpStandaloneSaveTests =
    let
        today =
            Date.fromCalendarDate 2026 Time.Aug 31

        enteredDoses =
            EverySet.singleton VaccineDoseFirst

        enteredDates =
            EverySet.singleton today

        encounterId =
            toEntityUuid "encounter"

        personId =
            toEntityUuid "person"

        saveMsgs form saved =
            let
                data =
                    emptyModel.immunisationData

                ( _, _, appMsgs ) =
                    update today
                        SiteRwanda
                        encounterId
                        emptyModelIndexedDb
                        (SaveDTPStandaloneImmunisation personId saved Nothing)
                        { emptyModel | immunisationData = { data | dtpStandaloneForm = form } }
            in
            appMsgs

        expectedSaveMsg measurementId value =
            App.Model.MsgIndexedDb
                (Backend.Model.MsgChildScoreboardEncounter encounterId
                    (Backend.ChildScoreboardEncounter.Model.SaveDTPStandaloneImmunisation personId measurementId value)
                )

        backToEncounterPageMsg =
            App.Model.SetActivePage (UserPage (ChildScoreboardEncounterPage encounterId))
    in
    describe "the DTP-standalone save"
        [ test "stores the doses, dates and note entered on the DTP-standalone form" <|
            \_ ->
                saveMsgs
                    { emptyVaccinationForm
                        | administeredDoses = Just enteredDoses
                        , administrationDates = Just enteredDates
                        , administrationNote = Just AdministeredToday
                    }
                    Nothing
                    |> Expect.equal
                        [ expectedSaveMsg Nothing
                            (VaccinationValue enteredDoses enteredDates AdministeredToday)
                        , backToEncounterPageMsg
                        ]
        , test "on edit, takes doses and dates from the saved measurement and saves under its id" <|
            \_ ->
                let
                    measurementId =
                        toEntityUuid "measurement"

                    savedDoses =
                        EverySet.singleton VaccineDoseSecond

                    savedDates =
                        EverySet.singleton (Date.fromCalendarDate 2026 Time.Aug 24)

                    measurement =
                        { dateMeasured = today
                        , nurse = Nothing
                        , healthCenter = Nothing
                        , participantId = personId
                        , deleted = False
                        , encounterId = Just encounterId
                        , value = VaccinationValue savedDoses savedDates AdministeredPreviously
                        }
                in
                saveMsgs
                    { emptyVaccinationForm
                        | administrationNote = Just AdministeredToday
                        , administrationNoteDirty = True
                    }
                    (Just ( measurementId, measurement ))
                    |> Expect.equal
                        [ expectedSaveMsg (Just measurementId)
                            (VaccinationValue savedDoses savedDates AdministeredToday)
                        , backToEncounterPageMsg
                        ]
        ]
