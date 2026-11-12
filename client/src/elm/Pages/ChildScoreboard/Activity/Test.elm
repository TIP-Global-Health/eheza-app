module Pages.ChildScoreboard.Activity.Test exposing (all)

import App.Model
import AssocList as Dict
import Backend.ChildScoreboardEncounter.Model
import Backend.Measurement.Model
    exposing
        ( AdministrationNote(..)
        , VaccinationValue
        , VaccineDose(..)
        , VaccineType(..)
        , WellChildVaccineType(..)
        )
import Backend.Model exposing (emptyModelIndexedDb)
import Date
import EverySet
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (Html, div)
import List.Extra
import Measurement.Model exposing (VaccinationProgressDict, emptyVaccinationForm)
import Measurement.Utils
    exposing
        ( expectVaccineDoseForPerson
        , getAllDosesForVaccine
        , getIntervalForVaccine
        , initialVaccinationDateByBirthDate
        , nextVaccinationDataForVaccine
        , toVaccinationValueWithDefault
        , vaccinationFormDynamicContentAndTasks
        , vaccinationFormWithDefault
        , vaccineDoseToComparable
        , wasFirstDoseAdministeredWithin14DaysFromBirthByVaccinationForm
        , wasInitialOpvAdministeredByVaccinationProgress
        )
import Pages.ChildScoreboard.Activity.Model exposing (ChildScoreboardVaccinationForm, Msg(..), emptyModel)
import Pages.ChildScoreboard.Activity.Update exposing (update)
import Pages.Page exposing (Page(..), UserPage(..))
import Restful.Endpoint exposing (toEntityUuid)
import SyncManager.Model exposing (Site(..))
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, text)
import TestFixtures exposing (expectDoseInHistory, expectSaveEnabled, expectYesAnswerSends, testChild, vaccinationQuestionPrevious, vaccinationQuestionToday)
import Time
import Translate.Model exposing (Language(..))


all : Test
all =
    describe "Pages.ChildScoreboard.Activity"
        [ dtpStandaloneSaveTests
        , doseFallingDueTodayTests
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


{-| The immunisation form as the Child Scoreboard page builds it. The page's
own builder is not exposed, so this one assembles the same config from the
same parts: the child's dose history from earlier encounters, the doses on
the form, the Well Child schedule, and no "will receive today?" question
(CHWs record doses, they do not give them).
-}
childScoreboardVaccinationForm :
    NominalDate
    -> NominalDate
    -> VaccinationProgressDict
    -> WellChildVaccineType
    -> ChildScoreboardVaccinationForm
    -> ( List (Html Msg), Int, Int )
childScoreboardVaccinationForm currentDate birthDate history vaccineType form =
    let
        person =
            testChild birthDate

        initialOpvAdministered =
            if form.administeredDosesDirty then
                wasFirstDoseAdministeredWithin14DaysFromBirthByVaccinationForm birthDate form

            else
                wasFirstDoseAdministeredWithin14DaysFromBirthByVaccinationForm birthDate form
                    || wasInitialOpvAdministeredByVaccinationProgress person.birthDate history

        dosesFromCurrentEncounterData =
            Maybe.map2
                (\doses dates ->
                    List.Extra.zip
                        (EverySet.toList doses |> List.sortBy vaccineDoseToComparable)
                        (EverySet.toList dates |> List.sortWith Date.compare)
                )
                form.administeredDoses
                form.administrationDates
                |> Maybe.withDefault []

        config =
            { birthDate = birthDate
            , expectedDoses =
                getAllDosesForVaccine SiteRwanda initialOpvAdministered vaccineType
                    |> List.filter
                        (\dose ->
                            expectVaccineDoseForPerson currentDate SiteRwanda person initialOpvAdministered history ( vaccineType, dose )
                        )
            , dosesFromPreviousEncountersData =
                Dict.get vaccineType history
                    |> Maybe.withDefault Dict.empty
                    |> Dict.toList
            , dosesFromCurrentEncounterData = dosesFromCurrentEncounterData
            , setVaccinationFormViewModeMsg = SetVaccinationFormViewMode vaccineType
            , setUpdatePreviousVaccinesMsg = SetUpdatePreviousVaccines vaccineType
            , setWillReceiveVaccineTodayMsg = SetWillReceiveVaccineToday vaccineType
            , setAdministrationNoteMsg = SetAdministrationNote vaccineType
            , setVaccinationUpdateDateSelectorStateMsg = SetVaccinationUpdateDateSelectorState vaccineType
            , setVaccinationUpdateDateMsg = SetVaccinationUpdateDate vaccineType
            , saveVaccinationUpdateDateMsg = SaveVaccinationUpdateDate vaccineType
            , deleteVaccinationUpdateDateMsg = DeleteVaccinationUpdateDate vaccineType
            , nextVaccinationDataForVaccine = nextVaccinationDataForVaccine SiteRwanda person.birthDate vaccineType initialOpvAdministered
            , getIntervalForVaccine = always (getIntervalForVaccine SiteRwanda vaccineType)
            , firstDoseExpectedFrom =
                initialVaccinationDateByBirthDate SiteRwanda birthDate initialOpvAdministered history ( vaccineType, VaccineDoseFirst )
            , suggestDoseToday = False
            }
    in
    vaccinationFormDynamicContentAndTasks English currentDate SiteRwanda config (WellChildVaccine vaccineType) form


{-| The Child Scoreboard immunisation form on the day a dose falls due.

The scenarios drive the form through the page's own `update`, with the
messages a tap sends. Save is enabled when the form's tasks completed equal
its tasks active.

  - BCG for a child born today: the first dose falls due on the birth day,
    and it is the only BCG dose.
  - DTP for a 30-week-old catch-up child whose first dose was given four
    weeks ago: the second dose falls due today. The third is expected by age,
    but it is due four weeks after the second.

"Recorded as given previously with today's date" is: "Administered
previously?" Yes, pick today in the date picker (the only day it offers for
a dose that falls due today), save the date.

-}
doseFallingDueTodayTests : Test
doseFallingDueTodayTests =
    let
        today =
            Date.fromCalendarDate 2026 Time.Nov 10

        -- The page model after the given taps, starting from a fresh page.
        afterTaps msgs =
            List.foldl
                (\msg model ->
                    let
                        ( updated, _, _ ) =
                            update today SiteRwanda (toEntityUuid "encounter") emptyModelIndexedDb msg model
                    in
                    updated
                )
                emptyModel
                msgs
                |> .immunisationData

        recordedPreviouslyOn date vaccineType dose =
            afterTaps
                [ SetUpdatePreviousVaccines vaccineType dose True
                , SetVaccinationUpdateDate vaccineType date
                , SaveVaccinationUpdateDate vaccineType dose
                ]

        inputsOf ( inputs, _, _ ) =
            inputs

        asQuery inputs =
            Query.fromHtml (div [] inputs)

        notAskedAbout inputs =
            asQuery inputs
                |> Expect.all
                    [ Query.hasNot [ text vaccinationQuestionPrevious ]
                    , Query.hasNot [ text vaccinationQuestionToday ]
                    ]

        -- What the page saves for the form, as Save does on a new measurement.
        savedValue form =
            toVaccinationValueWithDefault Nothing form

        -- BCG, child born today.
        bcgForm =
            childScoreboardVaccinationForm today today Dict.empty VaccineBCG

        bcgRecordedPreviouslyToday =
            recordedPreviouslyOn today VaccineBCG VaccineDoseFirst
                |> .bcgForm

        -- DTP, catch-up child, dose 1 given four weeks ago, dose 2 due today.
        dtpBirthDate =
            Date.add Date.Weeks -30 today

        dtpFirstDoseDate =
            Date.add Date.Weeks -4 today

        dtpForm =
            childScoreboardVaccinationForm today
                dtpBirthDate
                (Dict.singleton VaccineDTP (Dict.singleton VaccineDoseFirst dtpFirstDoseDate))
                VaccineDTP

        dtpRecordedPreviouslyToday =
            recordedPreviouslyOn today VaccineDTP VaccineDoseSecond
                |> .dtpForm
    in
    describe "Immunisation form, a dose that falls due today"
        [ describe "A dose that falls due today, recorded as given previously with today's date, shows in the dose history and is not asked about again"
            [ test "on the form, once the date is saved" <|
                \_ ->
                    bcgForm bcgRecordedPreviouslyToday
                        |> inputsOf
                        |> Expect.all
                            [ expectDoseInHistory 1 today
                            , notAskedAbout
                            ]
            , test "once the measurement is saved and the form is opened again" <|
                \_ ->
                    vaccinationFormWithDefault emptyVaccinationForm (savedValue bcgRecordedPreviouslyToday)
                        |> bcgForm
                        |> inputsOf
                        |> Expect.all
                            [ expectDoseInHistory 1 today
                            , notAskedAbout
                            ]
            , test "once the measurement is saved and the form is revisited in the same session" <|
                \_ ->
                    vaccinationFormWithDefault bcgRecordedPreviouslyToday (savedValue bcgRecordedPreviouslyToday)
                        |> bcgForm
                        |> Expect.all
                            [ inputsOf >> expectDoseInHistory 1 today
                            , inputsOf >> notAskedAbout
                            , expectSaveEnabled
                            ]
            , test "for a catch-up dose (DTP dose 2, four weeks after dose 1)" <|
                \_ ->
                    dtpForm dtpRecordedPreviouslyToday
                        |> inputsOf
                        |> Expect.all
                            [ expectDoseInHistory 1 dtpFirstDoseDate
                            , expectDoseInHistory 2 today
                            , notAskedAbout
                            ]
            ]
        , describe "After saving that date, the next dose is not asked about (it is not due yet), and Save is enabled"
            [ test "DTP dose 3, expected by age, is due four weeks after dose 2" <|
                \_ ->
                    dtpForm dtpRecordedPreviouslyToday
                        |> Expect.all
                            [ inputsOf >> notAskedAbout
                            , expectSaveEnabled
                            ]
            , test "BCG, which has no next dose" <|
                \_ ->
                    bcgForm bcgRecordedPreviouslyToday
                        |> Expect.all
                            [ inputsOf >> notAskedAbout
                            , expectSaveEnabled
                            ]
            ]
        , describe "A dose that falls due today and has not been recorded is still asked about"
            [ test "\"administered previously?\", about that dose, with Save disabled until it is answered" <|
                \_ ->
                    bcgForm emptyVaccinationForm
                        |> Expect.all
                            [ inputsOf >> asQuery >> Query.has [ text vaccinationQuestionPrevious ]
                            , inputsOf >> expectYesAnswerSends 0 (SetUpdatePreviousVaccines VaccineBCG VaccineDoseFirst True)
                            , \( _, completed, active ) -> Expect.notEqual active completed
                            ]
            , test "\"administered previously?\", for a catch-up dose (DTP dose 2)" <|
                \_ ->
                    dtpForm emptyVaccinationForm
                        |> inputsOf
                        |> Expect.all
                            [ asQuery >> Query.has [ text vaccinationQuestionPrevious ]
                            , expectYesAnswerSends 0 (SetUpdatePreviousVaccines VaccineDTP VaccineDoseSecond True)
                            ]
            , test "\"will receive today?\" is not asked, and No enables Save" <|
                \_ ->
                    afterTaps [ SetUpdatePreviousVaccines VaccineBCG VaccineDoseFirst False ]
                        |> .bcgForm
                        |> bcgForm
                        |> Expect.all
                            [ inputsOf >> asQuery >> Query.hasNot [ text vaccinationQuestionToday ]
                            , expectSaveEnabled
                            ]
            ]
        , describe "A dose given previously on an earlier day behaves as today"
            [ test "BCG recorded on the birth day, three days ago: in the history, deletable, not asked about, Save enabled" <|
                \_ ->
                    let
                        birthDate =
                            Date.add Date.Days -3 today
                    in
                    recordedPreviouslyOn birthDate VaccineBCG VaccineDoseFirst
                        |> .bcgForm
                        |> childScoreboardVaccinationForm today birthDate Dict.empty VaccineBCG
                        |> Expect.all
                            [ inputsOf >> expectDoseInHistory 1 birthDate
                            , inputsOf >> asQuery >> Query.find [ class "history" ] >> Query.has [ class "delete" ]
                            , inputsOf >> notAskedAbout
                            , expectSaveEnabled
                            ]
            , test "DTP dose 2 recorded two weeks ago: the next dose, due in two weeks, is not asked about, Save enabled" <|
                \_ ->
                    let
                        secondDoseDate =
                            Date.add Date.Weeks -2 today
                    in
                    recordedPreviouslyOn secondDoseDate VaccineDTP VaccineDoseSecond
                        |> .dtpForm
                        |> childScoreboardVaccinationForm today
                            dtpBirthDate
                            (Dict.singleton VaccineDTP (Dict.singleton VaccineDoseFirst (Date.add Date.Weeks -8 today)))
                            VaccineDTP
                        |> Expect.all
                            [ inputsOf >> expectDoseInHistory 2 secondDoseDate
                            , inputsOf >> notAskedAbout
                            , expectSaveEnabled
                            ]
            ]
        ]
