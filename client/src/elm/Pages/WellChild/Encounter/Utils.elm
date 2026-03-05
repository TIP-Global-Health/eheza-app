module Pages.WellChild.Encounter.Utils exposing (..)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.WellChildActivity.Model exposing (WellChildActivity(..))
import Backend.WellChildEncounter.Model exposing (PediatricCareMilestone(..), WellChildEncounterType(..))
import Date exposing (Unit(..))
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust)
import Measurement.Utils
import Pages.WellChild.Activity.Utils exposing (mandatoryDangerSignsTasksCompleted, mandatoryNutritionAssessmentTasksCompleted)
import Pages.WellChild.Encounter.Model exposing (..)
import RemoteData exposing (WebData)
import SyncManager.Model exposing (Site)


generateAssembledData : Site -> WellChildEncounterId -> ModelIndexedDb -> WebData AssembledData
generateAssembledData =
    Measurement.Utils.generateAssembledDataForWellChild


resolvePediatricCareMilestoneOnDate : NominalDate -> NominalDate -> Maybe PediatricCareMilestone
resolvePediatricCareMilestoneOnDate dueDate birthDate =
    let
        ageWeeks =
            Date.diff Weeks birthDate dueDate
    in
    if ageWeeks < 6 then
        Nothing

    else if ageWeeks < 14 then
        Just Milestone6Weeks

    else
        let
            ageMonths =
                Date.diff Months birthDate dueDate
        in
        if ageMonths < 6 then
            Just Milestone14Weeks

        else if ageMonths < 9 then
            Just Milestone6Months

        else if ageMonths < 12 then
            Just Milestone9Months

        else if ageMonths < 15 then
            Just Milestone12Months

        else if ageMonths < 18 then
            Just Milestone15Months

        else if ageMonths < 24 then
            Just Milestone18Months

        else if ageMonths < 36 then
            Just Milestone2Years

        else if ageMonths < 48 then
            Just Milestone3Years

        else
            Just Milestone4Years


resolveDateForPediatricCareMilestone : NominalDate -> PediatricCareMilestone -> NominalDate
resolveDateForPediatricCareMilestone birthDate milestone =
    case milestone of
        Milestone6Weeks ->
            Date.add Weeks 6 birthDate

        Milestone14Weeks ->
            Date.add Weeks 14 birthDate

        Milestone6Months ->
            Date.add Months 6 birthDate

        Milestone9Months ->
            Date.add Months 9 birthDate

        Milestone12Months ->
            Date.add Years 1 birthDate

        Milestone15Months ->
            Date.add Months 15 birthDate

        Milestone18Months ->
            Date.add Months 18 birthDate

        Milestone2Years ->
            Date.add Years 2 birthDate

        Milestone3Years ->
            Date.add Years 3 birthDate

        Milestone4Years ->
            Date.add Years 4 birthDate


pediatricCareMilestoneToComparable : PediatricCareMilestone -> Int
pediatricCareMilestoneToComparable milestone =
    case milestone of
        Milestone6Weeks ->
            1

        Milestone14Weeks ->
            2

        Milestone6Months ->
            3

        Milestone9Months ->
            4

        Milestone12Months ->
            5

        Milestone15Months ->
            6

        Milestone18Months ->
            7

        Milestone2Years ->
            8

        Milestone3Years ->
            9

        Milestone4Years ->
            10


allowEndingEncounter : NominalDate -> Site -> List WellChildActivity -> AssembledData -> Bool
allowEndingEncounter currentDate site pendingActivities assembled =
    List.filter (\activity -> not <| List.member activity [ WellChildNCDA, WellChildPhoto ]) pendingActivities
        |> (\pending ->
                case pending of
                    [] ->
                        True

                    [ WellChildDangerSigns ] ->
                        if assembled.encounter.encounterType == PediatricCare then
                            False

                        else
                            mandatoryDangerSignsTasksCompleted currentDate site assembled

                    [ WellChildNutritionAssessment ] ->
                        if assembled.encounter.encounterType == PediatricCare then
                            False

                        else
                            mandatoryNutritionAssessmentTasksCompleted currentDate site assembled

                    [ WellChildDangerSigns, WellChildNutritionAssessment ] ->
                        if assembled.encounter.encounterType == PediatricCare then
                            False

                        else
                            mandatoryDangerSignsTasksCompleted currentDate site assembled
                                && mandatoryNutritionAssessmentTasksCompleted currentDate site assembled

                    [ WellChildNutritionAssessment, WellChildDangerSigns ] ->
                        if assembled.encounter.encounterType == PediatricCare then
                            False

                        else
                            mandatoryDangerSignsTasksCompleted currentDate site assembled
                                && mandatoryNutritionAssessmentTasksCompleted currentDate site assembled

                    _ ->
                        False
           )
