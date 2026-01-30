module Pages.Router exposing (activePageByUrl, pageToFragment)

import Activity.Model exposing (Activity)
import Activity.Utils
import Backend.AcuteIllnessActivity.Model exposing (AcuteIllnessActivity)
import Backend.AcuteIllnessActivity.Utils
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessProgressReportInitiator)
import Backend.AcuteIllnessEncounter.Utils
import Backend.ChildScoreboardActivity.Model exposing (ChildScoreboardActivity)
import Backend.ChildScoreboardActivity.Utils
import Backend.HIVActivity.Model exposing (HIVActivity)
import Backend.HIVActivity.Utils
import Backend.HomeVisitActivity.Model exposing (HomeVisitActivity)
import Backend.HomeVisitActivity.Utils
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType, IndividualParticipantInitiator)
import Backend.IndividualEncounterParticipant.Utils exposing (individualEncounterTypeFromString, individualEncounterTypeToString)
import Backend.Measurement.Model exposing (LaboratoryTest)
import Backend.Measurement.Utils
import Backend.NCDActivity.Model exposing (NCDActivity, NCDRecurrentActivity)
import Backend.NCDActivity.Utils
import Backend.NCDEncounter.Types exposing (NCDProgressReportInitiator)
import Backend.NCDEncounter.Utils
import Backend.NutritionActivity.Model exposing (NutritionActivity)
import Backend.NutritionActivity.Utils
import Backend.PatientRecord.Model exposing (PatientRecordInitiator)
import Backend.PatientRecord.Utils
import Backend.Person.Model exposing (Initiator)
import Backend.Person.Utils
import Backend.PrenatalActivity.Model exposing (PrenatalActivity, PrenatalRecurrentActivity)
import Backend.PrenatalActivity.Utils
import Backend.PrenatalEncounter.Model exposing (PrenatalProgressReportInitiator, RecordPreganancyInitiator)
import Backend.PrenatalEncounter.Utils exposing (progressReportInitiatorFromUrlFragment, progressReportInitiatorToUrlFragment, recordPreganancyInitiatorFromUrlFragment, recordPreganancyInitiatorToUrlFragment)
import Backend.TuberculosisActivity.Model exposing (TuberculosisActivity)
import Backend.TuberculosisActivity.Utils
import Backend.WellChildActivity.Model exposing (WellChildActivity)
import Backend.WellChildActivity.Utils
import Pages.Page exposing (AcuteIllnessSubPage(..), ChildWellnessSubPage(..), DashboardPage(..), NCDSubPage(..), NutritionSubPage(..), Page(..), SessionPage(..), UserPage(..))
import Restful.Endpoint exposing (EntityUuid, fromEntityUuid, toEntityUuid)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, custom, map, oneOf, s, string, top)


activePageByUrl : Url -> Page
activePageByUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser
        |> Maybe.withDefault (PageNotFound "Failed to resolve page by analysing URL")


pageToFragment : Page -> Maybe String
pageToFragment current =
    case current of
        DevicePage ->
            Just "device"

        PinCodePage ->
            Just "pincode"

        PageNotFound _ ->
            -- If we couldn't interpret the URL, we don't try to change it.
            Nothing

        ServiceWorkerPage ->
            Just "deployment"

        -- These are pages that require a logged-in user.
        UserPage userPage ->
            case userPage of
                ClinicalPage ->
                    Just "clinical"

                ClinicsPage ->
                    Just "clinics"

                DashboardPage page ->
                    let
                        url =
                            case page of
                                PageMain ->
                                    "main"

                                PageAcuteIllness subPage ->
                                    case subPage of
                                        PageAcuteIllnessOverview ->
                                            "acute-illness"

                                        PageCovid19 ->
                                            "covid-19"

                                        PageMalaria ->
                                            "malaria"

                                        PageGastro ->
                                            "gastro"

                                PageNutrition subPage ->
                                    case subPage of
                                        PageCharts ->
                                            "nutrition-charts"

                                        PageStats ->
                                            "nutrition-stats"

                                        PageCaseManagement ->
                                            "nutrition-case-management"

                                PagePrenatal ->
                                    "prenatal"

                                PageNCD subPage ->
                                    case subPage of
                                        PageHypertension ->
                                            "hypertension"

                                        PageHIV ->
                                            "hiv"

                                        PageDiabetes ->
                                            "diabetes"

                                PageChildWellness subPage ->
                                    case subPage of
                                        PageChildWellnessOverview ->
                                            "child-wellness"

                                        PageChildWellnessNutrition ->
                                            "child-wellness-nutrition"

                                PageGroupEducation ->
                                    "group-education"
                    in
                    Just ("dashboard/" ++ url)

                GlobalCaseManagementPage ->
                    Just "case-management"

                ClinicalProgressReportPage initiator prenatalEncounterId ->
                    Just <| "clinical-progress-report/" ++ fromEntityUuid prenatalEncounterId ++ "/" ++ progressReportInitiatorToUrlFragment initiator

                DemographicsReportPage initiator prenatalEncounterId ->
                    Just <| "demographics-report/" ++ fromEntityUuid prenatalEncounterId ++ "/" ++ progressReportInitiatorToUrlFragment initiator

                MyAccountPage ->
                    Just "my-account"

                CreatePersonPage relationId initiator ->
                    let
                        fragment =
                            Backend.Person.Utils.initiatorToUrlFragment initiator

                        relation =
                            relationId
                                |> Maybe.map (\id -> "/" ++ fromEntityUuid id)
                                |> Maybe.withDefault ""
                    in
                    Just ("person/" ++ fragment ++ "/new" ++ relation)

                EditPersonPage id ->
                    Just ("person/" ++ fromEntityUuid id ++ "/edit")

                PersonPage id initiator ->
                    let
                        fragment =
                            Backend.Person.Utils.initiatorToUrlFragment initiator
                    in
                    Just ("person/" ++ fromEntityUuid id ++ "/" ++ fragment)

                PersonsPage related initiator ->
                    let
                        fragment =
                            Backend.Person.Utils.initiatorToUrlFragment initiator

                        url =
                            case related of
                                Nothing ->
                                    "persons"

                                Just relatedId ->
                                    "relations/" ++ fromEntityUuid relatedId
                    in
                    url ++ "/" ++ fragment |> Just

                PrenatalParticipantPage initiator id ->
                    Just <| "prenatal-participant/" ++ fromEntityUuid id ++ "/" ++ Backend.IndividualEncounterParticipant.Utils.initiatorToUrlFragment initiator

                NutritionParticipantPage initiator id ->
                    Just <| "nutrition-participant/" ++ fromEntityUuid id ++ "/" ++ Backend.IndividualEncounterParticipant.Utils.initiatorToUrlFragment initiator

                AcuteIllnessParticipantPage initiator id ->
                    Just <| "acute-illness-participant/" ++ fromEntityUuid id ++ "/" ++ Backend.IndividualEncounterParticipant.Utils.initiatorToUrlFragment initiator

                WellChildParticipantPage initiator id ->
                    Just <| "well-child-participant/" ++ fromEntityUuid id ++ "/" ++ Backend.IndividualEncounterParticipant.Utils.initiatorToUrlFragment initiator

                NCDParticipantPage initiator id ->
                    Just <| "ncd-participant/" ++ fromEntityUuid id ++ "/" ++ Backend.IndividualEncounterParticipant.Utils.initiatorToUrlFragment initiator

                ChildScoreboardParticipantPage id ->
                    Just <| "child-scoreboard-participant/" ++ fromEntityUuid id

                TuberculosisParticipantPage id ->
                    Just <| "tuberculosis-participant/" ++ fromEntityUuid id

                HIVParticipantPage id ->
                    Just <| "hiv-participant/" ++ fromEntityUuid id

                IndividualEncounterParticipantsPage encounterType ->
                    Just <| "individual-participants/" ++ individualEncounterTypeToString encounterType

                RelationshipPage id1 id2 initiator ->
                    let
                        fragment =
                            Backend.Person.Utils.initiatorToUrlFragment initiator
                    in
                    Just
                        ("relationship/"
                            ++ fromEntityUuid id1
                            ++ "/"
                            ++ fromEntityUuid id2
                            ++ "/"
                            ++ fragment
                        )

                SessionPage sessionId sessionPage ->
                    let
                        subUrl =
                            case sessionPage of
                                ActivitiesPage ->
                                    "/activities"

                                ActivityPage activity ->
                                    "/activity/" ++ Activity.Utils.encodeActivityAsString activity

                                AttendancePage ->
                                    ""

                                ChildPage id ->
                                    "/child/" ++ fromEntityUuid id

                                MotherPage id ->
                                    "/mother/" ++ fromEntityUuid id

                                NextStepsPage id activity ->
                                    "/next-steps/" ++ fromEntityUuid id ++ "/" ++ Activity.Utils.encodeActivityAsString activity

                                ParticipantsPage ->
                                    "/participants"

                                ProgressReportPage id ->
                                    "/progress/" ++ fromEntityUuid id

                        url =
                            "session/" ++ fromEntityUuid sessionId ++ subUrl
                    in
                    Just url

                PrenatalEncounterPage id ->
                    Just <| "prenatal-encounter/" ++ fromEntityUuid id

                PrenatalActivityPage id activity ->
                    Just <| "prenatal-activity/" ++ fromEntityUuid id ++ "/" ++ Backend.PrenatalActivity.Utils.activityToString activity

                PrenatalRecurrentEncounterPage id ->
                    Just <| "prenatal-recurrent-encounter/" ++ fromEntityUuid id

                PrenatalRecurrentActivityPage id activity ->
                    Just <| "prenatal-recurrent-activity/" ++ fromEntityUuid id ++ "/" ++ Backend.PrenatalActivity.Utils.recurrentActivityToString activity

                PrenatalLabsHistoryPage id labEncounterId lab ->
                    Just <|
                        "prenatal-labs-history/"
                            ++ fromEntityUuid id
                            ++ "/"
                            ++ fromEntityUuid labEncounterId
                            ++ "/"
                            ++ Backend.Measurement.Utils.laboratoryTestToString lab

                IndividualEncounterTypesPage ->
                    Just "individual-encounter-types/"

                GroupEncounterTypesPage ->
                    Just "group-encounter-types/"

                PregnancyOutcomePage initiator id ->
                    Just <| "pregnancy-outcome/" ++ fromEntityUuid id ++ "/" ++ recordPreganancyInitiatorToUrlFragment initiator

                NutritionEncounterPage id ->
                    Just <| "nutrition-encounter/" ++ fromEntityUuid id

                NutritionActivityPage id activity ->
                    Just <| "nutrition-activity/" ++ fromEntityUuid id ++ "/" ++ Backend.NutritionActivity.Utils.encodeActivityAsString activity

                NutritionProgressReportPage encounterId ->
                    Just <| "nutrition-progress-report/" ++ fromEntityUuid encounterId

                AcuteIllnessEncounterPage id ->
                    Just <| "acute-illness-encounter/" ++ fromEntityUuid id

                AcuteIllnessActivityPage id activity ->
                    Just <| "acute-illness-activity/" ++ fromEntityUuid id ++ "/" ++ Backend.AcuteIllnessActivity.Utils.encodeActivityAsString activity

                AcuteIllnessProgressReportPage initiator id ->
                    Just <|
                        "acute-illness-progress-report/"
                            ++ fromEntityUuid id
                            ++ "/"
                            ++ Backend.AcuteIllnessEncounter.Utils.progressReportInitiatorToUrlFragment initiator

                AcuteIllnessOutcomePage id ->
                    Just <| "acute-illness-outcome/" ++ fromEntityUuid id

                HomeVisitEncounterPage id ->
                    Just <| "home-visit-encounter/" ++ fromEntityUuid id

                HomeVisitActivityPage id activity ->
                    Just <| "home-visit-activity/" ++ fromEntityUuid id ++ "/" ++ Backend.HomeVisitActivity.Utils.encodeActivityAsString activity

                WellChildEncounterPage id ->
                    Just <| "well-child-encounter/" ++ fromEntityUuid id

                WellChildActivityPage id activity ->
                    Just <| "well-child-activity/" ++ fromEntityUuid id ++ "/" ++ Backend.WellChildActivity.Utils.encodeActivityAsString activity

                WellChildProgressReportPage id ->
                    Just <| "well-child-progress-report/" ++ fromEntityUuid id

                NCDEncounterPage id ->
                    Just <| "ncd-encounter/" ++ fromEntityUuid id

                NCDActivityPage id activity ->
                    Just <| "ncd-activity/" ++ fromEntityUuid id ++ "/" ++ Backend.NCDActivity.Utils.activityToString activity

                NCDRecurrentEncounterPage id ->
                    Just <| "ncd-recurrent-encounter/" ++ fromEntityUuid id

                NCDRecurrentActivityPage id activity ->
                    Just <| "ncd-recurrent-activity/" ++ fromEntityUuid id ++ "/" ++ Backend.NCDActivity.Utils.recurrentActivityToString activity

                NCDProgressReportPage initiator ->
                    Just <| "ncd-progress-report/" ++ Backend.NCDEncounter.Utils.progressReportInitiatorToUrlFragment initiator

                ChildScoreboardEncounterPage id ->
                    Just <| "child-scoreboard-encounter/" ++ fromEntityUuid id

                ChildScoreboardActivityPage id activity ->
                    Just <| "child-scoreboard-activity/" ++ fromEntityUuid id ++ "/" ++ Backend.ChildScoreboardActivity.Utils.activityToString activity

                ChildScoreboardProgressReportPage id ->
                    Just <| "child-scoreboard-progress-report/" ++ fromEntityUuid id

                TuberculosisEncounterPage id ->
                    Just <| "tuberculosis-encounter/" ++ fromEntityUuid id

                TuberculosisActivityPage id activity ->
                    Just <| "tuberculosis-activity/" ++ fromEntityUuid id ++ "/" ++ Backend.TuberculosisActivity.Utils.activityToString activity

                TuberculosisProgressReportPage id ->
                    Just <| "tuberculosis-progress-report/" ++ fromEntityUuid id

                EducationSessionPage id ->
                    Just <| "education-session/" ++ fromEntityUuid id

                HIVEncounterPage id ->
                    Just <| "hiv-encounter/" ++ fromEntityUuid id

                HIVActivityPage id activity ->
                    Just <| "hiv-activity/" ++ fromEntityUuid id ++ "/" ++ Backend.HIVActivity.Utils.activityToString activity

                TraceContactPage id ->
                    Just <| "trace-contact/" ++ fromEntityUuid id

                PatientRecordPage initiator id ->
                    Just <|
                        "patient-record/"
                            ++ fromEntityUuid id
                            ++ "/"
                            ++ Backend.PatientRecord.Utils.progressReportInitiatorToUrlFragment initiator

                MessagingCenterPage ->
                    Just "messaging-center"

                WellbeingPage ->
                    Just "wellbeing"

                MessagingGuide ->
                    Just "messaging-guide"

                StockManagementPage ->
                    Just "stock-management"


parser : Parser (Page -> c) c
parser =
    oneOf
        [ map (UserPage ClinicsPage) (s "clinics")
        , map DevicePage (s "device")
        , map PinCodePage (s "pincode")
        , map ServiceWorkerPage (s "deployment")
        , map (UserPage MyAccountPage) (s "my-account")
        , map (UserPage ClinicalPage) (s "clinical")
        , map (\page -> UserPage <| DashboardPage page) (s "dashboard" </> parseDashboardPage)
        , map (UserPage GlobalCaseManagementPage) (s "case-management")
        , map (\id page -> UserPage <| SessionPage id page) (s "session" </> parseUuid </> parseSessionPage)
        , map (\origin -> UserPage <| PersonsPage Nothing origin) (s "persons" </> parseOrigin)
        , map (\id origin -> UserPage <| PersonsPage (Just id) origin) (s "relations" </> parseUuid </> parseOrigin)
        , map (\origin id -> UserPage <| CreatePersonPage (Just id) origin) (s "person" </> parseOrigin </> s "new" </> parseUuid)
        , map (\origin -> UserPage <| CreatePersonPage Nothing origin) (s "person" </> parseOrigin </> s "new")
        , map (\id -> UserPage <| EditPersonPage id) (s "person" </> parseUuid </> s "edit")
        , map (\id origin -> UserPage <| PersonPage id origin) (s "person" </> parseUuid </> parseOrigin)
        , map (\id initiator -> UserPage <| PrenatalParticipantPage initiator id) (s "prenatal-participant" </> parseUuid </> parseIndividualParticipantInitiator)
        , map (\id initiator -> UserPage <| NutritionParticipantPage initiator id) (s "nutrition-participant" </> parseUuid </> parseIndividualParticipantInitiator)
        , map (\id initiator -> UserPage <| AcuteIllnessParticipantPage initiator id) (s "acute-illness-participant" </> parseUuid </> parseIndividualParticipantInitiator)
        , map (\id initiator -> UserPage <| WellChildParticipantPage initiator id) (s "well-child-participant" </> parseUuid </> parseIndividualParticipantInitiator)
        , map (\id initiator -> UserPage <| NCDParticipantPage initiator id) (s "ncd-participant" </> parseUuid </> parseIndividualParticipantInitiator)
        , map (\id -> UserPage <| ChildScoreboardParticipantPage id) (s "child-scoreboard-participant" </> parseUuid)
        , map (\id -> UserPage <| HIVParticipantPage id) (s "hiv-participant" </> parseUuid)
        , map (\id -> UserPage <| TuberculosisParticipantPage id) (s "tuberculosis-participant" </> parseUuid)
        , map (\id1 id2 origin -> UserPage <| RelationshipPage id1 id2 origin) (s "relationship" </> parseUuid </> parseUuid </> parseOrigin)
        , map (\id -> UserPage <| PrenatalEncounterPage id) (s "prenatal-encounter" </> parseUuid)
        , map (\id activity -> UserPage <| PrenatalActivityPage id activity) (s "prenatal-activity" </> parseUuid </> parsePrenatalActivity)
        , map (\id -> UserPage <| PrenatalRecurrentEncounterPage id) (s "prenatal-recurrent-encounter" </> parseUuid)
        , map (\id activity -> UserPage <| PrenatalRecurrentActivityPage id activity) (s "prenatal-recurrent-activity" </> parseUuid </> parsePrenatalRecurrentActivity)
        , map (\id labEncounterId lab -> UserPage <| PrenatalLabsHistoryPage id labEncounterId lab) (s "prenatal-labs-history" </> parseUuid </> parseUuid </> parseLaboratoryTest)
        , map (\id initiator -> UserPage <| ClinicalProgressReportPage initiator id) (s "clinical-progress-report" </> parseUuid </> parsePrenatalProgressReportInitiator)
        , map (\id initiator -> UserPage <| DemographicsReportPage initiator id) (s "demographics-report" </> parseUuid </> parsePrenatalProgressReportInitiator)
        , map (UserPage <| IndividualEncounterTypesPage) (s "individual-encounter-types")
        , map (UserPage <| GroupEncounterTypesPage) (s "group-encounter-types")
        , map (\encounterType -> UserPage <| IndividualEncounterParticipantsPage encounterType) (s "individual-participants" </> parseIndividualEncounterType)
        , map (\id initiator -> UserPage <| PregnancyOutcomePage initiator id) (s "pregnancy-outcome" </> parseUuid </> parseRecordPreganancyInitiator)
        , map (\id -> UserPage <| NutritionEncounterPage id) (s "nutrition-encounter" </> parseUuid)
        , map (\id activity -> UserPage <| NutritionActivityPage id activity) (s "nutrition-activity" </> parseUuid </> parseNutritionActivity)
        , map (\id -> UserPage <| NutritionProgressReportPage id) (s "nutrition-progress-report" </> parseUuid)
        , map (\id -> UserPage <| AcuteIllnessEncounterPage id) (s "acute-illness-encounter" </> parseUuid)
        , map (\id activity -> UserPage <| AcuteIllnessActivityPage id activity) (s "acute-illness-activity" </> parseUuid </> parseAcuteIllnessActivity)
        , map (\id initiator -> UserPage <| AcuteIllnessProgressReportPage initiator id) (s "acute-illness-progress-report" </> parseUuid </> parseAcuteIllnessProgressReportInitiator)
        , map (\id -> UserPage <| AcuteIllnessOutcomePage id) (s "acute-illness-outcome" </> parseUuid)
        , map (\id -> UserPage <| HomeVisitEncounterPage id) (s "home-visit-encounter" </> parseUuid)
        , map (\id activity -> UserPage <| HomeVisitActivityPage id activity) (s "home-visit-activity" </> parseUuid </> parseHomeVisitActivity)
        , map (\id -> UserPage <| WellChildEncounterPage id) (s "well-child-encounter" </> parseUuid)
        , map (\id activity -> UserPage <| WellChildActivityPage id activity) (s "well-child-activity" </> parseUuid </> parseWellChildActivity)
        , map (\id -> UserPage <| WellChildProgressReportPage id) (s "well-child-progress-report" </> parseUuid)
        , map (\id -> UserPage <| NCDEncounterPage id) (s "ncd-encounter" </> parseUuid)
        , map (\id activity -> UserPage <| NCDActivityPage id activity) (s "ncd-activity" </> parseUuid </> parseNCDActivity)
        , map (\id -> UserPage <| ChildScoreboardEncounterPage id) (s "child-scoreboard-encounter" </> parseUuid)
        , map (\id activity -> UserPage <| ChildScoreboardActivityPage id activity) (s "child-scoreboard-activity" </> parseUuid </> parseChildScoreboardActivity)
        , map (\id -> UserPage <| ChildScoreboardProgressReportPage id) (s "child-scoreboard-progress-report" </> parseUuid)
        , map (\id -> UserPage <| NCDRecurrentEncounterPage id) (s "ncd-recurrent-encounter" </> parseUuid)
        , map (\id activity -> UserPage <| NCDRecurrentActivityPage id activity) (s "ncd-recurrent-activity" </> parseUuid </> parseNCDRecurrentActivity)
        , map (\initiator -> UserPage <| NCDProgressReportPage initiator) (s "ncd-progress-report" </> parseNCDProgressReportInitiator)
        , map (\id -> UserPage <| TuberculosisEncounterPage id) (s "tuberculosis-encounter" </> parseUuid)
        , map (\id activity -> UserPage <| TuberculosisActivityPage id activity) (s "tuberculosis-activity" </> parseUuid </> parseTuberculosisActivity)
        , map (\id -> UserPage <| TuberculosisProgressReportPage id) (s "tuberculosis-progress-report" </> parseUuid)
        , map (\id -> UserPage <| EducationSessionPage id) (s "education-session" </> parseUuid)
        , map (\id -> UserPage <| HIVEncounterPage id) (s "hiv-encounter" </> parseUuid)
        , map (\id activity -> UserPage <| HIVActivityPage id activity) (s "hiv-activity" </> parseUuid </> parseHIVActivity)
        , map (\id -> UserPage <| TraceContactPage id) (s "trace-contact" </> parseUuid)
        , map (\id initiator -> UserPage <| PatientRecordPage initiator id) (s "patient-record" </> parseUuid </> parsePatientRecordInitiator)
        , map (UserPage MessagingCenterPage) (s "messaging-center")
        , map (UserPage WellbeingPage) (s "wellbeing")
        , map (UserPage MessagingGuide) (s "messaging-guide")
        , map (UserPage StockManagementPage) (s "stock-management")

        -- `top` represents the page without any segements ... i.e. the root page.
        , map PinCodePage top
        ]


parseDashboardPage : Parser (DashboardPage -> c) c
parseDashboardPage =
    oneOf
        [ map PageMain (s "main")
        , map (PageNutrition PageCharts) (s "nutrition-charts")
        , map (PageNutrition PageStats) (s "nutrition-stats")
        , map (PageNutrition PageCaseManagement) (s "nutrition-case-management")
        , map (PageAcuteIllness PageAcuteIllnessOverview) (s "acute-illness")
        , map (PageAcuteIllness PageCovid19) (s "covid-19")
        , map (PageAcuteIllness PageMalaria) (s "malaria")
        , map (PageAcuteIllness PageGastro) (s "gastro")
        , map PagePrenatal (s "prenatal")
        , map (PageNCD PageHypertension) (s "hypertension")
        , map (PageNCD PageHIV) (s "hiv")
        , map (PageNCD PageDiabetes) (s "diabetes")
        , map (PageChildWellness PageChildWellnessOverview) (s "child-wellness")
        , map (PageChildWellness PageChildWellnessNutrition) (s "child-wellness-nutrition")
        , map PageGroupEducation (s "group-education")
        ]


parseSessionPage : Parser (SessionPage -> c) c
parseSessionPage =
    oneOf
        [ map ActivitiesPage (s "activities")
        , map ActivityPage (s "activity" </> parseActivity)
        , map ChildPage (s "child" </> parseUuid)
        , map ProgressReportPage (s "progress" </> parseUuid)
        , map MotherPage (s "mother" </> parseUuid)
        , map NextStepsPage (s "next-steps" </> parseUuid </> parseActivity)
        , map ParticipantsPage (s "participants")
        , map AttendancePage top
        ]


parseUuid : Parser (EntityUuid a -> c) c
parseUuid =
    map toEntityUuid string


parseActivity : Parser (Activity -> c) c
parseActivity =
    custom "Activity" Activity.Utils.decodeActivityFromString


parsePrenatalActivity : Parser (PrenatalActivity -> c) c
parsePrenatalActivity =
    custom "PrenatalActivity" Backend.PrenatalActivity.Utils.activityFromString


parsePrenatalRecurrentActivity : Parser (PrenatalRecurrentActivity -> c) c
parsePrenatalRecurrentActivity =
    custom "PrenatalRecurrentActivity" Backend.PrenatalActivity.Utils.recurrentActivityFromString


parseLaboratoryTest : Parser (LaboratoryTest -> c) c
parseLaboratoryTest =
    custom "LaboratoryTest" Backend.Measurement.Utils.laboratoryTestFromString


parseNutritionActivity : Parser (NutritionActivity -> c) c
parseNutritionActivity =
    custom "NutritionActivity" Backend.NutritionActivity.Utils.decodeActivityFromString


parseAcuteIllnessActivity : Parser (AcuteIllnessActivity -> c) c
parseAcuteIllnessActivity =
    custom "AcuteIllnessActivity" Backend.AcuteIllnessActivity.Utils.decodeActivityFromString


parseHomeVisitActivity : Parser (HomeVisitActivity -> c) c
parseHomeVisitActivity =
    custom "HomeVisitActivity" Backend.HomeVisitActivity.Utils.decodeActivityFromString


parseWellChildActivity : Parser (WellChildActivity -> c) c
parseWellChildActivity =
    custom "WellChildActivity" Backend.WellChildActivity.Utils.decodeActivityFromString


parseNCDActivity : Parser (NCDActivity -> c) c
parseNCDActivity =
    custom "NCDActivity" Backend.NCDActivity.Utils.activityFromString


parseNCDRecurrentActivity : Parser (NCDRecurrentActivity -> c) c
parseNCDRecurrentActivity =
    custom "NCDRecurrentActivity" Backend.NCDActivity.Utils.recurrentActivityFromString


parseChildScoreboardActivity : Parser (ChildScoreboardActivity -> c) c
parseChildScoreboardActivity =
    custom "ChildScoreboardActivity" Backend.ChildScoreboardActivity.Utils.activityFromString


parseTuberculosisActivity : Parser (TuberculosisActivity -> c) c
parseTuberculosisActivity =
    custom "TuberculosisActivity" Backend.TuberculosisActivity.Utils.activityFromString


parseHIVActivity : Parser (HIVActivity -> c) c
parseHIVActivity =
    custom "HIVActivity" Backend.HIVActivity.Utils.activityFromString


parseIndividualEncounterType : Parser (IndividualEncounterType -> c) c
parseIndividualEncounterType =
    custom "IndividualEncounterType" individualEncounterTypeFromString


parseOrigin : Parser (Initiator -> c) c
parseOrigin =
    custom "Initiator" Backend.Person.Utils.initiatorFromUrlFragment


parseRecordPreganancyInitiator : Parser (RecordPreganancyInitiator -> c) c
parseRecordPreganancyInitiator =
    custom "RecordPreganancyInitiator" recordPreganancyInitiatorFromUrlFragment


parsePrenatalProgressReportInitiator : Parser (PrenatalProgressReportInitiator -> c) c
parsePrenatalProgressReportInitiator =
    custom "PrenatalProgressReportInitiator" progressReportInitiatorFromUrlFragment


parseAcuteIllnessProgressReportInitiator : Parser (AcuteIllnessProgressReportInitiator -> c) c
parseAcuteIllnessProgressReportInitiator =
    custom "AcuteIllnessProgressReportInitiator" Backend.AcuteIllnessEncounter.Utils.progressReportInitiatorFromUrlFragment


parsePatientRecordInitiator : Parser (PatientRecordInitiator -> c) c
parsePatientRecordInitiator =
    custom "PatientRecordInitiator" Backend.PatientRecord.Utils.progressReportInitiatorFromUrlFragment


parseIndividualParticipantInitiator : Parser (IndividualParticipantInitiator -> c) c
parseIndividualParticipantInitiator =
    custom "IndividualParticipantInitiator" Backend.IndividualEncounterParticipant.Utils.initiatorFromUrlFragment


parseNCDProgressReportInitiator : Parser (NCDProgressReportInitiator -> c) c
parseNCDProgressReportInitiator =
    custom "NCDProgressReportInitiator" Backend.NCDEncounter.Utils.progressReportInitiatorFromUrlFragment
