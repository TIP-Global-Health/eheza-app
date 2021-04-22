module Pages.Router exposing (activePageByUrl, pageToFragment)

import Activity.Model exposing (Activity)
import Activity.Utils
import AcuteIllnessActivity.Model exposing (AcuteIllnessActivity(..))
import AcuteIllnessActivity.Utils
import Backend.HomeVisitActivity.Model exposing (HomeVisitActivity(..))
import Backend.HomeVisitActivity.Utils
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.IndividualEncounterParticipant.Utils exposing (decodeIndividualEncounterTypeFromString, encodeIndividualEncounterTypeAsString)
import Backend.NutritionActivity.Model exposing (NutritionActivity(..))
import Backend.NutritionActivity.Utils
import Backend.Person.Model exposing (Initiator(..))
import Backend.Person.Utils exposing (initiatorFromUrlFragmemt, initiatorToUrlFragmemt)
import Backend.PrenatalEncounter.Model exposing (ClinicalProgressReportInitiator(..), RecordPreganancyInitiator(..))
import Backend.PrenatalEncounter.Utils exposing (..)
import Pages.Page exposing (..)
import PrenatalActivity.Model exposing (PrenatalActivity)
import PrenatalActivity.Utils
import Restful.Endpoint exposing (EntityUuid, fromEntityUuid, toEntityUuid)
import Url
import Url.Parser as Parser exposing ((</>), Parser, custom, int, map, oneOf, s, string, top)


activePageByUrl : Url.Url -> Page
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

        PageNotFound url ->
            -- If we couldn't interpret the URL, we don't try to change it.
            Nothing

        ServiceWorkerPage ->
            Just "deployment"

        -- These are pages that required a logged-in user
        UserPage userPage ->
            case userPage of
                ClinicalPage ->
                    Just "clinical"

                ClinicsPage clinicId ->
                    let
                        clinic =
                            clinicId
                                |> Maybe.map (\id -> "/" ++ fromEntityUuid id)
                                |> Maybe.withDefault ""
                    in
                    Just ("clinics" ++ clinic)

                DashboardPage subPage ->
                    let
                        url =
                            case subPage of
                                MainPage ->
                                    "main"

                                StatsPage ->
                                    "stats"

                                CaseManagementPage ->
                                    "case-management"
                    in
                    Just ("dashboard/" ++ url)

                GlobalCaseManagementPage ->
                    Just "case-management"

                ClinicalProgressReportPage initiator prenatalEncounterId ->
                    Just <| "clinical-progress-report/" ++ fromEntityUuid prenatalEncounterId ++ "/" ++ progressReportInitiatorToUrlFragmemt initiator

                DemographicsReportPage prenatalEncounterId ->
                    Just <| "demographics-report/" ++ fromEntityUuid prenatalEncounterId

                MyAccountPage ->
                    Just "my-account"

                CreatePersonPage relationId initiator ->
                    let
                        fragment =
                            initiatorToUrlFragmemt initiator

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
                            initiatorToUrlFragmemt initiator
                    in
                    Just ("person/" ++ fromEntityUuid id ++ "/" ++ fragment)

                PersonsPage related initiator ->
                    let
                        fragment =
                            initiatorToUrlFragmemt initiator

                        url =
                            case related of
                                Nothing ->
                                    "persons"

                                Just relatedId ->
                                    "relations/" ++ fromEntityUuid relatedId
                    in
                    url ++ "/" ++ fragment |> Just

                PrenatalParticipantPage id ->
                    Just <| "prenatal-participant/" ++ fromEntityUuid id

                NutritionParticipantPage id ->
                    Just <| "nutrition-participant/" ++ fromEntityUuid id

                AcuteIllnessParticipantPage id ->
                    Just <| "acute-illness-participant/" ++ fromEntityUuid id

                IndividualEncounterParticipantsPage encounterType ->
                    Just <| "individual-participants/" ++ encodeIndividualEncounterTypeAsString encounterType

                RelationshipPage id1 id2 initiator ->
                    let
                        fragment =
                            initiatorToUrlFragmemt initiator
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
                    Just <| "prenatal-activity/" ++ fromEntityUuid id ++ "/" ++ PrenatalActivity.Utils.encodeActivityAsString activity

                IndividualEncounterTypesPage ->
                    Just "individual-encounter-types/"

                PregnancyOutcomePage initiator id ->
                    Just <| "pregnancy-outcome/" ++ fromEntityUuid id ++ "/" ++ recordPreganancyInitiatorToUrlFragmemt initiator

                NutritionEncounterPage id ->
                    Just <| "nutrition-encounter/" ++ fromEntityUuid id

                NutritionActivityPage id activity ->
                    Just <| "nutrition-activity/" ++ fromEntityUuid id ++ "/" ++ Backend.NutritionActivity.Utils.encodeActivityAsString activity

                NutritionProgressReportPage encounterId ->
                    Just <| "nutrition-progress-report/" ++ fromEntityUuid encounterId

                AcuteIllnessEncounterPage id ->
                    Just <| "acute-illness-encounter/" ++ fromEntityUuid id

                AcuteIllnessActivityPage id activity ->
                    Just <| "acute-illness-activity/" ++ fromEntityUuid id ++ "/" ++ AcuteIllnessActivity.Utils.encodeActivityAsString activity

                AcuteIllnessProgressReportPage id ->
                    Just <| "acute-illness-progress-report/" ++ fromEntityUuid id

                AcuteIllnessOutcomePage id ->
                    Just <| "acute-illness-outcome/" ++ fromEntityUuid id

                HomeVisitEncounterPage id ->
                    Just <| "home-visit-encounter/" ++ fromEntityUuid id

                HomeVisitActivityPage id activity ->
                    Just <| "home-visit-activity/" ++ fromEntityUuid id ++ "/" ++ Backend.HomeVisitActivity.Utils.encodeActivityAsString activity


parser : Parser (Page -> c) c
parser =
    oneOf
        [ map (UserPage << ClinicsPage << Just) (s "clinics" </> parseUuid)
        , map (UserPage (ClinicsPage Nothing)) (s "clinics")
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
        , map (\id -> UserPage <| PrenatalParticipantPage id) (s "prenatal-participant" </> parseUuid)
        , map (\id -> UserPage <| NutritionParticipantPage id) (s "nutrition-participant" </> parseUuid)
        , map (\id -> UserPage <| AcuteIllnessParticipantPage id) (s "acute-illness-participant" </> parseUuid)
        , map (\id1 id2 origin -> UserPage <| RelationshipPage id1 id2 origin) (s "relationship" </> parseUuid </> parseUuid </> parseOrigin)
        , map (\id -> UserPage <| PrenatalEncounterPage id) (s "prenatal-encounter" </> parseUuid)
        , map (\id activity -> UserPage <| PrenatalActivityPage id activity) (s "prenatal-activity" </> parseUuid </> parsePrenatalActivity)
        , map (\id initiator -> UserPage <| ClinicalProgressReportPage initiator id) (s "clinical-progress-report" </> parseUuid </> parseClinicalProgressReportInitiator)
        , map (\id -> UserPage <| DemographicsReportPage id) (s "demographics-report" </> parseUuid)
        , map (UserPage <| IndividualEncounterTypesPage) (s "individual-encounter-types")
        , map (\encounterType -> UserPage <| IndividualEncounterParticipantsPage encounterType) (s "individual-participants" </> parseIndividualEncounterType)
        , map (\id initiator -> UserPage <| PregnancyOutcomePage initiator id) (s "pregnancy-outcome" </> parseUuid </> parseRecordPreganancyInitiator)
        , map (\id -> UserPage <| NutritionEncounterPage id) (s "nutrition-encounter" </> parseUuid)
        , map (\id activity -> UserPage <| NutritionActivityPage id activity) (s "nutrition-activity" </> parseUuid </> parseNutritionActivity)
        , map (\id -> UserPage <| NutritionProgressReportPage id) (s "nutrition-progress-report" </> parseUuid)
        , map (\id -> UserPage <| AcuteIllnessEncounterPage id) (s "acute-illness-encounter" </> parseUuid)
        , map (\id activity -> UserPage <| AcuteIllnessActivityPage id activity) (s "acute-illness-activity" </> parseUuid </> parseAcuteIllnessActivity)
        , map (\id -> UserPage <| AcuteIllnessProgressReportPage id) (s "acute-illness-progress-report" </> parseUuid)
        , map (\id -> UserPage <| AcuteIllnessOutcomePage id) (s "acute-illness-outcome" </> parseUuid)
        , map (\id -> UserPage <| HomeVisitEncounterPage id) (s "home-visit-encounter" </> parseUuid)
        , map (\id activity -> UserPage <| HomeVisitActivityPage id activity) (s "home-visit-activity" </> parseUuid </> parseHomeVisitActivity)

        -- `top` represents the page without any segements ... i.e. the root page.
        , map PinCodePage top
        ]


parseDashboardPage : Parser (DashboardPage -> c) c
parseDashboardPage =
    oneOf
        [ map MainPage (s "main")
        , map StatsPage (s "stats")
        , map CaseManagementPage (s "case-management")
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
    custom "PrenatalActivity" PrenatalActivity.Utils.decodeActivityFromString


parseNutritionActivity : Parser (NutritionActivity -> c) c
parseNutritionActivity =
    custom "NutritionActivity" Backend.NutritionActivity.Utils.decodeActivityFromString


parseAcuteIllnessActivity : Parser (AcuteIllnessActivity -> c) c
parseAcuteIllnessActivity =
    custom "AcuteIllnessActivity" AcuteIllnessActivity.Utils.decodeActivityFromString


parseHomeVisitActivity : Parser (HomeVisitActivity -> c) c
parseHomeVisitActivity =
    custom "HomeVisitActivity" Backend.HomeVisitActivity.Utils.decodeActivityFromString


parseIndividualEncounterType : Parser (IndividualEncounterType -> c) c
parseIndividualEncounterType =
    custom "IndividualEncounterType" decodeIndividualEncounterTypeFromString


parseOrigin : Parser (Initiator -> c) c
parseOrigin =
    custom "Initiator" initiatorFromUrlFragmemt


parseRecordPreganancyInitiator : Parser (RecordPreganancyInitiator -> c) c
parseRecordPreganancyInitiator =
    custom "RecordPreganancyInitiator" recordPreganancyInitiatorFromUrlFragmemt


parseClinicalProgressReportInitiator : Parser (ClinicalProgressReportInitiator -> c) c
parseClinicalProgressReportInitiator =
    custom "ClinicalProgressReportInitiator" progressReportInitiatorFromUrlFragmemt
