module Pages.Router exposing (activePageByUrl, pageToFragment)

import Activity.Model exposing (Activity)
import Activity.Utils
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.IndividualEncounterParticipant.Utils exposing (decodeIndividualEncounterTypeFromString, encoudeIndividualEncounterTypeAsString)
import Backend.Person.Model exposing (Initiator(..))
import Backend.Person.Utils exposing (initiatorFromUrlFragmemt, initiatorToUrlFragmemt)
import NutritionActivity.Model exposing (NutritionActivity(..))
import NutritionActivity.Utils
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

                ClinicalProgressReportPage prenatalEncounterId ->
                    Just <| "clinical-progress-report/" ++ fromEntityUuid prenatalEncounterId

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

                IndividualEncounterParticipantsPage encounterType ->
                    Just <| "individual-participants/" ++ encoudeIndividualEncounterTypeAsString encounterType

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

                PregnancyOutcomePage id ->
                    Just <| "pregnancy-outcome/" ++ fromEntityUuid id

                NutritionEncounterPage id ->
                    Just <| "nutrition-encounter/" ++ fromEntityUuid id

                NutritionActivityPage id activity ->
                    Just <| "nutrition-activity/" ++ fromEntityUuid id ++ "/" ++ NutritionActivity.Utils.encodeActivityAsString activity

                NutritionProgressReportPage encounterId ->
                    Just <| "nutrition-progress-report/" ++ fromEntityUuid encounterId


parser : Parser (Page -> c) c
parser =
    oneOf
        [ map (UserPage << ClinicsPage << Just) (s "clinics" </> parseUuid)
        , map (UserPage (ClinicsPage Nothing)) (s "clinics")
        , map (\page -> UserPage <| DashboardPage page) (s "dashboard" </> parseDashboardPage)
        , map DevicePage (s "device")
        , map PinCodePage (s "pincode")
        , map ServiceWorkerPage (s "deployment")
        , map (UserPage MyAccountPage) (s "my-account")
        , map (UserPage ClinicalPage) (s "clinical")
        , map (\id page -> UserPage <| SessionPage id page) (s "session" </> parseUuid </> parseSessionPage)
        , map (\origin -> UserPage <| PersonsPage Nothing origin) (s "persons" </> parseOrigin)
        , map (\id origin -> UserPage <| PersonsPage (Just id) origin) (s "relations" </> parseUuid </> parseOrigin)
        , map (\origin id -> UserPage <| CreatePersonPage (Just id) origin) (s "person" </> parseOrigin </> s "new" </> parseUuid)
        , map (\origin -> UserPage <| CreatePersonPage Nothing origin) (s "person" </> parseOrigin </> s "new")
        , map (\id -> UserPage <| EditPersonPage id) (s "person" </> parseUuid </> s "edit")
        , map (\id origin -> UserPage <| PersonPage id origin) (s "person" </> parseUuid </> parseOrigin)
        , map (\id -> UserPage <| PrenatalParticipantPage id) (s "prenatal-participant" </> parseUuid)
        , map (\id -> UserPage <| NutritionParticipantPage id) (s "nutrition-participant" </> parseUuid)
        , map (\id1 id2 origin -> UserPage <| RelationshipPage id1 id2 origin) (s "relationship" </> parseUuid </> parseUuid </> parseOrigin)
        , map (\id -> UserPage <| PrenatalEncounterPage id) (s "prenatal-encounter" </> parseUuid)
        , map (\id activity -> UserPage <| PrenatalActivityPage id activity) (s "prenatal-activity" </> parseUuid </> parsePrenatalActivity)
        , map (\id -> UserPage <| ClinicalProgressReportPage id) (s "clinical-progress-report" </> parseUuid)
        , map (\id -> UserPage <| DemographicsReportPage id) (s "demographics-report" </> parseUuid)
        , map (UserPage <| IndividualEncounterTypesPage) (s "individual-encounter-types")
        , map (\encounterType -> UserPage <| IndividualEncounterParticipantsPage encounterType) (s "individual-participants" </> parseIndividualEncounterType)
        , map (\id -> UserPage <| PregnancyOutcomePage id) (s "pregnancy-outcome" </> parseUuid)
        , map (\id -> UserPage <| NutritionEncounterPage id) (s "nutrition-encounter" </> parseUuid)
        , map (\id activity -> UserPage <| NutritionActivityPage id activity) (s "nutrition-activity" </> parseUuid </> parseNutritionActivity)
        , map (\id -> UserPage <| NutritionProgressReportPage id) (s "nutrition-progress-report" </> parseUuid)

        -- `top` represents the page without any segements ... i.e. the
        -- root page.
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
    custom "NutritionActivity" NutritionActivity.Utils.decodeActivityFromString


parseIndividualEncounterType : Parser (IndividualEncounterType -> c) c
parseIndividualEncounterType =
    custom "IndividualEncounterType" decodeIndividualEncounterTypeFromString


parseOrigin : Parser (Initiator -> c) c
parseOrigin =
    custom "Initiator" initiatorFromUrlFragmemt
