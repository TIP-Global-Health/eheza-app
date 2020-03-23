module Pages.Router exposing (activePageByUrl, pageToFragment)

import Activity.Model exposing (Activity)
import Activity.Utils
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.IndividualEncounterParticipant.Utils exposing (decodeIndividualEncounterTypeFromString, encoudeIndividualEncounterTypeAsString)
import Backend.Person.Model exposing (RegistrationInitiator(..))
import Backend.Person.Utils exposing (decodeRegistrationInitiatorFromString)
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

                ClinicalProgressReportPage prenatalEncounterId ->
                    Just <| "clinical-progress-report/" ++ fromEntityUuid prenatalEncounterId

                DemographicsReportPage prenatalEncounterId ->
                    Just <| "demographics-report/" ++ fromEntityUuid prenatalEncounterId

                MyAccountPage ->
                    Just "my-account"

                CreatePersonPage relationId initiator ->
                    let
                        origin =
                            case initiator of
                                ParticipantDirectoryOrigin ->
                                    "directory"

                                IndividualEncounterOrigin encounterType ->
                                    case encounterType of
                                        AntenatalEncounter ->
                                            "antenatal"

                                        InmmunizationEncounter ->
                                            "inmmunization"

                                        NutritionEncounter ->
                                            "nutrition"

                        relation =
                            relationId
                                |> Maybe.map (\id -> "/" ++ fromEntityUuid id)
                                |> Maybe.withDefault ""
                    in
                    Just ("person/" ++ origin ++ "/new" ++ relation)

                EditPersonPage id ->
                    Just ("person/" ++ fromEntityUuid id ++ "/edit")

                PersonPage id ->
                    Just ("person/" ++ fromEntityUuid id)

                PersonsPage related ->
                    let
                        url =
                            case related of
                                Nothing ->
                                    "persons"

                                Just relatedId ->
                                    "relations/" ++ fromEntityUuid relatedId
                    in
                    Just url

                PrenatalParticipantPage id ->
                    Just <| "prenatal-participant/" ++ fromEntityUuid id

                NutritionParticipantPage id ->
                    Just <| "nutrition-participant/" ++ fromEntityUuid id

                IndividualEncounterParticipantsPage encounterType ->
                    Just <| "individual-participants/" ++ encoudeIndividualEncounterTypeAsString encounterType

                RelationshipPage id1 id2 ->
                    Just
                        ("relationship/"
                            ++ fromEntityUuid id1
                            ++ "/"
                            ++ fromEntityUuid id2
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
        , map (\id page -> UserPage <| SessionPage id page) (s "session" </> parseUuid </> parseSessionPage)
        , map (UserPage <| PersonsPage Nothing) (s "persons")
        , map (\id -> UserPage <| PersonsPage (Just id)) (s "relations" </> parseUuid)
        , map (\origin id -> UserPage <| CreatePersonPage (Just id) origin) (s "person" </> parseOrigin </> s "new" </> parseUuid)
        , map (\origin -> UserPage <| CreatePersonPage Nothing origin) (s "person" </> parseOrigin </> s "new")
        , map (\id -> UserPage <| EditPersonPage id) (s "person" </> parseUuid </> s "edit")
        , map (\id -> UserPage <| PersonPage id) (s "person" </> parseUuid)
        , map (\id -> UserPage <| PrenatalParticipantPage id) (s "prenatal-participant" </> parseUuid)
        , map (\id -> UserPage <| NutritionParticipantPage id) (s "nutrition-participant" </> parseUuid)
        , map (\id1 id2 -> UserPage <| RelationshipPage id1 id2) (s "relationship" </> parseUuid </> parseUuid)
        , map (\id -> UserPage <| PrenatalEncounterPage id) (s "prenatal-encounter" </> parseUuid)
        , map (\id activity -> UserPage <| PrenatalActivityPage id activity) (s "prenatal-activity" </> parseUuid </> parsePrenatalActivity)
        , map (\id -> UserPage <| ClinicalProgressReportPage id) (s "clinical-progress-report" </> parseUuid)
        , map (\id -> UserPage <| DemographicsReportPage id) (s "demographics-report" </> parseUuid)
        , map (UserPage <| IndividualEncounterTypesPage) (s "individual-encounter-types")
        , map (\encounterType -> UserPage <| IndividualEncounterParticipantsPage encounterType) (s "individual-participants" </> parseIndividualEncounterType)
        , map (\id -> UserPage <| PregnancyOutcomePage id) (s "pregnancy-outcome" </> parseUuid)
        , map (\id -> UserPage <| NutritionEncounterPage id) (s "nutrition-encounter" </> parseUuid)
        , map (\id activity -> UserPage <| NutritionActivityPage id activity) (s "nutrition-activity" </> parseUuid </> parseNutritionActivity)

        -- `top` represents the page without any segements ... i.e. the
        -- root page.
        , map PinCodePage top
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


parseOrigin : Parser (RegistrationInitiator -> c) c
parseOrigin =
    custom "RegistrationInitiator" decodeRegistrationInitiatorFromString
