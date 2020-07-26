module Pages.AcuteIllnessProgressReport.View exposing (view)

import App.Model exposing (Msg(..))
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Gender(..), Person)
import Backend.Person.Utils exposing (ageInYears, isPersonAnAdult)
import Date
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, diffMonths, formatDDMMYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra
import Pages.AcuteIllnessActivity.Model exposing (NextStepsTask(..))
import Pages.AcuteIllnessActivity.Utils exposing (resolveAmoxicillinDosage, resolveCoartemDosage, resolveORSDosage, resolveZincDosage)
import Pages.AcuteIllnessActivity.View exposing (viewAdministeredMedicationLabel, viewOralSolutionPrescription, viewSendToHCActionLabel, viewTabletsPrescription)
import Pages.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..), AssembledData)
import Pages.AcuteIllnessEncounter.Utils exposing (generateAssembledData, resolveAcuteIllnessDiagnosis, resolveNextStepByDiagnosis)
import Pages.DemographicsReport.View exposing (viewItemHeading)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (fromEntityUuid)
import Translate exposing (Language, TranslationId, translate)
import Translate.Model exposing (Language(..))
import Utils.Html exposing (thumbnailImage)
import Utils.NominalDate exposing (renderAgeMonthsDays, renderDate)
import Utils.WebData exposing (viewWebData)


thumbnailDimensions : { width : Int, height : Int }
thumbnailDimensions =
    { width = 180
    , height = 180
    }


view : Language -> NominalDate -> AcuteIllnessEncounterId -> ModelIndexedDb -> Html Msg
view language currentDate id db =
    let
        data =
            generateAssembledData id db
    in
    viewWebData language (viewContent language currentDate id) identity data


viewContent : Language -> NominalDate -> AcuteIllnessEncounterId -> AssembledData -> Html Msg
viewContent language currentDate id data =
    let
        diagnosis =
            resolveAcuteIllnessDiagnosis currentDate data.person data.measurements
    in
    div [ class "page-report acute-illness" ]
        [ div
            [ class "ui report unstackable items" ]
            [ viewHeader language currentDate id
            , viewPersonInfo language currentDate data.person data.measurements
            , viewAssessmentPane language currentDate diagnosis
            , viewSymptomsPane language currentDate data.measurements
            , viewPhysicalExamPane language currentDate data.measurements
            , viewActionsTakenPane language currentDate diagnosis data
            ]
        ]


viewHeader : Language -> NominalDate -> AcuteIllnessEncounterId -> Html Msg
viewHeader language currentDate id =
    div [ class "report-header" ]
        [ a
            [ class "icon-back"
            , onClick <| SetActivePage (UserPage (AcuteIllnessEncounterPage id))
            ]
            []
        , h1 [ class "ui report header" ]
            [ text <| translate language Translate.ProgressReport ]
        , p [ class "date" ]
            [ text <| translate language Translate.CurrentIllnessBegan
            , text " - "
            , text <| renderDate language currentDate
            ]
        ]


viewPersonInfo : Language -> NominalDate -> Person -> AcuteIllnessMeasurements -> Html Msg
viewPersonInfo language currentDate person measurements =
    let
        isAdult =
            isPersonAnAdult currentDate person
                |> Maybe.withDefault True

        ( thumbnailClass, maybeAge ) =
            if isAdult then
                ( "mother"
                , ageInYears currentDate person
                    |> Maybe.map (\age -> translate language <| Translate.YearsOld age)
                )

            else
                ( "child"
                , person.birthDate
                    |> Maybe.map
                        (\birthDate -> renderAgeMonthsDays language birthDate currentDate)
                )

        viewAge =
            maybeAge
                |> Maybe.map
                    (\age ->
                        p []
                            [ span [ class "label" ] [ text <| translate language Translate.AgeWord ++ ": " ]
                            , span [] [ text age ]
                            ]
                    )
                |> Maybe.withDefault emptyNode

        viewVillage =
            person.village
                |> Maybe.map
                    (\village ->
                        p []
                            [ span [ class "label" ] [ text <| translate language Translate.Village ++ ": " ]
                            , span [] [ text village ]
                            ]
                    )
                |> Maybe.withDefault emptyNode
    in
    div
        [ class "item person-details" ]
        [ div [ class "ui image" ]
            [ thumbnailImage thumbnailClass person.avatarUrl person.name thumbnailDimensions.height thumbnailDimensions.width
            ]
        , div [ class "content" ]
            [ h2 [ class "ui header" ]
                [ text person.name ]
            , viewAge
            , viewVillage
            ]
        ]


viewAssessmentPane : Language -> NominalDate -> Maybe AcuteIllnessDiagnosis -> Html Msg
viewAssessmentPane language currentDate diagnosis =
    let
        assessment =
            diagnosis
                |> Maybe.map (Translate.AcuteIllnessDiagnosisWarning >> translate language >> text >> List.singleton)
                |> Maybe.withDefault []
    in
    div [ class "pane assessment" ]
        [ viewItemHeading language Translate.Assessment "blue"
        , div [ class "pane-content" ] assessment
        ]


viewSymptomsPane : Language -> NominalDate -> AcuteIllnessMeasurements -> Html Msg
viewSymptomsPane language currentDate measurements =
    let
        symptomsMaxDuration getFunc measurement =
            measurement
                |> Maybe.andThen (Tuple.second >> getFunc >> Dict.values >> List.maximum)
                |> Maybe.withDefault 1

        maxDuration =
            List.maximum
                [ symptomsMaxDuration .value measurements.symptomsGeneral
                , symptomsMaxDuration .value measurements.symptomsRespiratory
                , symptomsMaxDuration (.value >> .signs) measurements.symptomsGI
                ]
                |> Maybe.withDefault 1

        filterSymptoms symptomDuration exclusion dict =
            Dict.toList dict
                |> List.filterMap
                    (\( symptom, count ) ->
                        if symptom /= exclusion && count > symptomDuration then
                            Just symptom

                        else
                            Nothing
                    )

        symptomsGeneral duration =
            measurements.symptomsGeneral
                |> Maybe.map
                    (Tuple.second
                        >> .value
                        >> filterSymptoms duration NoSymptomsGeneral
                        >> List.map (\symptom -> li [ class "general" ] [ text <| translate language (Translate.SymptomsGeneralSign symptom) ])
                    )
                |> Maybe.withDefault []

        symptomsRespiratory duration =
            measurements.symptomsRespiratory
                |> Maybe.map
                    (Tuple.second
                        >> .value
                        >> filterSymptoms duration NoSymptomsRespiratory
                        >> List.map (\symptom -> li [ class "respiratory" ] [ text <| translate language (Translate.SymptomsRespiratorySign symptom) ])
                    )
                |> Maybe.withDefault []

        symptomsGI duration =
            measurements.symptomsGI
                |> Maybe.map
                    (\measurement ->
                        Tuple.second measurement
                            |> .value
                            |> .signs
                            |> filterSymptoms duration NoSymptomsGI
                            |> List.map
                                (\symptom ->
                                    let
                                        translation =
                                            if symptom == Vomiting then
                                                Tuple.second measurement
                                                    |> .value
                                                    |> .derivedSigns
                                                    |> EverySet.member IntractableVomiting
                                                    |> Translate.IntractableVomiting

                                            else
                                                Translate.SymptomsGISignAbbrev symptom
                                    in
                                    li [ class "gi" ] [ text <| translate language translation ]
                                )
                    )
                |> Maybe.withDefault []

        values =
            List.repeat maxDuration currentDate
                |> List.indexedMap
                    (\index date ->
                        ( Date.add Date.Days (-1 * index) date |> formatDDMMYY
                        , symptomsGeneral index ++ symptomsRespiratory index ++ symptomsGI index
                        )
                    )

        symptomsTable =
            values
                |> List.map
                    (\( date, symptoms ) ->
                        div [ class "symptoms-table-row" ]
                            [ div [ class "date" ] [ text date ]
                            , ul [] symptoms
                            ]
                    )
                |> div [ class "symptoms-table" ]
    in
    div [ class "pane symptoms" ]
        [ viewItemHeading language Translate.Symptoms "blue"
        , symptomsTable
        ]


viewPhysicalExamPane : Language -> NominalDate -> AcuteIllnessMeasurements -> Html Msg
viewPhysicalExamPane language currentDate measurements =
    let
        viewBodyTemperatureCell maybeBodyTemperature =
            maybeBodyTemperature
                |> Maybe.map
                    (\bodyTemperature_ ->
                        if bodyTemperature_ < 37.5 then
                            td [] [ text <| "(" ++ (String.toLower <| translate language Translate.Normal) ++ ")" ]

                        else
                            td [ class "alert" ] [ text <| String.fromFloat bodyTemperature_ ++ " " ++ translate language Translate.CelsiusAbbrev ]
                    )
                |> Maybe.withDefault (td [] [])

        viewRespiratoryRateCell maybeRespiratoryRate =
            maybeRespiratoryRate
                |> Maybe.map
                    (\respiratoryRate_ ->
                        if respiratoryRate_ < 20 then
                            td [] [ text <| "(" ++ (String.toLower <| translate language Translate.Normal) ++ ")" ]

                        else
                            td [ class "alert" ] [ text <| String.fromInt respiratoryRate_ ++ " " ++ translate language Translate.BpmUnit ]
                    )
                |> Maybe.withDefault (td [] [])

        bodyTemperature =
            measurements.vitals
                |> Maybe.map (Tuple.second >> .value >> .bodyTemperature)

        respiratoryRate =
            measurements.vitals
                |> Maybe.map (Tuple.second >> .value >> .respiratoryRate)

        values =
            [ ( currentDate, bodyTemperature, respiratoryRate ) ]

        tableHead =
            [ tr []
                [ th [] []
                , th [ class "uppercase" ]
                    [ text <| translate language Translate.Fever ]
                , th [ class "last" ]
                    [ text <| translate language Translate.Tachypnea ]
                ]
            ]

        tableBody =
            values
                |> List.map
                    (\( date, maybeBodyTemperature, maybeRespiratoryRate ) ->
                        tr []
                            [ td [ class "first" ] [ formatDDMMYY date |> text ]
                            , viewBodyTemperatureCell maybeBodyTemperature
                            , viewRespiratoryRateCell maybeRespiratoryRate
                            ]
                    )
    in
    div [ class "pane physical-exam" ]
        [ viewItemHeading language Translate.PhysicalExam "blue"
        , table
            [ class "ui celled table" ]
            [ thead [] tableHead
            , tbody [] tableBody
            ]
        ]


viewActionsTakenPane : Language -> NominalDate -> Maybe AcuteIllnessDiagnosis -> AssembledData -> Html Msg
viewActionsTakenPane language currentDate diagnosis data =
    let
        emptyPane =
            div [ class "pane-content" ] []

        actionsTaken =
            case resolveNextStepByDiagnosis currentDate data.person diagnosis of
                Just NextStepsIsolation ->
                    let
                        contacedHC =
                            data.measurements.hcContact
                                |> Maybe.map
                                    (Tuple.second
                                        >> .value
                                        >> .signs
                                        >> EverySet.member ContactedHealthCenter
                                    )
                                |> Maybe.withDefault False

                        contacedHCAction =
                            if contacedHC then
                                [ viewSendToHCActionLabel language Translate.ContactedHC "icon-phone" (Just currentDate) ]

                            else
                                []

                        patientIsolated =
                            data.measurements.isolation
                                |> Maybe.map
                                    (Tuple.second
                                        >> .value
                                        >> .signs
                                        >> EverySet.member PatientIsolated
                                    )
                                |> Maybe.withDefault False

                        patientIsolatedAction =
                            if patientIsolated then
                                [ viewSendToHCActionLabel language Translate.IsolatedAtHome "icon-patient-in-bed" (Just currentDate) ]

                            else
                                []
                    in
                    if contacedHC || patientIsolated then
                        div [ class "instructions" ] <|
                            (contacedHCAction ++ patientIsolatedAction)

                    else
                        emptyPane

                Just NextStepsMedicationDistribution ->
                    let
                        medicationSigns =
                            Maybe.map (Tuple.second >> .value) data.measurements.medicationDistribution
                    in
                    case diagnosis of
                        Just DiagnosisMalariaUncomplicated ->
                            let
                                coartemPrescribed =
                                    Maybe.map (EverySet.member Coartem) medicationSigns
                                        |> Maybe.withDefault False
                            in
                            if coartemPrescribed then
                                resolveCoartemDosage currentDate data.person
                                    |> Maybe.map
                                        (\dosage ->
                                            div [ class "instructions malaria-uncomplicated" ]
                                                [ viewAdministeredMedicationLabel language Translate.Administered (Translate.MedicationDistributionSign Coartem) "icon-pills" (Just currentDate)
                                                , viewTabletsPrescription language dosage (Translate.ByMouthTwiceADayForXDays 3)
                                                ]
                                        )
                                    |> Maybe.withDefault emptyPane

                            else
                                emptyPane

                        Just DiagnosisGastrointestinalInfectionUncomplicated ->
                            let
                                orsPrescribed =
                                    Maybe.map (EverySet.member ORS) medicationSigns
                                        |> Maybe.withDefault False

                                orsAction =
                                    if orsPrescribed then
                                        Maybe.map
                                            (\dosage ->
                                                [ viewAdministeredMedicationLabel language Translate.Administered (Translate.MedicationDistributionSign ORS) "icon-oral-solution" (Just currentDate)
                                                , viewOralSolutionPrescription language dosage
                                                ]
                                            )
                                            (resolveORSDosage currentDate data.person)
                                            |> Maybe.withDefault []

                                    else
                                        []

                                zincPrescribed =
                                    Maybe.map (EverySet.member Zinc) medicationSigns
                                        |> Maybe.withDefault False

                                zincAction =
                                    if zincPrescribed then
                                        Maybe.map
                                            (\dosage ->
                                                [ viewAdministeredMedicationLabel language Translate.Administered (Translate.MedicationDistributionSign Zinc) "icon-pills" (Just currentDate)
                                                , viewTabletsPrescription language dosage (Translate.ByMouthDaylyForXDays 10)
                                                ]
                                            )
                                            (resolveZincDosage currentDate data.person)
                                            |> Maybe.withDefault []

                                    else
                                        []
                            in
                            if orsPrescribed || zincPrescribed then
                                div [ class "instructions gastrointestinal-uncomplicated" ] <|
                                    (orsAction ++ zincAction)

                            else
                                emptyPane

                        Just DiagnosisSimpleColdAndCough ->
                            div [ class "instructions simple-cough-and-cold" ]
                                [ viewAdministeredMedicationLabel language Translate.Administered (Translate.MedicationDistributionSign LemonJuiceOrHoney) "icon-pills" (Just currentDate) ]

                        Just DiagnosisRespiratoryInfectionUncomplicated ->
                            let
                                amoxicillinPrescribed =
                                    Maybe.map (EverySet.member Amoxicillin) medicationSigns
                                        |> Maybe.withDefault False
                            in
                            if amoxicillinPrescribed then
                                resolveAmoxicillinDosage currentDate data.person
                                    |> Maybe.map
                                        (\dosage ->
                                            div [ class "instructions respiratory-infection-uncomplicated" ]
                                                [ viewAdministeredMedicationLabel language Translate.Administered (Translate.MedicationDistributionSign Amoxicillin) "icon-pills" (Just currentDate)
                                                , viewTabletsPrescription language dosage (Translate.ByMouthTwiceADayForXDays 5)
                                                ]
                                        )
                                    |> Maybe.withDefault emptyPane

                            else
                                emptyPane

                        _ ->
                            emptyPane

                Just NextStepsSendToHC ->
                    let
                        sendToHCSigns =
                            Maybe.map (Tuple.second >> .value) data.measurements.sendToHC

                        completedForm =
                            Maybe.map (EverySet.member HandReferrerForm) sendToHCSigns
                                |> Maybe.withDefault False

                        completedFormAction =
                            if completedForm then
                                [ viewSendToHCActionLabel language Translate.CompletedHCReferralForm "icon-forms" (Just currentDate) ]

                            else
                                []

                        sentToHC =
                            Maybe.map (EverySet.member ReferToHealthCenter) sendToHCSigns
                                |> Maybe.withDefault False

                        sentToHCAction =
                            if sentToHC then
                                [ viewSendToHCActionLabel language Translate.SentPatientToHC "icon-shuttle" (Just currentDate) ]

                            else
                                []
                    in
                    if completedForm || sentToHC then
                        div [ class "instructions" ] <|
                            (completedFormAction ++ sentToHCAction)

                    else
                        emptyPane

                _ ->
                    emptyPane
    in
    div [ class "pane actions-taken" ]
        [ viewItemHeading language Translate.ActionsTaken "blue"
        , actionsTaken
        ]
