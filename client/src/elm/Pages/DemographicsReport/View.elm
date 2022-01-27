module Pages.DemographicsReport.View exposing (view, viewHeader, viewItemHeading)

import App.Model exposing (Msg(..))
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.PatientRecord.Model exposing (PatientRecordInitiator(..))
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears, getHealthCenterName)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter, PrenatalProgressReportInitiator(..))
import Backend.Relationship.Model exposing (MyRelatedBy(..))
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (thumbnailImage)
import Utils.WebData exposing (viewWebData)


thumbnailDimensions : { width : Int, height : Int }
thumbnailDimensions =
    { width = 120
    , height = 120
    }


view : Language -> NominalDate -> PersonId -> PrenatalProgressReportInitiator -> ModelIndexedDb -> Html Msg
view language currentDate personId initiator db =
    let
        person =
            Dict.get personId db.people
                |> Maybe.withDefault NotAsked

        header =
            viewHeader language initiator

        content =
            viewWebData language (viewContent language currentDate db personId) identity person
    in
    div [ class "page-report demographics" ] <|
        [ header
        , content
        ]


viewHeader : Language -> PrenatalProgressReportInitiator -> Html Msg
viewHeader language initiator =
    let
        backIcon =
            let
                iconForView goBackPage =
                    span
                        [ class "link-back" ]
                        [ span
                            [ class "icon-back"
                            , onClick <| SetActivePage <| UserPage goBackPage
                            ]
                            []
                        ]
            in
            case initiator of
                InitiatorEncounterPage prenatalEncounterId ->
                    iconForView (PrenatalEncounterPage prenatalEncounterId)

                -- This option is not in use for Demographics report.
                InitiatorNewEncounter _ ->
                    emptyNode

                Backend.PrenatalEncounter.Model.InitiatorPatientRecord patientId ->
                    iconForView (PatientRecordPage InitiatorParticipantDirectory patientId)
    in
    div
        [ class "ui basic segment head" ]
        [ h1 [ class "ui header" ]
            [ text <| translate language Translate.DemographicsReport ]
        , backIcon
        ]


viewContent : Language -> NominalDate -> ModelIndexedDb -> PersonId -> Person -> Html Msg
viewContent language currentDate db personId person =
    div [ class "ui unstackable items" ]
        [ viewPatientInformationPane language currentDate person
        , viewFamilyInformationPane language currentDate db personId person
        , viewAddressInformationPane language currentDate person
        , viewContactInformationPane language currentDate db person
        ]


viewPatientInformationPane : Language -> NominalDate -> Person -> Html Msg
viewPatientInformationPane language currentDate person =
    let
        nationalIdNumber =
            person.nationalIdNumber |> Maybe.withDefault ""

        birthDate =
            person.birthDate
                |> Maybe.map formatDDMMYYYY
                |> Maybe.withDefault ""

        gender =
            translate language (Translate.Gender person.gender)

        age =
            ageInYears currentDate person
                |> Maybe.map (\age_ -> translate language <| Translate.YearsOld age_)
                |> Maybe.withDefault ""

        levelOfEducation =
            person.educationLevel
                |> Maybe.map (\level -> translate language <| Translate.LevelOfEducation level)
                |> Maybe.withDefault ""

        maritalStatus =
            person.maritalStatus
                |> Maybe.map (\status -> translate language <| Translate.MaritalStatus status)
                |> Maybe.withDefault ""

        hivStatus =
            person.hivStatus
                |> Maybe.map (\status -> translate language <| Translate.HIVStatus status)
                |> Maybe.withDefault ""
    in
    div [ class "patient-information" ]
        [ viewItemHeading language Translate.PatientInformation "blue"
        , div [ class "pane-content" ]
            [ div [ class "ui image" ]
                [ thumbnailImage "mother" person.avatarUrl person.name thumbnailDimensions.height thumbnailDimensions.width ]
            , viewLineItem language Translate.FirstName person.firstName
            , viewLineItem language Translate.SecondName person.secondName
            , viewLineItem language Translate.NationalIdNumber nationalIdNumber
            , viewLineItem language Translate.DateOfBirth birthDate
            , viewLineItem language Translate.GenderLabel gender
            , viewLineItem language Translate.AgeWord age
            , viewLineItem language Translate.LevelOfEducationLabel levelOfEducation
            , viewLineItem language Translate.MaritalStatusLabel maritalStatus
            , viewLineItem language Translate.HIVStatusLabel hivStatus
            ]
        ]


viewFamilyInformationPane : Language -> NominalDate -> ModelIndexedDb -> PersonId -> Person -> Html Msg
viewFamilyInformationPane language currentDate db personId person =
    let
        ubudehe =
            person.ubudehe
                |> Maybe.map (Translate.UbudeheNumber >> translate language)
                |> Maybe.withDefault ""

        numberOfChildren =
            person.numberOfChildren
                |> Maybe.map String.fromInt
                |> Maybe.withDefault ""

        children =
            Dict.get personId db.relationshipsByPerson
                |> Maybe.withDefault NotAsked
                |> RemoteData.map
                    (Dict.values
                        >> List.filter (\relationship -> relationship.relatedBy == MyChild)
                        >> List.filterMap (\relationship -> Dict.get relationship.relatedTo db.people)
                    )
                |> RemoteData.withDefault []

        childrenView =
            if List.isEmpty children then
                viewLineItem language Translate.Children (translate language Translate.NoChildrenRegisteredInTheSystem)

            else
                children
                    |> List.map
                        (RemoteData.map
                            (\child ->
                                p [ class "row" ]
                                    [ span [ class "value first" ] [ text child.name ]
                                    , span [ class "value" ] [ child.nationalIdNumber |> Maybe.withDefault " --- " |> text ]
                                    ]
                            )
                            >> RemoteData.withDefault emptyNode
                        )
                    |> List.append
                        [ p [ class "thead" ]
                            [ span [ class "label first" ] [ text <| translate language Translate.ChildrenNames ]
                            , span [ class "label" ] [ text <| translate language Translate.ChildrenNationalId ]
                            ]
                        ]
                    |> div [ class "children-table" ]
    in
    div [ class "family-information" ]
        [ viewItemHeading language Translate.FamilyInformation "blue"
        , div [ class "pane-content" ]
            [ viewLineItem language Translate.FamilyUbudehe ubudehe
            , viewLineItem language Translate.NumberOfChildrenUnder5 numberOfChildren
            , childrenView
            ]
        ]


viewAddressInformationPane : Language -> NominalDate -> Person -> Html Msg
viewAddressInformationPane language currentDate person =
    div [ class "address-information" ]
        [ viewItemHeading language Translate.AddressInformation "blue"
        , div [ class "pane-content" ]
            [ viewLineItem language Translate.Province (person.province |> Maybe.withDefault "")
            , viewLineItem language Translate.District (person.district |> Maybe.withDefault "")
            , viewLineItem language Translate.Sector (person.sector |> Maybe.withDefault "")
            , viewLineItem language Translate.Cell (person.cell |> Maybe.withDefault "")
            , viewLineItem language Translate.Village (person.village |> Maybe.withDefault "")
            ]
        ]


viewContactInformationPane : Language -> NominalDate -> ModelIndexedDb -> Person -> Html Msg
viewContactInformationPane language currentDate db person =
    let
        healthCenterName =
            getHealthCenterName person.healthCenterId db
                |> Maybe.withDefault ""
    in
    div [ class "contact-information" ]
        [ viewItemHeading language Translate.ContactInformation "blue"
        , div [ class "pane-content" ]
            [ viewLineItem language Translate.TelephoneNumber (person.telephoneNumber |> Maybe.withDefault "")
            , viewLineItem language Translate.HealthCenter healthCenterName
            ]
        ]


viewItemHeading : Language -> TranslationId -> String -> Html any
viewItemHeading language label color =
    div [ class <| "pane-heading " ++ color ]
        [ text <| translate language label ]


viewLineItem : Language -> TranslationId -> String -> Html Msg
viewLineItem language label value =
    p []
        [ span [ class "label" ] [ text <| translate language label ]
        , span [ class "value" ] [ text value ]
        ]
