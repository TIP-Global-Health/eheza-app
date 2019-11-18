module Pages.DemographicsReport.View exposing (view, viewHeader, viewItemHeading)

import App.Model exposing (Msg(..))
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter)
import Backend.PrenatalParticipant.Model exposing (PrenatalParticipant)
import EveryDict
import EveryDictList
import Gizra.NominalDate exposing (NominalDate, formatMMDDYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Participant.View exposing (viewUbudehe)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (thumbnailImage)
import Utils.WebData exposing (viewWebData)


thumbnailDimensions : { width : Int, height : Int }
thumbnailDimensions =
    { width = 120
    , height = 120
    }


type alias FetchedData =
    { encounter : PrenatalEncounter
    , participant : PrenatalParticipant
    , person : Person
    , id : PrenatalEncounterId
    }


view : Language -> NominalDate -> PrenatalEncounterId -> ModelIndexedDb -> Html Msg
view language currentDate prenatalEncounterId db =
    let
        encounter =
            EveryDict.get prenatalEncounterId db.prenatalEncounters
                |> Maybe.withDefault NotAsked

        participant =
            encounter
                |> RemoteData.andThen
                    (\encounter ->
                        EveryDict.get encounter.participant db.prenatalParticipants
                            |> Maybe.withDefault NotAsked
                    )

        person =
            participant
                |> RemoteData.andThen
                    (\participant ->
                        EveryDict.get participant.person db.people
                            |> Maybe.withDefault NotAsked
                    )

        data =
            RemoteData.map FetchedData encounter
                |> RemoteData.andMap participant
                |> RemoteData.andMap person
                |> RemoteData.andMap (Success prenatalEncounterId)

        header =
            viewHeader language prenatalEncounterId Translate.DemographicsReport

        content =
            viewWebData language (viewContent language currentDate db) identity data
    in
    div [ class "page-demographics-report" ] <|
        [ header
        , content
        ]


viewHeader : Language -> PrenatalEncounterId -> TranslationId -> Html Msg
viewHeader language prenatalEncounterId label =
    div
        [ class "ui basic segment head" ]
        [ a
            [ class "icon-back"
            , onClick <| SetActivePage <| UserPage <| PrenatalEncounterPage prenatalEncounterId
            ]
            []
        , h1 [ class "ui header" ]
            [ text <| translate language label ]
        ]


viewContent : Language -> NominalDate -> ModelIndexedDb -> FetchedData -> Html Msg
viewContent language currentDate db data =
    div [ class "ui unstackable items" ]
        [ viewPatientInformationPane language currentDate data
        , viewFamilyInformationPane language currentDate data
        , viewAddressInformationPane language currentDate data
        , viewContactInformationPane language currentDate db data
        ]


viewPatientInformationPane : Language -> NominalDate -> FetchedData -> Html Msg
viewPatientInformationPane language currentDate data =
    let
        nationalIdNumber =
            data.person.nationalIdNumber |> Maybe.withDefault ""

        birthDate =
            data.person.birthDate
                |> Maybe.map formatMMDDYYYY
                |> Maybe.withDefault ""

        gender =
            translate language (Translate.Gender data.person.gender)

        age =
            ageInYears currentDate data.person
                |> Maybe.map (\age_ -> translate language <| Translate.YearsOld age_)
                |> Maybe.withDefault ""

        levelOfEducation =
            data.person.educationLevel
                |> Maybe.map (\level -> translate language <| Translate.LevelOfEducation level)
                |> Maybe.withDefault ""

        maritalStatus =
            data.person.maritalStatus
                |> Maybe.map (\status -> translate language <| Translate.MaritalStatus status)
                |> Maybe.withDefault ""

        hivStatus =
            data.person.hivStatus
                |> Maybe.map (\status -> translate language <| Translate.HIVStatus status)
                |> Maybe.withDefault ""
    in
    div [ class "patient-information" ]
        [ viewItemHeading language Translate.PatientInformation "gray"
        , div [ class "pane-content" ]
            [ div [ class "ui image" ]
                [ thumbnailImage "mother" data.person.avatarUrl data.person.name thumbnailDimensions.height thumbnailDimensions.width ]
            , viewLineItem language Translate.FirstName data.person.firstName
            , viewLineItem language Translate.SecondName data.person.secondName
            , viewLineItem language Translate.NationalIdNumber nationalIdNumber
            , viewLineItem language Translate.DateOfBirth birthDate
            , viewLineItem language Translate.GenderLabel gender
            , viewLineItem language Translate.AgeWord age
            , viewLineItem language Translate.LevelOfEducationLabel levelOfEducation
            , viewLineItem language Translate.MaritalStatusLabel maritalStatus
            , viewLineItem language Translate.HIVStatusLabel hivStatus
            ]
        ]


viewFamilyInformationPane : Language -> NominalDate -> FetchedData -> Html Msg
viewFamilyInformationPane language currentDate data =
    let
        ubudehe =
            data.person.ubudehe
                |> Maybe.map viewUbudehe
                |> Maybe.withDefault ""

        numberOfChildren =
            data.person.numberOfChildren
                |> Maybe.map toString
                |> Maybe.withDefault ""
    in
    div [ class "family-information" ]
        [ viewItemHeading language Translate.FamilyInformation "gray"
        , div [ class "pane-content" ]
            [ viewLineItem language Translate.FamilyUbudehe ubudehe
            , viewLineItem language Translate.NumberOfChildrenUnder5 numberOfChildren
            ]
        ]


viewAddressInformationPane : Language -> NominalDate -> FetchedData -> Html Msg
viewAddressInformationPane language currentDate data =
    div [ class "address-information" ]
        [ viewItemHeading language Translate.AddressInformation "gray"
        , div [ class "pane-content" ]
            [ viewLineItem language Translate.Province (data.person.province |> Maybe.withDefault "")
            , viewLineItem language Translate.District (data.person.district |> Maybe.withDefault "")
            , viewLineItem language Translate.Sector (data.person.sector |> Maybe.withDefault "")
            , viewLineItem language Translate.Cell (data.person.cell |> Maybe.withDefault "")
            , viewLineItem language Translate.Village (data.person.village |> Maybe.withDefault "")
            ]
        ]


viewContactInformationPane : Language -> NominalDate -> ModelIndexedDb -> FetchedData -> Html Msg
viewContactInformationPane language currentDate db data =
    let
        healthCenterName =
            data.person.healthCenterId
                |> Maybe.andThen
                    (\id ->
                        RemoteData.toMaybe db.healthCenters
                            |> Maybe.andThen (EveryDictList.get id)
                    )
                |> Maybe.map .name
                |> Maybe.withDefault ""
    in
    div [ class "contact-information" ]
        [ viewItemHeading language Translate.ContactInformation "gray"
        , div [ class "pane-content" ]
            [ viewLineItem language Translate.TelephoneNumber (data.person.telephoneNumber |> Maybe.withDefault "")
            , viewLineItem language Translate.HealthCenter healthCenterName
            ]
        ]


viewItemHeading : Language -> TranslationId -> String -> Html Msg
viewItemHeading language label color =
    div [ class <| "pane-heading " ++ color ]
        [ text <| translate language label ]


viewLineItem : Language -> TranslationId -> String -> Html Msg
viewLineItem language label value =
    p []
        [ span [ class "label" ] [ text <| translate language label ++ ":" ]
        , span [ class "value" ] [ text value ]
        ]
