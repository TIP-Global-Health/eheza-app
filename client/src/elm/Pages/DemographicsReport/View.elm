module Pages.DemographicsReport.View exposing (view, viewHeader, viewItemHeading)

import App.Model exposing (Msg(..))
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears, getHealthCenterName)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter)
import Backend.Relationship.Model exposing (MyRelatedBy(..))
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, formatDDMMyyyy)
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
    , participant : IndividualEncounterParticipant
    , person : Person
    , id : PrenatalEncounterId
    }


view : Language -> NominalDate -> PrenatalEncounterId -> ModelIndexedDb -> Html Msg
view language currentDate prenatalEncounterId db =
    let
        encounter =
            Dict.get prenatalEncounterId db.prenatalEncounters
                |> Maybe.withDefault NotAsked

        participant =
            encounter
                |> RemoteData.andThen
                    (\encounter_ ->
                        Dict.get encounter_.participant db.individualParticipants
                            |> Maybe.withDefault NotAsked
                    )

        person =
            participant
                |> RemoteData.andThen
                    (\participant_ ->
                        Dict.get participant_.person db.people
                            |> Maybe.withDefault NotAsked
                    )

        data =
            RemoteData.map FetchedData encounter
                |> RemoteData.andMap participant
                |> RemoteData.andMap person
                |> RemoteData.andMap (Success prenatalEncounterId)

        header =
            viewHeader language prenatalEncounterId Translate.DemographicsReport True

        content =
            viewWebData language (viewContent language currentDate db) identity data
    in
    div [ class "page-demographics-report" ] <|
        [ header
        , content
        ]


viewHeader : Language -> PrenatalEncounterId -> TranslationId -> Bool -> Html Msg
viewHeader language prenatalEncounterId label allowBackAction =
    let
        backIcon =
            if allowBackAction then
                a
                    [ class "icon-back"
                    , onClick <| SetActivePage <| UserPage <| PrenatalEncounterPage prenatalEncounterId
                    ]
                    []

            else
                emptyNode
    in
    div
        [ class "ui basic segment head" ]
        [ backIcon
        , h1 [ class "ui header" ]
            [ text <| translate language label ]
        ]


viewContent : Language -> NominalDate -> ModelIndexedDb -> FetchedData -> Html Msg
viewContent language currentDate db data =
    div [ class "ui unstackable items" ]
        [ viewPatientInformationPane language currentDate data
        , viewFamilyInformationPane language currentDate db data
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
                |> Maybe.map formatDDMMyyyy
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


viewFamilyInformationPane : Language -> NominalDate -> ModelIndexedDb -> FetchedData -> Html Msg
viewFamilyInformationPane language currentDate db data =
    let
        ubudehe =
            data.person.ubudehe
                |> Maybe.map viewUbudehe
                |> Maybe.withDefault ""

        numberOfChildren =
            data.person.numberOfChildren
                |> Maybe.map String.fromInt
                |> Maybe.withDefault ""

        children =
            Dict.get data.participant.person db.relationshipsByPerson
                |> Maybe.withDefault NotAsked
                |> RemoteData.map
                    (Dict.values
                        >> List.filter (\person -> person.relatedBy == MyChild)
                        >> List.filterMap (\person -> Dict.get person.relatedTo db.people)
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
        [ viewItemHeading language Translate.FamilyInformation "gray"
        , div [ class "pane-content" ]
            [ viewLineItem language Translate.FamilyUbudehe ubudehe
            , viewLineItem language Translate.NumberOfChildrenUnder5 numberOfChildren
            , childrenView
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
            getHealthCenterName data.person.healthCenterId db
                |> Maybe.withDefault ""
    in
    div [ class "contact-information" ]
        [ viewItemHeading language Translate.ContactInformation "gray"
        , div [ class "pane-content" ]
            [ viewLineItem language Translate.TelephoneNumber (data.person.telephoneNumber |> Maybe.withDefault "")
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
        [ span [ class "label" ] [ text <| translate language label ++ ":" ]
        , span [ class "value" ] [ text value ]
        ]
