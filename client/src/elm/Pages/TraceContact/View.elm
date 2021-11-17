module Pages.TraceContact.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (ContactTraceEntry)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (generateFullName)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.TraceContact.Model exposing (..)
import Pages.Utils exposing (taskCompleted, viewBoolInput, viewCheckBoxSelectInput, viewQuestionLabel, viewSaveAction)
import Pages.WellChildEncounter.View exposing (thumbnailDimensions, viewPersonDetails)
import RemoteData exposing (RemoteData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (thumbnailImage)


view : Language -> NominalDate -> AcuteIllnessTraceContactId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    let
        traceContact =
            Dict.get id db.traceContacts
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .value

        tracePerson =
            Maybe.andThen
                (\contact ->
                    Dict.get contact.personId db.people
                        |> Maybe.andThen RemoteData.toMaybe
                )
                traceContact

        personDetails =
            Maybe.map (viewPersonDetails language currentDate)
                tracePerson
                |> Maybe.withDefault (viewContactDetails language currentDate traceContact)

        traceContentStepForm =
            Maybe.map (viewTraceContactStep language currentDate model)
                traceContact
                |> Maybe.withDefault []

        content =
            div [ class "ui unstackable items" ] <|
                [ div [ class "item" ] personDetails ]
                    ++ traceContentStepForm
    in
    div [ class "page-activity trace-contact" ]
        [ viewHeader language
        , content
        ]


viewHeader : Language -> Html Msg
viewHeader language =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.CovidContactTracing ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage GlobalCaseManagementPage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContactDetails : Language -> NominalDate -> Maybe ContactTraceEntry -> List (Html any)
viewContactDetails language currentDate traceContact =
    Maybe.map
        (\contact ->
            let
                name =
                    generateFullName contact.firstName contact.secondName

                -- genderEntry =
                --     viewEntry Translate.GenderLabel (translate language <| Translate.Gender person.gender)
                --
                -- villageEntry =
                --     Maybe.map (viewEntry Translate.Village) person.village
                --         |> Maybe.withDefault emptyNode
                viewEntry labelTransId content =
                    p []
                        [ span [ class "label" ] [ text <| translate language labelTransId ++ ": " ]
                        , span [] [ text content ]
                        ]
            in
            [ div [ class "ui image" ]
                [ thumbnailImage "mother" Nothing name thumbnailDimensions.height thumbnailDimensions.width ]
            , div [ class "details" ]
                [ h2 [ class "ui header" ]
                    [ text name ]

                -- , genderEntry
                -- , villageEntry
                ]
            ]
        )
        traceContact
        |> Maybe.withDefault []


viewTraceContactStep : Language -> NominalDate -> Model -> ContactTraceEntry -> List (Html Msg)
viewTraceContactStep language currentDate model contact =
    case model.step of
        StepInitiateContact data ->
            viewStepInitiateContact language currentDate contact data

        StepRecordSymptoms ->
            []

        StepReferToHealthCenter ->
            []


viewStepInitiateContact : Language -> NominalDate -> ContactTraceEntry -> StepInitiateContactData -> List (Html Msg)
viewStepInitiateContact language currentDate contact data =
    let
        instructions =
            p [ class "contact-details" ]
                [ text <| translate language Translate.PleaseCall
                , text " "
                , span [ class "blue" ] [ text <| generateFullName contact.firstName contact.secondName ]
                , text " "
                , text <| translate language Translate.At
                , text " "
                , span [ class "blue" ] [ text contact.phoneNumber ]
                ]

        inputs =
            [ viewQuestionLabel language Translate.ContactInitiatedQuesiton
            , viewBoolInput
                language
                data.contactInitiated
                SetContactInitiated
                ""
                Nothing
            ]
                ++ derivedInput

        derivedInput =
            if data.contactInitiated == Just False then
                [ div [ class "why-not" ]
                    [ viewQuestionLabel language Translate.WhyNot
                    , viewCheckBoxSelectInput language
                        [ ReasonNoAnser, ReasonWrongContactInfo, ReasonDeclinedFollowUp ]
                        []
                        data.noContactReason
                        SetNoContactReason
                        Translate.NoContactReason
                    ]
                ]

            else
                []

        ( totalTasks, tasksCompleted ) =
            ( 1 + derivedTaskActive
            , taskCompleted data.contactInitiated + derivedTaskCompleted
            )

        ( derivedTaskActive, derivedTaskCompleted ) =
            if data.contactInitiated == Just False then
                ( 1, taskCompleted data.noContactReason )

            else
                ( 0, 0 )
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            instructions
                :: inputs
                ++ [ viewSaveAction language SaveStepInitiateContact (tasksCompleted /= totalTasks) ]
        ]
    ]



-- div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
--     [ div [ class "ui five column grid" ] <|
--         List.map viewTask tasks
--     ]
