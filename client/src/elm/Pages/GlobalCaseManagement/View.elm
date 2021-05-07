module Pages.GlobalCaseManagement.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..))
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Measurement.Model exposing (FollowUpMeasurements, NutritionAssesment(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounterType(..))
import Backend.Utils exposing (resolveIndividualParticipantForPerson)
import Date exposing (Month, Unit(..), isBetween, numberToMonth)
import EverySet
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra
import Maybe exposing (Maybe)
import Maybe.Extra exposing (isJust, isNothing)
import Pages.AcuteIllnessEncounter.Utils exposing (compareAcuteIllnessEncounterDataDesc)
import Pages.GlobalCaseManagement.Model exposing (..)
import Pages.GlobalCaseManagement.Utils exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PageNotFound.View
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, TranslationId, translate, translateText)
import Utils.Html exposing (spinner, viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> ( HealthCenterId, Maybe VillageId ) -> Bool -> Model -> ModelIndexedDb -> Html Msg
view language currentDate ( healthCenterId, maybeVillageId ) isChw model db =
    maybeVillageId
        |> Maybe.map
            (\villageId ->
                let
                    header =
                        div
                            [ class "ui basic head segment" ]
                            [ h1 [ class "ui header" ]
                                [ translateText language Translate.CaseManagement ]
                            , a
                                [ class "link-back"
                                , onClick <| SetActivePage PinCodePage
                                ]
                                [ span [ class "icon-back" ] [] ]
                            ]

                    followUps =
                        Dict.get healthCenterId db.followUpMeasurements
                            |> Maybe.withDefault NotAsked

                    content =
                        viewWebData language (viewContent language currentDate healthCenterId villageId isChw model db) identity followUps
                in
                div [ class "wrap wrap-alt-2 page-case-management" ]
                    [ header
                    , content
                    , viewModal <|
                        viewStartFollowUpEncounterDialog language
                            model.dialogState
                    ]
            )
        |> Maybe.withDefault (Pages.PageNotFound.View.viewPage language (SetActivePage PinCodePage) (UserPage GlobalCaseManagementPage))


viewContent : Language -> NominalDate -> HealthCenterId -> VillageId -> Bool -> Model -> ModelIndexedDb -> FollowUpMeasurements -> Html Msg
viewContent language currentDate healthCenterId villageId isChw model db followUps =
    let
        nutritionFollowUps =
            generateNutritionFollowUps db followUps
                |> filterVillageResidents villageId identity db

        nutritionFollowUpsPane =
            viewNutritionPane language currentDate nutritionFollowUps db model

        acuteIllnessFollowUps =
            generateAcuteIllnessFollowUps db followUps
                |> filterVillageResidents villageId Tuple.second db

        acuteIllnessFollowUpsPane =
            viewAcuteIllnessPane language currentDate acuteIllnessFollowUps db model

        prenatalFollowUps =
            generatePrenatalFollowUps db followUps
                |> filterVillageResidents villageId Tuple.second db

        prenatalFollowUpsPane =
            viewPrenatalPane language currentDate prenatalFollowUps db model

        _ =
            Debug.log "prenatalFollowUps" prenatalFollowUps

        panes =
            [ ( AcuteIllnessEncounter, acuteIllnessFollowUpsPane ), ( AntenatalEncounter, prenatalFollowUpsPane ), ( NutritionEncounter, nutritionFollowUpsPane ) ]
                |> List.filterMap
                    (\( type_, pane ) ->
                        if isNothing model.encounterTypeFilter || model.encounterTypeFilter == Just type_ then
                            Just pane

                        else
                            Nothing
                    )
    in
    div [ class "ui unstackable items" ] <|
        viewFilters language model
            :: panes


viewStartFollowUpEncounterDialog : Language -> Maybe FollowUpEncounterDataType -> Maybe (Html Msg)
viewStartFollowUpEncounterDialog language dialogState =
    dialogState
        |> Maybe.map
            (\dataType ->
                let
                    ( encounterType, personName ) =
                        case dataType of
                            FollowUpNutrition data ->
                                ( HomeVisitEncounter, data.personName )

                            FollowUpAcuteIllness data ->
                                ( AcuteIllnessEncounter, data.personName )

                            FollowUpPrenatal data ->
                                ( AntenatalEncounter, data.personName )
                in
                div [ class "ui tiny active modal" ]
                    [ div
                        [ class "content" ]
                        [ text <| translate language <| Translate.EncounterTypeFollowUpQuestion encounterType
                        , text " "
                        , span [ class "person-name" ] [ text personName ]
                        , text "?"
                        ]
                    , div
                        [ class "actions" ]
                        [ div [ class "two ui buttons" ]
                            [ button
                                [ class "ui primary fluid button"
                                , onClick <| StartFollowUpEncounter dataType
                                ]
                                [ text <| translate language Translate.Yes ]
                            , button
                                [ class "ui fluid button"
                                , onClick <| SetDialogState Nothing
                                ]
                                [ text <| translate language Translate.No ]
                            ]
                        ]
                    ]
            )


viewFilters : Language -> Model -> Html Msg
viewFilters language model =
    let
        filters =
            allEncounterTypes
                |> List.map Just
                |> List.append [ Nothing ]

        renderButton maybeFilter =
            let
                label =
                    Maybe.map Translate.EncounterTypeFileterLabel maybeFilter
                        |> Maybe.withDefault Translate.All
            in
            button
                [ classList
                    [ ( "active", model.encounterTypeFilter == maybeFilter )
                    , ( "primary ui button", True )
                    ]
                , onClick <| SetEncounterTypeFilter maybeFilter
                ]
                [ translateText language label ]
    in
    div [ class "ui segment filters" ] <|
        List.map renderButton filters


viewNutritionPane : Language -> NominalDate -> Dict PersonId NutritionFollowUpItem -> ModelIndexedDb -> Model -> Html Msg
viewNutritionPane language currentDate itemsDict db model =
    let
        entries =
            Dict.map (viewNutritionFollowUpItem language currentDate db) itemsDict

        content =
            if Dict.isEmpty entries then
                [ translateText language Translate.NoMatchesFound ]

            else
                Dict.values entries
    in
    div [ class "pane" ]
        [ viewItemHeading language NutritionEncounter
        , div [ class "pane-content" ] content
        ]


viewItemHeading : Language -> IndividualEncounterType -> Html Msg
viewItemHeading language encounterType =
    div [ class "pane-heading" ]
        [ text <| translate language <| Translate.EncounterTypeFollowUpLabel encounterType ]


viewNutritionFollowUpItem : Language -> NominalDate -> ModelIndexedDb -> PersonId -> NutritionFollowUpItem -> Html Msg
viewNutritionFollowUpItem language currentDate db personId item =
    let
        lastHomeVisitEncounter =
            resolveIndividualParticipantForPerson personId HomeVisitEncounter db
                |> Maybe.map
                    (\participantId ->
                        Dict.get participantId db.homeVisitEncountersByParticipant
                            |> Maybe.andThen RemoteData.toMaybe
                            |> Maybe.map Dict.values
                            |> Maybe.withDefault []
                    )
                |> Maybe.withDefault []
                -- Sort DESC
                |> List.sortWith (\e1 e2 -> Date.compare e2.startDate e1.startDate)
                |> List.head
    in
    lastHomeVisitEncounter
        |> Maybe.map
            (\encounter ->
                -- Last Home Visitit encounter occurred before follow up was scheduled.
                if Date.compare encounter.startDate item.dateMeasured == LT then
                    viewNutritionFollowUpEntry language currentDate personId item

                else
                    emptyNode
            )
        |> -- No Home Visitit encounter found.
           Maybe.withDefault (viewNutritionFollowUpEntry language currentDate personId item)


viewNutritionFollowUpEntry : Language -> NominalDate -> PersonId -> NutritionFollowUpItem -> Html Msg
viewNutritionFollowUpEntry language currentDate personId item =
    let
        dueOption =
            followUpDueOptionByDate currentDate item.dateMeasured item.value.options

        dueLabel =
            Translate.FollowUpDueOption dueOption
                |> translateText language

        dueClass =
            viewDueClass dueOption

        assessments =
            EverySet.toList item.value.assesment
                |> List.reverse
                |> List.map (\assessment -> p [] [ translateAssement assessment ])

        translateAssement assessment =
            case assessment of
                AssesmentMalnutritionSigns signs ->
                    let
                        translatedSigns =
                            List.map (Translate.ChildNutritionSignLabel >> translate language) signs
                                |> String.join ", "
                    in
                    text <| translate language (Translate.NutritionAssesment assessment) ++ ": " ++ translatedSigns

                _ ->
                    text <| translate language <| Translate.NutritionAssesment assessment

        popupData =
            FollowUpNutritionData personId item.personName
    in
    div [ class "follow-up-entry" ]
        [ div [ class "name" ] [ text item.personName ]
        , div [ class dueClass ] [ dueLabel ]
        , div [ class "assesment" ] assessments
        , div
            [ class "icon-forward"
            , onClick <| SetDialogState <| Just <| FollowUpNutrition popupData
            ]
            []
        ]


viewDueClass : FollowUpDueOption -> String
viewDueClass dueOption =
    "due "
        ++ (case dueOption of
                OverDue ->
                    "overdue"

                DueToday ->
                    "today"

                DueThisWeek ->
                    "this-week"

                DueThisMonth ->
                    "this-month"
           )


viewAcuteIllnessPane :
    Language
    -> NominalDate
    -> Dict ( IndividualEncounterParticipantId, PersonId ) AcuteIllnessFollowUpItem
    -> ModelIndexedDb
    -> Model
    -> Html Msg
viewAcuteIllnessPane language currentDate itemsDict db model =
    let
        entries =
            Dict.map (viewAcuteIllnessFollowUpItem language currentDate db) itemsDict

        content =
            if Dict.isEmpty entries then
                [ translateText language Translate.NoMatchesFound ]

            else
                Dict.values entries
    in
    div [ class "pane" ]
        [ viewItemHeading language AcuteIllnessEncounter
        , div [ class "pane-content" ]
            content
        ]


viewAcuteIllnessFollowUpItem : Language -> NominalDate -> ModelIndexedDb -> ( IndividualEncounterParticipantId, PersonId ) -> AcuteIllnessFollowUpItem -> Html Msg
viewAcuteIllnessFollowUpItem language currentDate db ( participantId, personId ) item =
    let
        outcome =
            Dict.get participantId db.individualParticipants
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.andThen .outcome
    in
    if isJust outcome then
        -- Illness was concluded, so we do not need to follow up on it.
        emptyNode

    else
        let
            allEncountersWithIds =
                Dict.get participantId db.acuteIllnessEncountersByParticipant
                    |> Maybe.andThen RemoteData.toMaybe
                    |> Maybe.map Dict.toList
                    |> Maybe.withDefault []
                    -- Sort DESC, by date and sequence number.
                    |> List.sortWith (\( _, e1 ) ( _, e2 ) -> compareAcuteIllnessEncounterDataDesc e1 e2)

            allEncounters =
                List.map Tuple.second allEncountersWithIds

            lastEncounterWithId =
                List.head allEncountersWithIds
        in
        lastEncounterWithId
            |> Maybe.map
                (\( encounterId, encounter ) ->
                    -- The follow up was issued at last encounter for the illness,
                    -- so we know we still need to follow up on that.
                    if item.encounterId == Just encounterId then
                        let
                            diagnosis =
                                allEncounters
                                    |> List.filter
                                        -- We filters out encounters that got no diagnosis set,
                                        -- to get most recent diagnosis made for the illness.
                                        (.diagnosis >> (/=) NoAcuteIllnessDiagnosis)
                                    |> List.head
                                    |> Maybe.map .diagnosis

                            encounterSequenceNumber =
                                allEncounters
                                    |> List.filter (.startDate >> (==) currentDate)
                                    |> List.sortBy .sequenceNumber
                                    |> List.reverse
                                    |> List.head
                                    |> Maybe.map (.sequenceNumber >> (+) 1)
                                    |> Maybe.withDefault 1
                        in
                        diagnosis
                            |> Maybe.map (viewAcuteIllnessFollowUpEntry language currentDate ( participantId, personId ) item encounterSequenceNumber)
                            |> Maybe.withDefault emptyNode

                    else
                        emptyNode
                )
            |> Maybe.withDefault emptyNode


viewAcuteIllnessFollowUpEntry :
    Language
    -> NominalDate
    -> ( IndividualEncounterParticipantId, PersonId )
    -> AcuteIllnessFollowUpItem
    -> Int
    -> AcuteIllnessDiagnosis
    -> Html Msg
viewAcuteIllnessFollowUpEntry language currentDate ( participantId, personId ) item sequenceNumber diagnosis =
    let
        dueOption =
            followUpDueOptionByDate currentDate item.dateMeasured item.value

        dueLabel =
            Translate.FollowUpDueOption dueOption
                |> translateText language

        dueClass =
            viewDueClass dueOption

        popupData =
            FollowUpAcuteIllnessData personId item.personName participantId sequenceNumber
    in
    div [ class "follow-up-entry" ]
        [ div [ class "name" ] [ text item.personName ]
        , div [ class dueClass ] [ dueLabel ]
        , div [ class "assesment" ] [ text <| translate language <| Translate.AcuteIllnessDiagnosis diagnosis ]
        , div
            [ class "icon-forward"
            , onClick <| SetDialogState <| Just <| FollowUpAcuteIllness popupData
            ]
            []
        ]


viewPrenatalPane :
    Language
    -> NominalDate
    -> Dict ( IndividualEncounterParticipantId, PersonId ) PrenatalFollowUpItem
    -> ModelIndexedDb
    -> Model
    -> Html Msg
viewPrenatalPane language currentDate itemsDict db model =
    let
        entries =
            Dict.map (viewPrenatalFollowUpItem language currentDate db) itemsDict

        content =
            if Dict.isEmpty entries then
                [ translateText language Translate.NoMatchesFound ]

            else
                Dict.values entries
    in
    div [ class "pane" ]
        [ viewItemHeading language AntenatalEncounter
        , div [ class "pane-content" ]
            content
        ]


viewPrenatalFollowUpItem : Language -> NominalDate -> ModelIndexedDb -> ( IndividualEncounterParticipantId, PersonId ) -> PrenatalFollowUpItem -> Html Msg
viewPrenatalFollowUpItem language currentDate db ( participantId, personId ) item =
    let
        outcome =
            Dict.get participantId db.individualParticipants
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.andThen .outcome
    in
    if isJust outcome then
        -- Illness was concluded, so we do not need to follow up on it.
        emptyNode

    else
        let
            allEncountersWithIds =
                Dict.get participantId db.prenatalEncountersByParticipant
                    |> Maybe.andThen RemoteData.toMaybe
                    |> Maybe.map Dict.toList
                    |> Maybe.withDefault []

            allEncounters =
                List.map Tuple.second allEncountersWithIds

            lastEncounterWithId =
                List.head allEncountersWithIds
        in
        lastEncounterWithId
            |> Maybe.map
                (\( encounterId, encounter ) ->
                    -- The follow up was issued at last encounter for the illness,
                    -- so we know we still need to follow up on that.
                    if item.encounterId == Just encounterId then
                        let
                            encounterType =
                                allEncounters
                                    |> List.filter
                                        -- We filters out encounters that got no diagnosis set,
                                        -- to get most recent diagnosis made for the illness.
                                        (.encounterType >> (/=) NurseEncounter)
                                    |> List.head
                                    |> Maybe.map .encounterType
                        in
                        encounterType
                            |> Maybe.map (viewPrenatalFollowUpEntry language currentDate ( participantId, personId ) item)
                            |> Maybe.withDefault emptyNode

                    else
                        emptyNode
                )
            |> Maybe.withDefault emptyNode


viewPrenatalFollowUpEntry :
    Language
    -> NominalDate
    -> ( IndividualEncounterParticipantId, PersonId )
    -> PrenatalFollowUpItem
    -> PrenatalEncounterType
    -> Html Msg
viewPrenatalFollowUpEntry language currentDate ( participantId, personId ) item encounterType =
    let
        dueOption =
            followUpDueOptionByDate currentDate item.dateMeasured item.value.options

        dueLabel =
            Translate.FollowUpDueOption dueOption
                |> translateText language

        dueClass =
            viewDueClass dueOption

        popupData =
            FollowUpPrenatalData personId item.personName
    in
    div [ class "follow-up-entry" ]
        [ div [ class "name" ] [ text item.personName ]
        , div [ class dueClass ] [ dueLabel ]
        , div [ class "assesment" ][]
        , div
            [ class "icon-forward"
            , onClick <| SetDialogState <| Just <| FollowUpPrenatal popupData
            ]
            []
        ]
