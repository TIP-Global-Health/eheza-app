module Pages.GlobalCaseManagement.View exposing
    ( generateAcuteIllnessFollowUpEntries
    , generateNutritionFollowUpEntries
    , generatePrenatalFollowUpEntries
    , view
    )

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis(..))
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Measurement.Model
    exposing
        ( AcuteIllnessTraceContact
        , FollowUpMeasurements
        , LaboratoryTest(..)
        , LabsResultsReviewState(..)
        , NCDLabsResults
        , NutritionAssessment(..)
        , PrenatalLabsResults
        )
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils
    exposing
        ( getHomeVisitEncountersForParticipant
        , getTuberculosisEncountersForParticipant
        , getWellChildEncountersForParticipant
        )
import Backend.Person.Utils exposing (generateFullName)
import Backend.PrenatalActivity.Model exposing (PrenatalRecurrentActivity(..))
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounterType(..))
import Backend.PrenatalEncounter.Utils exposing (isNurseEncounter)
import Backend.Utils exposing (resolveIndividualParticipantForPerson)
import Backend.Village.Model exposing (Village)
import Backend.Village.Utils exposing (getVillageById)
import Backend.WellChildEncounter.Model exposing (WellChildEncounterType(..))
import Date exposing (Unit(..))
import EverySet
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Maybe exposing (Maybe)
import Maybe.Extra exposing (isNothing)
import Pages.GlobalCaseManagement.Model exposing (..)
import Pages.GlobalCaseManagement.Utils exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Prenatal.Encounter.Utils exposing (getPrenatalEncountersForParticipant)
import Pages.Report.Utils exposing (getAcuteIllnessEncountersForParticipant)
import Pages.Utils exposing (viewBySyncStatus)
import RemoteData exposing (RemoteData(..))
import SyncManager.Model
import Translate exposing (Language, translate, translateText)
import Utils.Html exposing (viewModal)
import Utils.NominalDate exposing (sortEncounterTuplesDesc)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> HealthCenterId -> Maybe VillageId -> Bool -> SyncManager.Model.Model -> ModelIndexedDb -> Model -> Html Msg
view language currentDate healthCenterId villageId isLabTech syncManager db model =
    let
        header =
            div
                [ class "ui basic head segment" ]
                [ h1 [ class "ui header" ]
                    [ translateText language Translate.CaseManagement ]
                , span
                    [ class "link-back"
                    , onClick <| SetActivePage PinCodePage
                    ]
                    [ span [ class "icon-back" ] [] ]
                ]

        followUps =
            Dict.get healthCenterId db.followUpMeasurements
                |> Maybe.withDefault NotAsked

        content =
            Maybe.andThen
                (getVillageById db
                    >> Maybe.map
                        (\village ->
                            viewWebData language (viewContentForChw language currentDate village model db) identity followUps
                        )
                )
                villageId
                |> Maybe.withDefault (viewWebData language (viewContentForNurse language currentDate isLabTech model db) identity followUps)
                |> viewBySyncStatus language healthCenterId syncManager.syncInfoAuthorities
    in
    div [ class "wrap wrap-alt-2 page-case-management" ]
        [ header
        , content
        , viewModal <|
            viewEntryPopUp language
                currentDate
                model.dialogState
        ]


viewContentForChw : Language -> NominalDate -> Village -> Model -> ModelIndexedDb -> FollowUpMeasurements -> Html Msg
viewContentForChw language currentDate village model db followUps =
    let
        followUpsForResidents =
            resolveUniquePatientsFromFollowUps currentDate followUps
                |> generateFollowUpsForResidents currentDate village db followUps

        nutritionFollowUps =
            generateNutritionFollowUps currentDate followUpsForResidents
                |> fillPersonName identity db

        nutritionFollowUpsPane =
            viewNutritionPane language currentDate nutritionFollowUps db model

        acuteIllnessFollowUps =
            generateAcuteIllnessFollowUps currentDate db followUpsForResidents
                |> fillPersonName Tuple.second db

        ( tbSuspectAcuteIllnessFollowUps, nonTBSuspectAcuteIllnessFollowUps ) =
            Dict.partition
                (\_ item ->
                    item.value.diagnosis == Just DiagnosisTuberculosisSuspect
                )
                acuteIllnessFollowUps

        acuteIllnessFollowUpsPane =
            viewAcuteIllnessPane language currentDate nonTBSuspectAcuteIllnessFollowUps db model

        prenatalFollowUps =
            generatePrenatalFollowUps currentDate db followUpsForResidents
                |> fillPersonName Tuple.second db

        prenatalFollowUpsPane =
            viewPrenatalPane language currentDate prenatalFollowUps db model

        immunizationFollowUps =
            generateImmunizationFollowUps currentDate followUpsForResidents
                |> fillPersonName identity db

        immunizationFollowUpsPane =
            viewImmunizationPane language currentDate immunizationFollowUps db model

        ( tuberculosisFollowUpsForExisitingParticipants, tuberculosisFollowUpsForNewParticipants ) =
            generateTuberculosisFollowUps currentDate db followUpsForResidents tbSuspectAcuteIllnessFollowUps

        tuberculosisFollowUps =
            fillPersonName Tuple.second db tuberculosisFollowUpsForExisitingParticipants

        tuberculosisFollowUpsPane =
            viewTuberculosisPane language currentDate tuberculosisFollowUps db model

        panes =
            [ ( FilterAcuteIllness, acuteIllnessFollowUpsPane )
            , ( FilterAntenatal, prenatalFollowUpsPane )
            , ( FilterNutrition, nutritionFollowUpsPane )
            , ( FilterImmunization, immunizationFollowUpsPane )
            , ( FilterTuberculosis, tuberculosisFollowUpsPane )
            ]
                |> List.filterMap
                    (\( type_, pane ) ->
                        if isNothing model.filter || model.filter == Just type_ then
                            Just pane

                        else
                            Nothing
                    )
    in
    div [ class "ui unstackable items" ] <|
        viewFilters language chwFilters model
            :: panes


viewContentForNurse : Language -> NominalDate -> Bool -> Model -> ModelIndexedDb -> FollowUpMeasurements -> Html Msg
viewContentForNurse language currentDate isLabTech model db followUps =
    let
        ( panes, filters ) =
            let
                prenatalLabsPane =
                    viewPrenatalLabsPane language currentDate isLabTech followUps.prenatalLabs db model
            in
            if isLabTech then
                ( [ ( FilterPrenatalLabs, prenatalLabsPane ) ]
                , labTechFilters
                )

            else
                let
                    contactsTracingPane =
                        viewContactsTracingPane language currentDate followUps.traceContacts db model

                    ncdLabsPane =
                        viewNCDLabsPane language currentDate followUps.ncdLabs db model
                in
                ( [ ( FilterContactsTrace, contactsTracingPane )
                  , ( FilterPrenatalLabs, prenatalLabsPane )
                  , ( FilterNCDLabs, ncdLabsPane )
                  ]
                , nurseFilters
                )

        panesForView =
            List.filterMap
                (\( type_, pane ) ->
                    if isNothing model.filter || model.filter == Just type_ then
                        Just pane

                    else
                        Nothing
                )
                panes
    in
    div [ class "ui unstackable items" ] <|
        viewFilters language filters model
            :: panesForView


viewEntryPopUp : Language -> NominalDate -> Maybe FollowUpEncounterDataType -> Maybe (Html Msg)
viewEntryPopUp language currentDate dialogState =
    dialogState
        |> Maybe.map
            (\dataType ->
                case dataType of
                    FollowUpPrenatal data ->
                        viewStartFollowUpPrenatalEncounterDialog language currentDate data

                    _ ->
                        viewStartFollowUpEncounterDialog language dataType
            )


viewStartFollowUpEncounterDialog : Language -> FollowUpEncounterDataType -> Html Msg
viewStartFollowUpEncounterDialog language dataType =
    let
        startFollowUpDialog encounterType personName =
            div [ class "ui active modal" ]
                [ div [ class "content" ]
                    [ text <| translate language <| Translate.EncounterTypeFollowUpQuestion encounterType
                    , text " "
                    , span [ class "person-name" ] [ text personName ]
                    , text "?"
                    ]
                , div [ class "actions" ]
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
    in
    case dataType of
        FollowUpNutrition data ->
            startFollowUpDialog HomeVisitEncounter data.personName

        FollowUpAcuteIllness data ->
            startFollowUpDialog AcuteIllnessEncounter data.personName

        FollowUpImmunization data ->
            startFollowUpDialog WellChildEncounter data.personName

        FollowUpTuberculosis data ->
            startFollowUpDialog TuberculosisEncounter data.personName

        -- We should never get here, since Prenatal got
        -- it's own dialog.
        FollowUpPrenatal _ ->
            emptyNode

        -- This is not a follow up encounter.
        CaseManagementContactsTracing ->
            emptyNode


viewStartFollowUpPrenatalEncounterDialog : Language -> NominalDate -> FollowUpPrenatalData -> Html Msg
viewStartFollowUpPrenatalEncounterDialog language currentDate data =
    let
        ( content, actions ) =
            if data.dateMeasured == currentDate then
                ( [ text <| translate language Translate.CannotStartEncounterLabel
                  , text " "
                  , span [ class "person-name" ] [ text data.personName ]
                  , text "."
                  ]
                , [ div [ class "two ui buttons" ]
                        [ button
                            [ class "ui primary fluid button"
                            , onClick <| SetDialogState Nothing
                            ]
                            [ text <| translate language Translate.Close ]
                        ]
                  ]
                )

            else
                let
                    subsequentEncounterButton =
                        Pages.Prenatal.Encounter.Utils.getSubsequentEncounterType data.encounterType
                            |> Maybe.map
                                (\subsequentEncounterType ->
                                    button
                                        [ class "ui primary fluid stacked button"
                                        , onClick <| StartPrenatalFollowUpEncounter data.participantId data.hasNurseEncounter subsequentEncounterType
                                        ]
                                        [ text <| translate language Translate.SubsequentEncounter ]
                                )
                            |> Maybe.withDefault emptyNode
                in
                ( [ p []
                        [ text <| translate language <| Translate.EncounterTypeFollowUpQuestion AntenatalEncounter
                        , text " "
                        , span [ class "person-name" ] [ text data.personName ]
                        , text "?"
                        ]
                  , subsequentEncounterButton
                  , button
                        [ class "ui primary fluid stacked button"
                        , onClick <| StartPrenatalFollowUpEncounter data.participantId data.hasNurseEncounter ChwPostpartumEncounter
                        ]
                        [ text <| translate language Translate.PostpartumEncounter ]
                  , button
                        [ class "ui primary fluid stacked button"
                        , onClick <| SetDialogState Nothing
                        ]
                        [ text <| translate language Translate.Cancel ]
                  ]
                , []
                )
    in
    div [ class "ui active modal prenatal-follow-up-popup" ]
        [ div [ class "content" ] content
        , div [ class "actions" ] actions
        ]


viewFilters : Language -> List CaseManagementFilter -> Model -> Html Msg
viewFilters language filters model =
    let
        renderButton maybeFilter =
            let
                label =
                    Maybe.map Translate.CaseManagementFilterLabel maybeFilter
                        |> Maybe.withDefault Translate.All
            in
            button
                [ classList
                    [ ( "active", model.filter == maybeFilter )
                    , ( "primary ui button", True )
                    ]
                , onClick <| SetFilter maybeFilter
                ]
                [ translateText language label ]
    in
    List.map Just filters
        |> List.append [ Nothing ]
        |> List.map renderButton
        |> div [ class "ui segment filters" ]


viewItemHeading : Language -> CaseManagementFilter -> Html Msg
viewItemHeading language filter =
    div [ class "pane-heading" ]
        [ text <| translate language <| Translate.CaseManagementPaneHeader filter ]


viewNutritionPane : Language -> NominalDate -> Dict PersonId NutritionFollowUpItem -> ModelIndexedDb -> Model -> Html Msg
viewNutritionPane language currentDate itemsDict db model =
    let
        limitDate =
            -- Set limit date for tomorrow, so that we
            -- load all available follow ups.
            Date.add Days 1 currentDate

        entries =
            generateNutritionFollowUpEntries language limitDate itemsDict db

        content =
            if List.isEmpty entries then
                [ translateText language Translate.NoMatchesFound ]

            else
                List.map (viewNutritionFollowUpEntry language currentDate) entries
    in
    div [ class "pane" ]
        [ viewItemHeading language FilterNutrition
        , div [ class "pane-content" ] content
        ]


generateNutritionFollowUpEntries : Language -> NominalDate -> Dict PersonId NutritionFollowUpItem -> ModelIndexedDb -> List NutritionFollowUpEntry
generateNutritionFollowUpEntries language limitDate itemsDict db =
    Dict.map (generateNutritionFollowUpEntryData language limitDate db) itemsDict
        |> Dict.values
        |> Maybe.Extra.values


generateNutritionFollowUpEntryData : Language -> NominalDate -> ModelIndexedDb -> PersonId -> NutritionFollowUpItem -> Maybe NutritionFollowUpEntry
generateNutritionFollowUpEntryData language limitDate db personId item =
    let
        lastHomeVisitEncounter =
            resolveIndividualParticipantForPerson personId HomeVisitEncounter db
                |> Maybe.map
                    (getHomeVisitEncountersForParticipant db
                        >> List.map Tuple.second
                        >> List.filter (\encounter -> Date.compare encounter.startDate limitDate == LT)
                    )
                |> Maybe.withDefault []
                -- Sort DESC
                |> List.sortWith (\e1 e2 -> Date.compare e2.startDate e1.startDate)
                |> List.head
    in
    lastHomeVisitEncounter
        |> Maybe.map
            (\encounter ->
                -- Last Home Visit encounter occurred before follow up was scheduled.
                if Date.compare encounter.startDate item.dateMeasured == LT then
                    Just <| NutritionFollowUpEntry personId item

                else
                    Nothing
            )
        |> -- No Home Visit encounter found.
           Maybe.withDefault
            (Just <| NutritionFollowUpEntry personId item)


viewNutritionFollowUpEntry : Language -> NominalDate -> NutritionFollowUpEntry -> Html Msg
viewNutritionFollowUpEntry language currentDate entry =
    let
        item =
            entry.item

        dueOption =
            followUpDueOptionByDate currentDate item.dateMeasured item.value.options

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
                    text <| translate language (Translate.NutritionAssessment assessment) ++ ": " ++ translatedSigns

                _ ->
                    text <| translate language <| Translate.NutritionAssessment assessment

        popupData =
            FollowUpNutrition <| FollowUpNutritionData entry.personId item.personName
    in
    viewFollowUpEntry language dueOption item.personName popupData assessments


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

                DueNextMonth ->
                    "next-month"
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
        limitDate =
            -- Set limit date for tomorrow, so that we
            -- load all available follow ups.
            Date.add Days 1 currentDate

        entries =
            generateAcuteIllnessFollowUpEntries language currentDate limitDate itemsDict db

        content =
            if List.isEmpty entries then
                [ translateText language Translate.NoMatchesFound ]

            else
                List.map (viewAcuteIllnessFollowUpEntry language currentDate) entries
    in
    div [ class "pane" ]
        [ viewItemHeading language FilterAcuteIllness
        , div [ class "pane-content" ]
            content
        ]


generateAcuteIllnessFollowUpEntries :
    Language
    -> NominalDate
    -> NominalDate
    -> Dict ( IndividualEncounterParticipantId, PersonId ) AcuteIllnessFollowUpItem
    -> ModelIndexedDb
    -> List AcuteIllnessFollowUpEntry
generateAcuteIllnessFollowUpEntries language currentDate limitDate itemsDict db =
    Dict.map (generateAcuteIllnessFollowUpEntryData language currentDate limitDate db) itemsDict
        |> Dict.values
        |> Maybe.Extra.values


generateAcuteIllnessFollowUpEntryData :
    Language
    -> NominalDate
    -> NominalDate
    -> ModelIndexedDb
    -> ( IndividualEncounterParticipantId, PersonId )
    -> AcuteIllnessFollowUpItem
    -> Maybe AcuteIllnessFollowUpEntry
generateAcuteIllnessFollowUpEntryData language currentDate limitDate db ( participantId, personId ) item =
    let
        dateConcludedCriteria =
            Dict.get participantId db.individualParticipants
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.andThen .dateConcluded
                |> Maybe.map (\dateConcluded -> Date.compare dateConcluded limitDate)
    in
    if dateConcludedCriteria == Just LT then
        -- Illness was concluded before limit date, so we do not need to follow up on it.
        Nothing

    else
        let
            allEncountersWithIds =
                getAcuteIllnessEncountersForParticipant db participantId
                    |> List.filter (\( _, encounter ) -> Date.compare encounter.startDate limitDate == LT)

            lastEncounterWithId =
                List.head allEncountersWithIds
        in
        lastEncounterWithId
            |> Maybe.andThen
                (\( encounterId, _ ) ->
                    -- The follow up was issued at last encounter for the illness,
                    -- so we know we still need to follow up on that.
                    if item.encounterId == Just encounterId then
                        let
                            allEncounters =
                                List.map Tuple.second allEncountersWithIds

                            diagnosis =
                                -- At TB management feature, we started recording the
                                -- diagnosis on follow up.
                                -- Therefore, we try to resolve it from follow up,
                                -- and fallback to more heavy resolving by running through
                                -- all encounters.
                                Maybe.Extra.or
                                    item.value.diagnosis
                                    (List.filter
                                        -- We filters out encounters that got no diagnosis set,
                                        -- to get most recent diagnosis made for the illness.
                                        (.diagnosis >> (/=) NoAcuteIllnessDiagnosis)
                                        allEncounters
                                        |> List.head
                                        |> Maybe.map .diagnosis
                                    )

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
                            |> Maybe.map (AcuteIllnessFollowUpEntry participantId personId item encounterSequenceNumber)

                    else
                        Nothing
                )


viewAcuteIllnessFollowUpEntry :
    Language
    -> NominalDate
    -> AcuteIllnessFollowUpEntry
    -> Html Msg
viewAcuteIllnessFollowUpEntry language currentDate entry =
    let
        item =
            entry.item

        dueOption =
            followUpDueOptionByDate currentDate item.dateMeasured item.value.options

        assessment =
            [ p [] [ text <| translate language <| Translate.AcuteIllnessDiagnosis entry.diagnosis ] ]

        popupData =
            FollowUpAcuteIllness <| FollowUpAcuteIllnessData entry.personId item.personName entry.participantId entry.newEncounterSequenceNumber
    in
    viewFollowUpEntry language dueOption item.personName popupData assessment


viewPrenatalPane :
    Language
    -> NominalDate
    -> Dict ( IndividualEncounterParticipantId, PersonId ) PrenatalFollowUpItem
    -> ModelIndexedDb
    -> Model
    -> Html Msg
viewPrenatalPane language currentDate itemsDict db model =
    let
        limitDate =
            -- Set limit date for tomorrow, so that we
            -- load all available follow ups.
            Date.add Days 1 currentDate

        entries =
            generatePrenatalFollowUpEntries language currentDate limitDate itemsDict db

        content =
            if List.isEmpty entries then
                [ translateText language Translate.NoMatchesFound ]

            else
                List.map (viewPrenatalFollowUpEntry language currentDate) entries
    in
    div [ class "pane" ]
        [ viewItemHeading language FilterAntenatal
        , div [ class "pane-content" ]
            content
        ]


generatePrenatalFollowUpEntries :
    Language
    -> NominalDate
    -> NominalDate
    -> Dict ( IndividualEncounterParticipantId, PersonId ) PrenatalFollowUpItem
    -> ModelIndexedDb
    -> List PrenatalFollowUpEntry
generatePrenatalFollowUpEntries language currentDate limitDate itemsDict db =
    Dict.map (generatePrenatalFollowUpEntryData language currentDate limitDate db) itemsDict
        |> Dict.values
        |> Maybe.Extra.values


generatePrenatalFollowUpEntryData :
    Language
    -> NominalDate
    -> NominalDate
    -> ModelIndexedDb
    -> ( IndividualEncounterParticipantId, PersonId )
    -> PrenatalFollowUpItem
    -> Maybe PrenatalFollowUpEntry
generatePrenatalFollowUpEntryData language currentDate limitDate db ( participantId, personId ) item =
    let
        dateConcludedCriteria =
            Dict.get participantId db.individualParticipants
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.andThen .dateConcluded
                |> Maybe.map (\dateConcluded -> Date.compare dateConcluded limitDate)
    in
    if dateConcludedCriteria == Just LT then
        -- Pregnancy was concluded before limit date, so we do not need to follow up on it.
        Nothing

    else
        let
            allEncountersWithIds =
                getPrenatalEncountersForParticipant db participantId
                    |> List.filter (\( _, encounter ) -> Date.compare encounter.startDate limitDate == LT)
                    -- Sort DESC
                    |> List.sortWith sortEncounterTuplesDesc

            allChwEncountersWithIds =
                List.filter (Tuple.second >> .encounterType >> isNurseEncounter >> not) allEncountersWithIds
        in
        List.head allChwEncountersWithIds
            |> Maybe.andThen
                (\( encounterId, encounter ) ->
                    -- Follow up belongs to last encounter, which indicates that
                    -- there was no other encounter that has resolved this follow up.
                    if item.encounterId == Just encounterId then
                        if encounter.encounterType == ChwPostpartumEncounter then
                            -- We do not show follow ups taken at Postpartum encounter.
                            Nothing

                        else
                            let
                                hasNurseEncounter =
                                    List.length allChwEncountersWithIds < List.length allEncountersWithIds
                            in
                            PrenatalFollowUpEntry
                                participantId
                                personId
                                item
                                encounter.encounterType
                                hasNurseEncounter
                                |> Just

                    else
                        -- Last encounter has not originated the follow up.
                        -- Therefore, we know that follow up is resolved.
                        Nothing
                )


viewPrenatalFollowUpEntry : Language -> NominalDate -> PrenatalFollowUpEntry -> Html Msg
viewPrenatalFollowUpEntry language currentDate entry =
    let
        item =
            entry.item

        dueOption =
            followUpDueOptionByDate currentDate item.dateMeasured item.value.options

        assessment =
            [ p [] [ text <| translate language <| Translate.PrenatalAssesment item.value.assesment ] ]

        popupData =
            FollowUpPrenatal <|
                FollowUpPrenatalData
                    entry.personId
                    item.personName
                    entry.participantId
                    entry.encounterType
                    entry.hasNurseEncounter
                    item.dateMeasured
    in
    viewFollowUpEntry language dueOption item.personName popupData assessment


viewTuberculosisPane :
    Language
    -> NominalDate
    -> Dict ( IndividualEncounterParticipantId, PersonId ) TuberculosisFollowUpItem
    -> ModelIndexedDb
    -> Model
    -> Html Msg
viewTuberculosisPane language currentDate itemsDict db model =
    let
        limitDate =
            -- Set limit date for tomorrow, so that we
            -- load all available follow ups.
            Date.add Days 1 currentDate

        entries =
            generateTuberculosisFollowUpEntries language currentDate limitDate itemsDict db

        content =
            if List.isEmpty entries then
                [ translateText language Translate.NoMatchesFound ]

            else
                List.map (viewTuberculosisFollowUpEntry language currentDate) entries
    in
    div [ class "pane" ]
        [ viewItemHeading language FilterTuberculosis
        , div [ class "pane-content" ]
            content
        ]


generateTuberculosisFollowUpEntries :
    Language
    -> NominalDate
    -> NominalDate
    -> Dict ( IndividualEncounterParticipantId, PersonId ) TuberculosisFollowUpItem
    -> ModelIndexedDb
    -> List TuberculosisFollowUpEntry
generateTuberculosisFollowUpEntries language currentDate limitDate itemsDict db =
    Dict.map (generateTuberculosisFollowUpEntryData language currentDate limitDate db) itemsDict
        |> Dict.values
        |> Maybe.Extra.values


generateTuberculosisFollowUpEntryData :
    Language
    -> NominalDate
    -> NominalDate
    -> ModelIndexedDb
    -> ( IndividualEncounterParticipantId, PersonId )
    -> TuberculosisFollowUpItem
    -> Maybe TuberculosisFollowUpEntry
generateTuberculosisFollowUpEntryData language currentDate limitDate db ( participantId, personId ) item =
    if item.dateMeasured == currentDate then
        -- We do not display follow ups that were scheduled today,
        -- since we should not allow starting an encounter, if there
        -- was already and encounter completed today (where follow up
        -- was scheduled).
        Nothing

    else
        let
            dateConcludedCriteria =
                Dict.get participantId db.individualParticipants
                    |> Maybe.andThen RemoteData.toMaybe
                    |> Maybe.andThen .dateConcluded
                    |> Maybe.map (\dateConcluded -> Date.compare dateConcluded limitDate)
        in
        if dateConcludedCriteria == Just LT then
            -- Illness was concluded before limit date, so we do not need to follow up on it.
            Nothing

        else
            let
                allEncountersWithIds =
                    getTuberculosisEncountersForParticipant db participantId
                        |> List.filter (\( _, encounter ) -> Date.compare encounter.startDate limitDate == LT)
                        -- Sort DESC
                        |> List.sortWith sortEncounterTuplesDesc
            in
            List.head allEncountersWithIds
                |> Maybe.andThen
                    (\( encounterId, encounter ) ->
                        -- Follow up belongs to last encounter, which indicates that
                        -- there was no other encounter that has resolved this follow up.
                        if item.encounterId == Just encounterId then
                            TuberculosisFollowUpEntry
                                participantId
                                personId
                                item
                                |> Just

                        else
                            -- Last encounter has not originated the follow up.
                            -- Therefore, we know that follow up is resolved.
                            Nothing
                    )


viewTuberculosisFollowUpEntry : Language -> NominalDate -> TuberculosisFollowUpEntry -> Html Msg
viewTuberculosisFollowUpEntry language currentDate entry =
    let
        item =
            entry.item

        dueOption =
            followUpDueOptionByDate currentDate item.dateMeasured item.value.options

        label =
            [ p [] [ text <| translate language Translate.TuberculosisFollowUpLabel ] ]

        popupData =
            FollowUpTuberculosis <|
                FollowUpTuberculosisData
                    entry.personId
                    item.personName
                    entry.participantId
    in
    viewFollowUpEntry language dueOption item.personName popupData label


viewFollowUpEntry :
    Language
    -> FollowUpDueOption
    -> String
    -> FollowUpEncounterDataType
    -> List (Html Msg)
    -> Html Msg
viewFollowUpEntry language dueOption personName popupData assessment =
    let
        dueLabel =
            Translate.FollowUpDueOption dueOption
                |> translateText language

        dueClass =
            viewDueClass dueOption
    in
    div [ class "follow-up-entry" ]
        [ div [ class "name" ] [ text personName ]
        , div [ class dueClass ] [ dueLabel ]
        , div [ class "assesment" ] assessment
        , div
            [ class "icon-forward"
            , onClick <| SetDialogState <| Just popupData
            ]
            []
        ]


viewContactsTracingPane :
    Language
    -> NominalDate
    -> Dict AcuteIllnessTraceContactId AcuteIllnessTraceContact
    -> ModelIndexedDb
    -> Model
    -> Html Msg
viewContactsTracingPane language currentDate itemsDict db model =
    let
        filteredItemsDict =
            Dict.filter
                (\_ item ->
                    let
                        -- Initially, resolution date is set to date on which
                        -- Covid isolation period is completed, which is 11-th
                        -- day after the contact.
                        -- We know that item is not resolved, if resolution
                        -- date is a future date.
                        traceNotCompleted =
                            Date.compare currentDate item.value.resolutionDate == LT

                        -- Follow up is performed every 2 days.
                        -- For example, if we had a follow up yesterday, we
                        -- should not do another one today.
                        -- We'll do one tomorrow.
                        followUpScheduled =
                            Maybe.map
                                (\lastFollowUpDate ->
                                    Date.diff Days lastFollowUpDate currentDate > 1
                                )
                                item.value.lastFollowUpDate
                                |> -- There was no follow up so far, so we want
                                   -- to perform it ASAP.
                                   Maybe.withDefault True
                    in
                    traceNotCompleted && followUpScheduled
                )
                itemsDict

        entries =
            generateContactsTracingEntries language currentDate filteredItemsDict db

        content =
            if List.isEmpty entries then
                [ translateText language Translate.NoMatchesFound ]

            else
                let
                    heading =
                        div [ class "trace-contact-entry heading" ]
                            [ div [ class "name" ] [ translateText language Translate.ContactName ]
                            , div [ class "last-contact" ] [ translateText language Translate.LastContacted ]
                            , div [ class "reporter" ] [ translateText language Translate.IndexPatient ]
                            , div [ class "phone-number" ] [ translateText language Translate.TelephoneNumber ]
                            ]
                in
                heading
                    :: List.map (viewTraceContactEntry language currentDate db) entries
    in
    div [ class "pane" ]
        [ viewItemHeading language FilterContactsTrace
        , div [ class "pane-content" ]
            content
        ]


generateContactsTracingEntries :
    Language
    -> NominalDate
    -> Dict AcuteIllnessTraceContactId AcuteIllnessTraceContact
    -> ModelIndexedDb
    -> List ContactsTracingEntryData
generateContactsTracingEntries language currentDate itemsDict db =
    Dict.map (generateContactsTracingEntryData language currentDate db) itemsDict
        |> Dict.values


generateContactsTracingEntryData :
    Language
    -> NominalDate
    -> ModelIndexedDb
    -> AcuteIllnessTraceContactId
    -> AcuteIllnessTraceContact
    -> ContactsTracingEntryData
generateContactsTracingEntryData language currentDate db itemId item =
    let
        name =
            generateFullName item.value.firstName item.value.secondName

        reporterName =
            Dict.get item.participantId db.people
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .name
                |> Maybe.withDefault ""
    in
    ContactsTracingEntryData itemId name item.value.phoneNumber reporterName item.value.lastFollowUpDate


viewTraceContactEntry :
    Language
    -> NominalDate
    -> ModelIndexedDb
    -> ContactsTracingEntryData
    -> Html Msg
viewTraceContactEntry language currentDate db entry =
    let
        lastContactDate =
            Maybe.map formatDDMMYYYY entry.lastFollowUpDate
                |> Maybe.withDefault ""
    in
    div [ class "trace-contact-entry" ]
        [ div [ class "name" ] [ text entry.personName ]
        , div [ class "last-contact" ] [ text lastContactDate ]
        , div [ class "reporter" ] [ text entry.reporterName ]
        , div [ class "phone-number" ] [ text entry.phoneNumber ]
        , div
            [ class "icon-forward"
            , onClick <| SetActivePage <| UserPage (TraceContactPage entry.itemId)
            ]
            []
        ]


viewPrenatalLabsPane :
    Language
    -> NominalDate
    -> Bool
    -> Dict PrenatalLabsResultsId PrenatalLabsResults
    -> ModelIndexedDb
    -> Model
    -> Html Msg
viewPrenatalLabsPane language currentDate isLabTech itemsDict db model =
    let
        filteredItemsDict =
            Dict.filter
                (\_ item ->
                    let
                        resolutionDateCondition =
                            -- We know that item is not resolved, if resolution
                            -- date is a future date.
                            Date.compare currentDate item.value.resolutionDate == LT

                        roleDependantCondition =
                            if isLabTech then
                                -- If review was requested (by lab technician), or completed
                                -- (by nurse) we do not display entry for lab technician.
                                isNothing item.value.reviewState
                                    && -- If all tests were completed, or only one that was not is
                                       -- vitals recheck, we do not display entry for lab technician.
                                       -- For nurse,all tests were completed condition des not apply,
                                       -- since there maybe follow up quesitons to fill.
                                       (case EverySet.toList item.value.performedTests of
                                            [ TestVitalsRecheck ] ->
                                                False

                                            _ ->
                                                True
                                       )

                            else
                                True
                    in
                    resolutionDateCondition && roleDependantCondition
                )
                itemsDict

        entries =
            generatePrenatalLabsEntries language currentDate isLabTech filteredItemsDict db

        content =
            if List.isEmpty entries then
                [ translateText language Translate.NoMatchesFound ]

            else
                List.map (viewPrenatalLabsEntry language isLabTech) entries
    in
    div [ class "pane" ]
        [ viewItemHeading language FilterPrenatalLabs
        , div [ class "pane-content" ]
            content
        ]


generatePrenatalLabsEntries :
    Language
    -> NominalDate
    -> Bool
    -> Dict PrenatalLabsResultsId PrenatalLabsResults
    -> ModelIndexedDb
    -> List PrenatalLabsEntryData
generatePrenatalLabsEntries language currentDate isLabTech itemsDict db =
    Dict.values itemsDict
        |> List.map (generatePrenatalLabsEntryData language currentDate isLabTech db)
        |> Maybe.Extra.values


generatePrenatalLabsEntryData :
    Language
    -> NominalDate
    -> Bool
    -> ModelIndexedDb
    -> PrenatalLabsResults
    -> Maybe PrenatalLabsEntryData
generatePrenatalLabsEntryData language currentDate isLabTech db item =
    Maybe.map
        (\encounterId ->
            let
                name =
                    Dict.get item.participantId db.people
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map .name
                        |> Maybe.withDefault ""

                state =
                    if Date.diff Days currentDate item.value.resolutionDate < 8 then
                        LabsEntryClosingSoon

                    else if not isLabTech && (item.value.reviewState == Just LabsResultsReviewRequested) then
                        LabsEntryReadyForReview

                    else if not isLabTech && (item.value.reviewState == Just LabsResultsReviewCompleted) then
                        LabsEntryReviewed

                    else
                        LabsEntryPending

                ( performedTests, completedTests ) =
                    labsResultsTestData currentDate item

                label =
                    if
                        EverySet.member TestVitalsRecheck performedTests
                            && (not <| EverySet.member TestVitalsRecheck completedTests)
                    then
                        -- Vitals recheck was scheduled, but not completed yet.
                        Translate.PrenatalLabsCaseManagementEntryTypeVitals

                    else
                        Translate.PrenatalLabsCaseManagementEntryTypeResults
            in
            translate language label
                |> PrenatalLabsEntryData item.participantId name encounterId state
        )
        item.encounterId


viewPrenatalLabsEntry :
    Language
    -> Bool
    -> PrenatalLabsEntryData
    -> Html Msg
viewPrenatalLabsEntry language isLabTech data =
    let
        targetPage =
            if isLabTech then
                PrenatalRecurrentActivityPage data.encounterId LabResults

            else if data.state == LabsEntryReadyForReview then
                ClinicalProgressReportPage (Backend.PrenatalEncounter.Model.InitiatorCaseManagement data.encounterId) data.encounterId

            else
                PrenatalRecurrentEncounterPage data.encounterId
    in
    viewLabsEntry language
        isLabTech
        data.personName
        data.state
        data.label
        targetPage


viewLabsEntry :
    Language
    -> Bool
    -> String
    -> LabsEntryState
    -> String
    -> UserPage
    -> Html Msg
viewLabsEntry language isLabTech personName state label targetPage =
    let
        entryStateClass =
            "due "
                ++ (case state of
                        LabsEntryClosingSoon ->
                            "overdue"

                        LabsEntryPending ->
                            "this-week"

                        LabsEntryReadyForReview ->
                            -- @todo: needs styling?
                            "this-week"

                        LabsEntryReviewed ->
                            -- @todo: needs styling?
                            "this-week"
                   )
    in
    div [ class "follow-up-entry" ]
        [ div [ class "name" ] [ text personName ]
        , div [ class entryStateClass ] [ translateText language <| Translate.LabsEntryState isLabTech state ]
        , div [ class "assesment center" ] [ text label ]
        , div
            [ class "icon-forward"
            , onClick <| SetActivePage <| UserPage targetPage
            ]
            []
        ]


viewNCDLabsPane :
    Language
    -> NominalDate
    -> Dict NCDLabsResultsId NCDLabsResults
    -> ModelIndexedDb
    -> Model
    -> Html Msg
viewNCDLabsPane language currentDate itemsDict db model =
    let
        filteredItemsDict =
            Dict.filter
                (\_ item ->
                    -- We know that item is not resolved, if resolution
                    -- date is a future date.
                    Date.compare currentDate item.value.resolutionDate == LT
                )
                itemsDict

        entries =
            generateNCDLabsEntries language currentDate filteredItemsDict db

        content =
            if List.isEmpty entries then
                [ translateText language Translate.NoMatchesFound ]

            else
                List.map (viewNCDLabsEntry language) entries
    in
    div [ class "pane" ]
        [ viewItemHeading language FilterNCDLabs
        , div [ class "pane-content" ]
            content
        ]


generateNCDLabsEntries :
    Language
    -> NominalDate
    -> Dict NCDLabsResultsId NCDLabsResults
    -> ModelIndexedDb
    -> List NCDLabsEntryData
generateNCDLabsEntries language currentDate itemsDict db =
    Dict.values itemsDict
        |> List.map (generateNCDLabsEntryData language currentDate db)
        |> Maybe.Extra.values


generateNCDLabsEntryData :
    Language
    -> NominalDate
    -> ModelIndexedDb
    -> NCDLabsResults
    -> Maybe NCDLabsEntryData
generateNCDLabsEntryData language currentDate db item =
    Maybe.map
        (\encounterId ->
            let
                name =
                    Dict.get item.participantId db.people
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map .name
                        |> Maybe.withDefault ""

                state =
                    if Date.diff Days currentDate item.value.resolutionDate < 8 then
                        LabsEntryClosingSoon

                    else
                        LabsEntryPending
            in
            translate language Translate.NCDLabsCaseManagementEntryTypeResults
                |> NCDLabsEntryData item.participantId name encounterId state
        )
        item.encounterId


viewNCDLabsEntry :
    Language
    -> NCDLabsEntryData
    -> Html Msg
viewNCDLabsEntry language data =
    viewLabsEntry language
        False
        data.personName
        data.state
        data.label
        (NCDRecurrentEncounterPage data.encounterId)


viewImmunizationPane : Language -> NominalDate -> Dict PersonId ImmunizationFollowUpItem -> ModelIndexedDb -> Model -> Html Msg
viewImmunizationPane language currentDate itemsDict db model =
    let
        entries =
            generateImmunizationFollowUpEntries language currentDate itemsDict db

        content =
            if List.isEmpty entries then
                [ translateText language Translate.NoMatchesFound ]

            else
                List.map (viewImmunizationFollowUpEntry language currentDate) entries
    in
    div [ class "pane" ]
        [ viewItemHeading language FilterImmunization
        , div [ class "pane-content" ] content
        ]


generateImmunizationFollowUpEntries : Language -> NominalDate -> Dict PersonId ImmunizationFollowUpItem -> ModelIndexedDb -> List ImmunizationFollowUpEntry
generateImmunizationFollowUpEntries language limitDate itemsDict db =
    Dict.map (generateImmunizationFollowUpEntryData language limitDate db) itemsDict
        |> Dict.values
        |> Maybe.Extra.values


generateImmunizationFollowUpEntryData : Language -> NominalDate -> ModelIndexedDb -> PersonId -> ImmunizationFollowUpItem -> Maybe ImmunizationFollowUpEntry
generateImmunizationFollowUpEntryData language limitDate db personId item =
    let
        lastWellChildEncounter =
            resolveIndividualParticipantForPerson personId WellChildEncounter db
                |> Maybe.map
                    (getWellChildEncountersForParticipant db
                        >> List.map Tuple.second
                        >> List.filter
                            (\encounter ->
                                (encounter.encounterType /= NewbornExam)
                                    && (Date.compare encounter.startDate limitDate == LT)
                            )
                    )
                |> Maybe.withDefault []
                -- Sort DESC
                |> List.sortWith (\e1 e2 -> Date.compare e2.startDate e1.startDate)
                |> List.head
    in
    Maybe.map
        (\encounter ->
            -- Last Well Child encounter occurred before follow up was scheduled.
            if Date.compare encounter.startDate item.dateMeasured == LT then
                Just <| ImmunizationFollowUpEntry personId item

            else
                Nothing
        )
        lastWellChildEncounter
        |> -- No Home Visit encounter found.
           Maybe.withDefault
            (Just <| ImmunizationFollowUpEntry personId item)


viewImmunizationFollowUpEntry : Language -> NominalDate -> ImmunizationFollowUpEntry -> Html Msg
viewImmunizationFollowUpEntry language currentDate entry =
    let
        item =
            entry.item

        dueOption =
            if diffDays item.dueDate currentDate <= 30 then
                DueThisMonth

            else
                OverDue

        popupData =
            FollowUpImmunization <| FollowUpImmunizationData entry.personId item.personName
    in
    viewFollowUpEntry language
        dueOption
        item.personName
        popupData
        [ p [] [ text <| translate language Translate.ImmunizationFollowUpInstructions ] ]
