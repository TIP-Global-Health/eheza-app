module Pages.Relationship.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Clinic.Model exposing (Clinic, ClinicType(..))
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Initiator(..), Person)
import Backend.Person.Utils exposing (ageInYears, isPersonAnAdult)
import Backend.PmtctParticipant.Model exposing (PmtctParticipant)
import Backend.Relationship.Model exposing (MyRelatedBy(..), MyRelationship, Relationship)
import Backend.Village.Utils exposing (getVillageClinicId)
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isNothing)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Relationship.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (fromEntityUuid)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (thumbnailImage)
import Utils.NominalDate exposing (renderDate)
import Utils.WebData exposing (viewError, viewWebData)


{-| Offer to edit the relationship between these persons, from the point of
view of the first person.
-}
view : Language -> NominalDate -> ( HealthCenterId, Maybe VillageId ) -> Bool -> Initiator -> PersonId -> PersonId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate ( healthCenterId, maybeVillageId ) isChw initiator id1 id2 db model =
    div
        [ class "page-relationship" ]
        [ viewHeader language initiator id1
        , div
            [ class "ui full segment blue" ]
            [ viewContent language currentDate ( healthCenterId, maybeVillageId ) isChw initiator id1 id2 db model ]
        ]


viewHeader : Language -> Initiator -> PersonId -> Html Msg
viewHeader language initiator id1 =
    let
        goBackPage =
            UserPage (PersonPage id1 initiator)
    in
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.CreateRelationship ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage goBackPage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> ( HealthCenterId, Maybe VillageId ) -> Bool -> Initiator -> PersonId -> PersonId -> ModelIndexedDb -> Model -> Html Msg
viewContent language currentDate ( healthCenterId, maybeVillageId ) isChw initiator id1 id2 db model =
    let
        person1 =
            Dict.get id1 db.people
                |> Maybe.withDefault NotAsked

        relationships =
            Dict.get id1 db.relationshipsByPerson
                |> Maybe.withDefault NotAsked

        person2 =
            Dict.get id2 db.people
                |> Maybe.withDefault NotAsked

        request =
            Dict.get id1 db.postRelationship
                |> Maybe.withDefault NotAsked

        participants =
            Dict.get id1 db.participantsByPerson
                |> Maybe.withDefault NotAsked

        clinics =
            db.clinics

        fetched =
            RemoteData.map FetchedData person1
                |> RemoteData.andMap person2
                |> RemoteData.andMap relationships
                |> RemoteData.andMap participants
                |> RemoteData.andMap clinics

        maybeVillageGroupId =
            maybeVillageId
                |> Maybe.andThen (\villageId -> getVillageClinicId villageId db)
    in
    viewWebData language (viewFetchedContent language currentDate healthCenterId maybeVillageGroupId isChw initiator id1 id2 model request) identity fetched


type alias FetchedData =
    { person1 : Person
    , person2 : Person
    , relationships : Dict RelationshipId MyRelationship
    , participants : Dict PmtctParticipantId PmtctParticipant
    , clinics : Dict ClinicId Clinic
    }


viewFetchedContent : Language -> NominalDate -> HealthCenterId -> Maybe ClinicId -> Bool -> Initiator -> PersonId -> PersonId -> Model -> WebData MyRelationship -> FetchedData -> Html Msg
viewFetchedContent language currentDate selectedHealthCenter maybeVillageGroupId isChw initiator id1 id2 model request data =
    let
        savedRelationship =
            data.relationships
                |> Dict.filter (\_ relationship -> relationship.relatedTo == id2)
                |> Dict.toList
                |> List.head
                |> Maybe.map (Tuple.second >> .relatedBy)

        viewedRelationship =
            model.relatedBy
                |> Maybe.Extra.orElse savedRelationship

        possibleRelationships =
            let
                expected =
                    case isPersonAnAdult currentDate data.person1 of
                        Just True ->
                            [ MyChild, MyCaregiven ]

                        Just False ->
                            [ MyParent, MyCaregiver ]

                        Nothing ->
                            [ MyChild, MyCaregiven, MyParent, MyCaregiver ]
            in
            case savedRelationship of
                -- In case we got relationship already,
                -- we do not allow to set another relationship type.
                Just saved ->
                    [ saved ]

                Nothing ->
                    -- Always add the currently set relationship, if there is one, even
                    -- if it's not expected.
                    case viewedRelationship of
                        Just viewed ->
                            if List.member viewed expected then
                                expected

                            else
                                viewed :: expected

                        Nothing ->
                            expected

        relationshipSelector =
            div
                [ class "ui form relationship-selector" ]
                [ div
                    [ class "grouped fields" ]
                    (List.indexedMap showRelationship possibleRelationships)
                ]

        showRelationship index possible =
            let
                isChecked =
                    viewedRelationship == Just possible

                inputId =
                    "input-relationship-" ++ Debug.toString index
            in
            div
                [ class "field" ]
                [ div
                    [ classList
                        [ ( "ui radio checkbox", True )
                        , ( "checked", isChecked )
                        ]
                    ]
                    [ input
                        [ type_ "radio"
                        , id inputId
                        , checked isChecked
                        , classList [ ( "checked", isChecked ) ]
                        , onCheck (always (RelationshipSelected possible))
                        ]
                        []
                    , label
                        [ class "relationship-selection"
                        , for inputId
                        ]
                        [ text <| translate language <| Translate.MyRelatedByQuestion possible ]
                    ]
                ]

        viewGroupSection =
            if isChw then
                emptyNode

            else
                let
                    participants =
                        data.participants
                            |> Dict.filter
                                (\_ participant ->
                                    (participant.child == id1 && participant.adult == id2)
                                        || (participant.adult == id1 && participant.child == id2)
                                )

                    currentGroupsIds =
                        participants
                            |> Dict.values
                            |> List.map .clinic

                    viewCurrentGroups =
                        currentGroupsIds
                            |> List.filterMap
                                (\clinicId ->
                                    Dict.get clinicId data.clinics
                                        |> Maybe.map .name
                                )
                            |> String.join ", "
                            |> text
                            |> List.singleton
                            |> div [ class "current-groups" ]

                    viewGroupSelector =
                        let
                            emptyOption =
                                option
                                    [ value ""
                                    , selected (model.assignToGroup == Nothing)
                                    ]
                                    [ text "" ]

                            selector =
                                data.clinics
                                    |> Dict.filter
                                        (\clinicId clinic ->
                                            -- Clinic is not already selected.
                                            (not <| List.member clinicId currentGroupsIds)
                                                -- It's not a CHW clinic.
                                                && (clinic.clinicType /= Chw)
                                                -- Clinic belongs to selected health center.
                                                && (clinic.healthCenterId == selectedHealthCenter)
                                        )
                                    |> Dict.map
                                        (\clinicId clinic ->
                                            option
                                                [ value (fromEntityUuid clinicId)
                                                , selected (model.assignToGroup == Just clinicId)
                                                ]
                                                [ text clinic.name ]
                                        )
                                    |> Dict.values
                                    |> (::) emptyOption
                                    |> select [ onInput AssignToClinicId ]
                        in
                        div [ class "ui form" ]
                            [ div
                                [ class "inline field" ]
                                [ label [] [ text <| translate language Translate.AddToGroup ]
                                , selector
                                ]
                            ]
                in
                div
                    [ class "ui unstackable items participants-list" ]
                    [ div
                        [ class "ui header" ]
                        [ text <| translate language Translate.Groups ++ ": " ]
                    , viewCurrentGroups
                    , viewGroupSelector
                    ]

        buttons =
            let
                assignToGroup =
                    if isChw then
                        maybeVillageGroupId

                    else
                        model.assignToGroup
            in
            div
                [ class "ui grid save-buttons" ]
                [ div
                    [ class "six wide column" ]
                    [ button
                        [ class "ui button secondary fluid"
                        , onClick <| Reset initiator
                        ]
                        [ text <| translate language Translate.Cancel ]
                    ]
                , div
                    [ class "four wide column" ]
                    []
                , div
                    [ class "six wide column" ]
                    [ button
                        [ classList
                            [ ( "ui button primary fluid", True )
                            , ( "loading", RemoteData.isLoading request )
                            , ( "disabled", RemoteData.isLoading request || isNothing assignToGroup || isNothing viewedRelationship )
                            ]
                        , onClick <| Save viewedRelationship assignToGroup initiator
                        ]
                        [ text <| translate language Translate.Save ]
                    ]
                ]

        requestStatus =
            case request of
                Success _ ->
                    emptyNode

                Failure err ->
                    div
                        [ class "ui warning message" ]
                        [ div [ class "header" ] [ text <| translate language Translate.BackendError ]
                        , viewError language err
                        ]

                Loading ->
                    emptyNode

                NotAsked ->
                    emptyNode
    in
    div [ class "registration-page view" ]
        [ div
            [ class "ui unstackable items participants-list" ]
            [ viewParticipant language currentDate id1 data.person1 ]
        , relationshipSelector
        , div
            [ class "ui unstackable items participants-list" ]
            [ viewParticipant language currentDate id2 data.person2 ]
        , viewGroupSection
        , requestStatus
        , buttons
        ]


viewParticipant : Language -> NominalDate -> PersonId -> Person -> Html Msg
viewParticipant language currentDate id person =
    let
        typeForThumbnail =
            case isPersonAnAdult currentDate person of
                Just True ->
                    "mother"

                Just False ->
                    "child"

                Nothing ->
                    "mother"

        content =
            div [ class "content" ]
                [ div
                    [ class "details" ]
                    [ h2
                        [ class "ui header" ]
                        [ text person.name ]
                    , p []
                        [ label [] [ text <| translate language Translate.DOB ++ ": " ]
                        , span []
                            [ person.birthDate
                                |> Maybe.map (renderDate language >> text)
                                |> showMaybe
                            ]
                        ]
                    , p []
                        [ label [] [ text <| translate language Translate.Village ++ ": " ]
                        , span [] [ person.village |> Maybe.withDefault "" |> text ]
                        ]
                    ]
                ]
    in
    div
        [ class "item participant-view" ]
        [ div
            [ class "ui image" ]
            [ thumbnailImage typeForThumbnail person.avatarUrl person.name 120 120 ]
        , content
        ]
