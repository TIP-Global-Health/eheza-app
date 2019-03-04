module Pages.PatientRegistration.Model exposing
    ( DialogState(..)
    , GeoInfo
    , GeoLocation
    , Model
    , Msg(..)
    , ParticipantsData
    , PatientActionType(..)
    , PatientData(..)
    , RegistrationForm
    , RegistrationPhase(..)
    , RegistrationStep(..)
    , emptyModel
    , initModel
    , validateRegistrationForm
    )

-- import Pages.PatientRegistration.Utils exposing (generateUuid)

import Backend.Child.Model exposing (Child)
import Backend.Entities exposing (GeoLocationId, GeoLocationIdType(..))
import Backend.Measurement.Model exposing (PhotoValue)
import Backend.Mother.Model exposing (ChildrenRelationType(..), Mother)
import Backend.Patient.Model exposing (Gender(..))
import EveryDict exposing (EveryDict)
import Form exposing (Form)
import Form.Error exposing (ErrorValue(..))
import Form.Validate exposing (Validation, andMap, andThen, bool, emptyString, field, format, mapError, oneOf, string, succeed)
import Measurement.Model exposing (DropZoneFile)
import Pages.Page exposing (Page)
import Random.Pcg exposing (initialSeed, step)
import Regex exposing (Regex)
import Restful.Endpoint exposing (EntityId(..), toEntityId)
import Time exposing (Time)
import Time.Date exposing (date)
import Uuid exposing (Uuid, uuidGenerator)


type alias Model =
    { photo : Maybe PhotoValue
    , registrationForm : Form () RegistrationForm
    , registrationPhase : RegistrationPhase
    , previousPhases : List RegistrationPhase
    , participantsData : ParticipantsData
    , relationPatient : Maybe PatientData
    , submittedSearch : Maybe String
    , geoInfo : GeoInfo
    , dialogState : Maybe DialogState
    }


type alias ParticipantsData =
    { mothersToRegister : EveryDict Uuid Mother
    , childrenToRegister : EveryDict Uuid Child
    }


emptyModel : Model
emptyModel =
    { photo = Nothing
    , registrationForm = Form.initial [] validateRegistrationForm
    , registrationPhase = ParticipantSearch Nothing
    , previousPhases = []
    , participantsData = dummyParticipantsData
    , relationPatient = Nothing
    , submittedSearch = Nothing
    , geoInfo = GeoInfo getGeoProvinces getGeoDistricts getGeoSectors getGeoCells getGeoVillages
    , dialogState = Nothing
    }


initModel : Model -> Model
initModel model =
    { emptyModel | participantsData = model.participantsData }


dummyParticipantsData : ParticipantsData
dummyParticipantsData =
    { mothersToRegister = EveryDict.fromList [ ( mother1Uuid, mother1 ), ( mother2Uuid, mother2 ), ( mother3Uuid, mother3 ), ( mother4Uuid, mother4 ) ]
    , childrenToRegister = EveryDict.fromList [ ( child1Uuid, child1 ), ( child2Uuid, child2 ), ( child3Uuid, child3 ), ( child4Uuid, child4 ) ]
    }


emptyParticipantsData : ParticipantsData
emptyParticipantsData =
    { mothersToRegister = EveryDict.empty
    , childrenToRegister = EveryDict.empty
    }


type RegistrationPhase
    = ParticipantSearch (Maybe String)
    | ParticipantRegistration RegistrationStep
    | ParticipantView PatientData


type alias GeoInfo =
    { provinces : EveryDict GeoLocationId GeoLocation
    , districts : EveryDict GeoLocationId GeoLocation
    , sectors : EveryDict GeoLocationId GeoLocation
    , cells : EveryDict GeoLocationId GeoLocation
    , villages : EveryDict GeoLocationId GeoLocation
    }


type RegistrationStep
    = First
    | Second
    | Third


type PatientData
    = PatientMother Uuid Mother
    | PatientChild Uuid Child


type PatientActionType
    = Forward
    | Link


type Msg
    = DropZoneComplete DropZoneFile
    | MakeRelation PatientData
    | MsgRegistrationForm Form.Msg
    | Reset
    | SearchForParticipant String
    | SetActivePage Page
    | SetDialogState (Maybe DialogState)
    | SetRegistrationPhase RegistrationPhase
    | SetRelationPatient (Maybe PatientData)
    | StepBack
    | Submit


type alias GeoLocation =
    { name : String
    , parent : Maybe GeoLocationId
    }


type DialogState
    = ConfirmSubmision
    | SuccessfulRegistration (Maybe PatientData)
    | SuccessfulRelation PatientData


type alias RegistrationForm =
    { firstName : String
    , middleName : String
    , secondName : String
    , nationalIdNumber : String
    , dayOfBirth : String
    , monthOfBirth : String
    , yearOfBirth : String
    , isDateOfBirthEstimated : Bool
    , gender : String
    , levelOfEducation : String
    , profession : String
    , maritalStatus : String
    , hivStatus : String
    , modeOfDelivery : String
    , familyUbudehe : String
    , householdSize : String
    , numberOfChildren : String
    , motherName : String
    , motherNationalId : String
    , fatherName : String
    , fatherNationalId : String
    , caregiverName : String
    , caregiverNationalId : String
    , province : String
    , district : String
    , sector : String
    , cell : String
    , village : String
    , telephoneNumber : String
    , healthCenterName : String
    }


validateRegistrationForm : Validation () RegistrationForm
validateRegistrationForm =
    succeed RegistrationForm
        |> andMap (field "firstName" string)
        |> andMap (field "middleName" string)
        |> andMap (field "secondName" string)
        |> andMap (field "nationalIdNumber" (oneOf [ emptyString, validateAlphanumeric ]))
        |> andMap (field "dayOfBirth" string)
        |> andMap (field "monthOfBirth" string)
        |> andMap (field "yearOfBirth" string)
        |> andMap (field "isDateOfBirthEstimated" bool)
        |> andMap (field "gender" string)
        |> andMap (field "levelOfEducation" string)
        |> andMap (field "profession" string)
        |> andMap (field "maritalStatus" string)
        |> andMap (field "hivStatus" string)
        |> andMap (field "modeOfDelivery" string)
        |> andMap (field "familyUbudehe" string)
        |> andMap (field "householdSize" string)
        |> andMap (field "numberOfChildren" string)
        |> andMap (field "motherName" string)
        |> andMap (field "motherNationalId" (oneOf [ emptyString, validateAlphanumeric ]))
        |> andMap (field "fatherName" string)
        |> andMap (field "fatherNationalId" (oneOf [ emptyString, validateAlphanumeric ]))
        |> andMap (field "caregiverName" string)
        |> andMap (field "caregiverNationalId" (oneOf [ emptyString, validateAlphanumeric ]))
        |> andMap (field "province" string)
        |> andMap (field "district" string)
        |> andMap (field "sector" string)
        |> andMap (field "cell" string)
        |> andMap (field "village" string)
        |> andMap (field "telephoneNumber" string)
        |> andMap (field "healthCenterName" string)


validateAlphanumeric : Validation e String
validateAlphanumeric =
    string
        |> andThen
            (\s ->
                format alphanumericPattern s
                    |> mapError (\_ -> Form.Error.value InvalidFormat)
            )


alphanumericPattern : Regex
alphanumericPattern =
    Regex.regex "^[a-zA-Z0-9]*$"



-- Temporary copy of function from Utils to solve cyclic dependency.


generateUuid : Time -> Uuid
generateUuid currentTime =
    let
        ( uuid, _ ) =
            step uuidGenerator (initialSeed <| round currentTime)
    in
    uuid


child1Uuid : Uuid
child1Uuid =
    generateUuid 21


child2Uuid : Uuid
child2Uuid =
    generateUuid 22


child3Uuid : Uuid
child3Uuid =
    generateUuid 23


child4Uuid : Uuid
child4Uuid =
    generateUuid 24


mother1Uuid : Uuid
mother1Uuid =
    generateUuid 11


mother2Uuid : Uuid
mother2Uuid =
    generateUuid 12


mother3Uuid : Uuid
mother3Uuid =
    generateUuid 13


mother4Uuid : Uuid
mother4Uuid =
    generateUuid 14


child1 : Child
child1 =
    Child "child1 child1"
        "child1"
        Nothing
        "child1"
        Nothing
        Nothing
        Nothing
        (Just mother1Uuid)
        (date 2016 1 1)
        False
        Male
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        (Just
            "Murumbu"
        )
        Nothing
        Nothing


child2 : Child
child2 =
    Child "child2 child2"
        "child2"
        Nothing
        "child2"
        Nothing
        Nothing
        Nothing
        (Just mother1Uuid)
        (date 2014 2 2)
        True
        Female
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        (Just
            "Zurumbu"
        )
        Nothing
        Nothing


child3 : Child
child3 =
    Child "child3 child3"
        "child3"
        Nothing
        "child3"
        Nothing
        Nothing
        Nothing
        (Just mother2Uuid)
        (date 2013 3 3)
        True
        Male
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        (Just
            "Purumbu"
        )
        Nothing
        Nothing


child4 : Child
child4 =
    Child "child4 child4"
        "child4"
        Nothing
        "child4"
        Nothing
        Nothing
        Nothing
        Nothing
        (date 2011 4 4)
        False
        Female
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        (Just
            "Kurumbu"
        )
        Nothing
        Nothing


mother1 : Mother
mother1 =
    Mother "mother1 mother1"
        "mother1"
        Nothing
        "mother1"
        Nothing
        Nothing
        []
        [ child1Uuid, child2Uuid ]
        (Just (date 2001 1 1))
        False
        MotherRelation
        Female
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        (Just
            "Wurumbu"
        )
        Nothing
        Nothing


mother2 : Mother
mother2 =
    Mother "mother2 mother2"
        "mother2"
        Nothing
        "mother2"
        Nothing
        Nothing
        []
        [ child3Uuid ]
        (Just (date 2002 2 2))
        True
        MotherRelation
        Female
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        (Just
            "Durumbu"
        )
        Nothing
        Nothing


mother3 : Mother
mother3 =
    Mother "mother3 mother3"
        "mother3"
        Nothing
        "mother3"
        Nothing
        Nothing
        []
        []
        (Just (date 2003 3 3))
        True
        MotherRelation
        Female
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        (Just
            "Urumbu"
        )
        Nothing
        Nothing


mother4 : Mother
mother4 =
    Mother "mother4 mother4"
        "mother4"
        Nothing
        "mother4"
        Nothing
        Nothing
        []
        []
        (Just (date 2004 4 4))
        False
        MotherRelation
        Male
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        (Just
            "Gurumbu"
        )
        Nothing
        Nothing


getGeoProvinces : EveryDict GeoLocationId GeoLocation
getGeoProvinces =
    EveryDict.fromList
        [ ( toEntityId 1, GeoLocation "Amajyaruguru" Nothing ) ]


getGeoDistricts : EveryDict GeoLocationId GeoLocation
getGeoDistricts =
    EveryDict.fromList
        [ ( toEntityId 2, GeoLocation "Gakenke" (Just <| toEntityId 1) )
        , ( toEntityId 736, GeoLocation "Rulindo" (Just <| toEntityId 1) )
        ]


getGeoSectors : EveryDict GeoLocationId GeoLocation
getGeoSectors =
    EveryDict.fromList
        [ ( toEntityId 3, GeoLocation "Busengo" (Just <| toEntityId 2) )
        , ( toEntityId 49, GeoLocation "Coko" (Just <| toEntityId 2) )
        , ( toEntityId 82, GeoLocation "Cyabingo" (Just <| toEntityId 2) )
        , ( toEntityId 122, GeoLocation "Gakenke" (Just <| toEntityId 2) )
        , ( toEntityId 173, GeoLocation "Gashenyi" (Just <| toEntityId 2) )
        , ( toEntityId 217, GeoLocation "Janja" (Just <| toEntityId 2) )
        , ( toEntityId 248, GeoLocation "Kamubuga" (Just <| toEntityId 2) )
        , ( toEntityId 289, GeoLocation "Karambo" (Just <| toEntityId 2) )
        , ( toEntityId 321, GeoLocation "Kivuruga" (Just <| toEntityId 2) )
        , ( toEntityId 355, GeoLocation "Mataba" (Just <| toEntityId 2) )
        , ( toEntityId 386, GeoLocation "Minazi" (Just <| toEntityId 2) )
        , ( toEntityId 415, GeoLocation "Mugunga" (Just <| toEntityId 2) )
        , ( toEntityId 457, GeoLocation "Muhondo" (Just <| toEntityId 2) )
        , ( toEntityId 507, GeoLocation "Muyongwe" (Just <| toEntityId 2) )
        , ( toEntityId 540, GeoLocation "Muzo" (Just <| toEntityId 2) )
        , ( toEntityId 581, GeoLocation "Nemba" (Just <| toEntityId 2) )
        , ( toEntityId 618, GeoLocation "Ruli" (Just <| toEntityId 2) )
        , ( toEntityId 654, GeoLocation "Rusasa" (Just <| toEntityId 2) )
        , ( toEntityId 691, GeoLocation "Rushashi" (Just <| toEntityId 2) )
        , ( toEntityId 737, GeoLocation "Base" (Just <| toEntityId 736) )
        , ( toEntityId 769, GeoLocation "Burega" (Just <| toEntityId 736) )
        , ( toEntityId 813, GeoLocation "Bushoki" (Just <| toEntityId 736) )
        , ( toEntityId 856, GeoLocation "Buyoga" (Just <| toEntityId 736) )
        , ( toEntityId 901, GeoLocation "Cyinzuzi" (Just <| toEntityId 736) )
        , ( toEntityId 929, GeoLocation "Cyungo" (Just <| toEntityId 736) )
        , ( toEntityId 955, GeoLocation "Kinihira" (Just <| toEntityId 736) )
        , ( toEntityId 983, GeoLocation "Kisaro" (Just <| toEntityId 736) )
        , ( toEntityId 1024, GeoLocation "Masoro" (Just <| toEntityId 736) )
        , ( toEntityId 1056, GeoLocation "Mbogo" (Just <| toEntityId 736) )
        , ( toEntityId 1093, GeoLocation "Murambi" (Just <| toEntityId 736) )
        , ( toEntityId 1115, GeoLocation "Murambi" (Just <| toEntityId 736) )
        , ( toEntityId 1129, GeoLocation "Ngoma" (Just <| toEntityId 736) )
        , ( toEntityId 1162, GeoLocation "Ntarabana" (Just <| toEntityId 736) )
        , ( toEntityId 1189, GeoLocation "Rukozo" (Just <| toEntityId 736) )
        , ( toEntityId 1218, GeoLocation "Rusiga" (Just <| toEntityId 736) )
        , ( toEntityId 1238, GeoLocation "Shyorongi" (Just <| toEntityId 736) )
        , ( toEntityId 1284, GeoLocation "Tumba" (Just <| toEntityId 736) )
        ]


getGeoCells : EveryDict GeoLocationId GeoLocation
getGeoCells =
    EveryDict.fromList
        [ ( toEntityId 4, GeoLocation "Birambo" (Just <| toEntityId 3) )
        , ( toEntityId 9, GeoLocation "Butereri" (Just <| toEntityId 3) )
        , ( toEntityId 17, GeoLocation "Byibuhiro" (Just <| toEntityId 3) )
        , ( toEntityId 23, GeoLocation "Kamina" (Just <| toEntityId 3) )
        , ( toEntityId 30, GeoLocation "Kirabo" (Just <| toEntityId 3) )
        , ( toEntityId 37, GeoLocation "Mwumba" (Just <| toEntityId 3) )
        , ( toEntityId 43, GeoLocation "Ruhanga" (Just <| toEntityId 3) )
        , ( toEntityId 50, GeoLocation "Kiruku" (Just <| toEntityId 49) )
        , ( toEntityId 55, GeoLocation "Mbirima" (Just <| toEntityId 49) )
        , ( toEntityId 64, GeoLocation "Nyange" (Just <| toEntityId 49) )
        , ( toEntityId 72, GeoLocation "Nyanza" (Just <| toEntityId 49) )
        , ( toEntityId 83, GeoLocation "Muhaza" (Just <| toEntityId 82) )
        , ( toEntityId 91, GeoLocation "Muhororo" (Just <| toEntityId 82) )
        , ( toEntityId 100, GeoLocation "Muramba" (Just <| toEntityId 82) )
        , ( toEntityId 107, GeoLocation "Mutanda" (Just <| toEntityId 82) )
        , ( toEntityId 114, GeoLocation "Rukore" (Just <| toEntityId 82) )
        , ( toEntityId 123, GeoLocation "Buheta" (Just <| toEntityId 122) )
        , ( toEntityId 128, GeoLocation "Kagoma" (Just <| toEntityId 122) )
        , ( toEntityId 139, GeoLocation "Nganzo" (Just <| toEntityId 122) )
        , ( toEntityId 153, GeoLocation "Rusagara" (Just <| toEntityId 122) )
        , ( toEntityId 174, GeoLocation "Rutabo" (Just <| toEntityId 173) )
        , ( toEntityId 180, GeoLocation "Rutenderi" (Just <| toEntityId 173) )
        , ( toEntityId 188, GeoLocation "Taba" (Just <| toEntityId 173) )
        , ( toEntityId 194, GeoLocation "Nyacyina" (Just <| toEntityId 173) )
        , ( toEntityId 208, GeoLocation "Rukura" (Just <| toEntityId 173) )
        , ( toEntityId 218, GeoLocation "Gatwa" (Just <| toEntityId 217) )
        , ( toEntityId 224, GeoLocation "Karukungu" (Just <| toEntityId 217) )
        , ( toEntityId 233, GeoLocation "Gakindo" (Just <| toEntityId 217) )
        , ( toEntityId 240, GeoLocation "Gashyamba" (Just <| toEntityId 217) )
        , ( toEntityId 249, GeoLocation "Kamubuga" (Just <| toEntityId 248) )
        , ( toEntityId 262, GeoLocation "Kidomo" (Just <| toEntityId 248) )
        , ( toEntityId 271, GeoLocation "Mbatabata" (Just <| toEntityId 248) )
        , ( toEntityId 280, GeoLocation "Rukore" (Just <| toEntityId 248) )
        , ( toEntityId 290, GeoLocation "Karambo" (Just <| toEntityId 289) )
        , ( toEntityId 304, GeoLocation "Kirebe" (Just <| toEntityId 289) )
        , ( toEntityId 313, GeoLocation "Kanyanza" (Just <| toEntityId 289) )
        , ( toEntityId 322, GeoLocation "Cyintare" (Just <| toEntityId 321) )
        , ( toEntityId 325, GeoLocation "Ruhinga" (Just <| toEntityId 321) )
        , ( toEntityId 332, GeoLocation "Sereri" (Just <| toEntityId 321) )
        , ( toEntityId 341, GeoLocation "Gasiza" (Just <| toEntityId 321) )
        , ( toEntityId 347, GeoLocation "Rugimbu" (Just <| toEntityId 321) )
        , ( toEntityId 356, GeoLocation "Buyange" (Just <| toEntityId 355) )
        , ( toEntityId 366, GeoLocation "Gikombe" (Just <| toEntityId 355) )
        , ( toEntityId 377, GeoLocation "Nyundo" (Just <| toEntityId 355) )
        , ( toEntityId 387, GeoLocation "Gasiho" (Just <| toEntityId 386) )
        , ( toEntityId 395, GeoLocation "Munyana" (Just <| toEntityId 386) )
        , ( toEntityId 401, GeoLocation "Murambi" (Just <| toEntityId 386) )
        , ( toEntityId 407, GeoLocation "Raba" (Just <| toEntityId 386) )
        , ( toEntityId 416, GeoLocation "Munyana" (Just <| toEntityId 415) )
        , ( toEntityId 418, GeoLocation "Mutego" (Just <| toEntityId 415) )
        , ( toEntityId 424, GeoLocation "Nkomane" (Just <| toEntityId 415) )
        , ( toEntityId 430, GeoLocation "Rutabo" (Just <| toEntityId 415) )
        , ( toEntityId 436, GeoLocation "Rutenderi" (Just <| toEntityId 415) )
        , ( toEntityId 441, GeoLocation "Rwamambe" (Just <| toEntityId 415) )
        , ( toEntityId 446, GeoLocation "Gahinga" (Just <| toEntityId 415) )
        , ( toEntityId 458, GeoLocation "Bwenda" (Just <| toEntityId 457) )
        , ( toEntityId 460, GeoLocation "Gasiza" (Just <| toEntityId 457) )
        , ( toEntityId 466, GeoLocation "Gihinga" (Just <| toEntityId 457) )
        , ( toEntityId 472, GeoLocation "Huro" (Just <| toEntityId 457) )
        , ( toEntityId 478, GeoLocation "Musagara" (Just <| toEntityId 457) )
        , ( toEntityId 483, GeoLocation "Musenyi" (Just <| toEntityId 457) )
        , ( toEntityId 488, GeoLocation "Ruganda" (Just <| toEntityId 457) )
        , ( toEntityId 494, GeoLocation "Rwinkuba" (Just <| toEntityId 457) )
        , ( toEntityId 498, GeoLocation "Busake" (Just <| toEntityId 457) )
        , ( toEntityId 508, GeoLocation "Bumba" (Just <| toEntityId 507) )
        , ( toEntityId 516, GeoLocation "Gisiza" (Just <| toEntityId 507) )
        , ( toEntityId 523, GeoLocation "Karyango" (Just <| toEntityId 507) )
        , ( toEntityId 528, GeoLocation "Nganzo" (Just <| toEntityId 507) )
        , ( toEntityId 534, GeoLocation "Va" (Just <| toEntityId 507) )
        , ( toEntityId 541, GeoLocation "Mwiyando" (Just <| toEntityId 540) )
        , ( toEntityId 547, GeoLocation "Rwa" (Just <| toEntityId 540) )
        , ( toEntityId 556, GeoLocation "Kabatezi" (Just <| toEntityId 540) )
        , ( toEntityId 564, GeoLocation "Kiryamo" (Just <| toEntityId 540) )
        , ( toEntityId 572, GeoLocation "Mubuga" (Just <| toEntityId 540) )
        , ( toEntityId 582, GeoLocation "Buranga" (Just <| toEntityId 581) )
        , ( toEntityId 590, GeoLocation "Gahinga" (Just <| toEntityId 581) )
        , ( toEntityId 595, GeoLocation "Gisozi" (Just <| toEntityId 581) )
        , ( toEntityId 606, GeoLocation "Mucaca" (Just <| toEntityId 581) )
        , ( toEntityId 619, GeoLocation "Busoro" (Just <| toEntityId 618) )
        , ( toEntityId 622, GeoLocation "Gikingo" (Just <| toEntityId 618) )
        , ( toEntityId 629, GeoLocation "Jango" (Just <| toEntityId 618) )
        , ( toEntityId 636, GeoLocation "Ruli" (Just <| toEntityId 618) )
        , ( toEntityId 643, GeoLocation "Rwesero" (Just <| toEntityId 618) )
        , ( toEntityId 655, GeoLocation "Rurembo" (Just <| toEntityId 654) )
        , ( toEntityId 661, GeoLocation "Gataba" (Just <| toEntityId 654) )
        , ( toEntityId 667, GeoLocation "Kamonyi" (Just <| toEntityId 654) )
        , ( toEntityId 675, GeoLocation "Murambi" (Just <| toEntityId 654) )
        , ( toEntityId 680, GeoLocation "Nyundo" (Just <| toEntityId 654) )
        , ( toEntityId 686, GeoLocation "Rumbi" (Just <| toEntityId 654) )
        , ( toEntityId 692, GeoLocation "Burimba" (Just <| toEntityId 691) )
        , ( toEntityId 696, GeoLocation "Busanane" (Just <| toEntityId 691) )
        , ( toEntityId 701, GeoLocation "Joma" (Just <| toEntityId 691) )
        , ( toEntityId 707, GeoLocation "Kageyo" (Just <| toEntityId 691) )
        , ( toEntityId 713, GeoLocation "Mbogo" (Just <| toEntityId 691) )
        , ( toEntityId 719, GeoLocation "Razi" (Just <| toEntityId 691) )
        , ( toEntityId 725, GeoLocation "Rwankuba" (Just <| toEntityId 691) )
        , ( toEntityId 731, GeoLocation "Shyombwe" (Just <| toEntityId 691) )
        , ( toEntityId 738, GeoLocation "Rwamahwa" (Just <| toEntityId 737) )
        , ( toEntityId 747, GeoLocation "Cyohoha" (Just <| toEntityId 737) )
        , ( toEntityId 758, GeoLocation "Gitare" (Just <| toEntityId 737) )
        , ( toEntityId 770, GeoLocation "Butangampundu" (Just <| toEntityId 769) )
        , ( toEntityId 785, GeoLocation "Karengeri" (Just <| toEntityId 769) )
        , ( toEntityId 800, GeoLocation "Taba" (Just <| toEntityId 769) )
        , ( toEntityId 814, GeoLocation "Gasiza" (Just <| toEntityId 813) )
        , ( toEntityId 820, GeoLocation "Giko" (Just <| toEntityId 813) )
        , ( toEntityId 829, GeoLocation "Kayenzi" (Just <| toEntityId 813) )
        , ( toEntityId 835, GeoLocation "Mukoto" (Just <| toEntityId 813) )
        , ( toEntityId 843, GeoLocation "Nyirangarama" (Just <| toEntityId 813) )
        , ( toEntityId 857, GeoLocation "Busoro" (Just <| toEntityId 856) )
        , ( toEntityId 863, GeoLocation "Butare" (Just <| toEntityId 856) )
        , ( toEntityId 869, GeoLocation "Gahororo" (Just <| toEntityId 856) )
        , ( toEntityId 876, GeoLocation "Gitumba" (Just <| toEntityId 856) )
        , ( toEntityId 882, GeoLocation "Karama" (Just <| toEntityId 856) )
        , ( toEntityId 889, GeoLocation "Mwumba" (Just <| toEntityId 856) )
        , ( toEntityId 895, GeoLocation "Ndarage" (Just <| toEntityId 856) )
        , ( toEntityId 902, GeoLocation "Budakiranya" (Just <| toEntityId 901) )
        , ( toEntityId 911, GeoLocation "Migendezo" (Just <| toEntityId 901) )
        , ( toEntityId 920, GeoLocation "Rudogo" (Just <| toEntityId 901) )
        , ( toEntityId 930, GeoLocation "Burehe" (Just <| toEntityId 929) )
        , ( toEntityId 934, GeoLocation "Marembo" (Just <| toEntityId 929) )
        , ( toEntityId 943, GeoLocation "Rwili" (Just <| toEntityId 929) )
        , ( toEntityId 956, GeoLocation "Butunzi" (Just <| toEntityId 955) )
        , ( toEntityId 963, GeoLocation "Karegamazi" (Just <| toEntityId 955) )
        , ( toEntityId 970, GeoLocation "Marembo" (Just <| toEntityId 955) )
        , ( toEntityId 976, GeoLocation "Rebero" (Just <| toEntityId 955) )
        , ( toEntityId 984, GeoLocation "Kamushenyi" (Just <| toEntityId 983) )
        , ( toEntityId 987, GeoLocation "Kigarama" (Just <| toEntityId 983) )
        , ( toEntityId 993, GeoLocation "Mubuga" (Just <| toEntityId 983) )
        , ( toEntityId 1000, GeoLocation "Murama" (Just <| toEntityId 983) )
        , ( toEntityId 1007, GeoLocation "Sayo" (Just <| toEntityId 983) )
        , ( toEntityId 1014, GeoLocation "Gitatsa" (Just <| toEntityId 983) )
        , ( toEntityId 1025, GeoLocation "Shengampuli" (Just <| toEntityId 1024) )
        , ( toEntityId 1027, GeoLocation "Kabuga" (Just <| toEntityId 1024) )
        , ( toEntityId 1035, GeoLocation "Kigarama" (Just <| toEntityId 1024) )
        , ( toEntityId 1040, GeoLocation "Kivugiza" (Just <| toEntityId 1024) )
        , ( toEntityId 1045, GeoLocation "Nyamyumba" (Just <| toEntityId 1024) )
        , ( toEntityId 1057, GeoLocation "Bukoro" (Just <| toEntityId 1056) )
        , ( toEntityId 1068, GeoLocation "Mushari" (Just <| toEntityId 1056) )
        , ( toEntityId 1076, GeoLocation "Ngiramazi" (Just <| toEntityId 1056) )
        , ( toEntityId 1084, GeoLocation "Rurenge" (Just <| toEntityId 1056) )
        , ( toEntityId 1094, GeoLocation "Mugambazi" (Just <| toEntityId 1093) )
        , ( toEntityId 1100, GeoLocation "Mvuzo" (Just <| toEntityId 1093) )
        , ( toEntityId 1108, GeoLocation "Bubangu" (Just <| toEntityId 1093) )
        , ( toEntityId 1119, GeoLocation "Gatwa" (Just <| toEntityId 1093) )
        , ( toEntityId 1116, GeoLocation "Bubangu" (Just <| toEntityId 1115) )
        , ( toEntityId 1130, GeoLocation "Kabuga" (Just <| toEntityId 1129) )
        , ( toEntityId 1137, GeoLocation "Karambo" (Just <| toEntityId 1129) )
        , ( toEntityId 1144, GeoLocation "Mugote" (Just <| toEntityId 1129) )
        , ( toEntityId 1153, GeoLocation "Munyarwanda" (Just <| toEntityId 1129) )
        , ( toEntityId 1163, GeoLocation "Kiyanza" (Just <| toEntityId 1162) )
        , ( toEntityId 1170, GeoLocation "Mahaza" (Just <| toEntityId 1162) )
        , ( toEntityId 1178, GeoLocation "Kajevuba" (Just <| toEntityId 1162) )
        , ( toEntityId 1190, GeoLocation "Buraro" (Just <| toEntityId 1189) )
        , ( toEntityId 1199, GeoLocation "Bwimo" (Just <| toEntityId 1189) )
        , ( toEntityId 1206, GeoLocation "Mberuka" (Just <| toEntityId 1189) )
        , ( toEntityId 1212, GeoLocation "Mbuye" (Just <| toEntityId 1189) )
        , ( toEntityId 1219, GeoLocation "Taba" (Just <| toEntityId 1218) )
        , ( toEntityId 1224, GeoLocation "Gako" (Just <| toEntityId 1218) )
        , ( toEntityId 1231, GeoLocation "Kirenge" (Just <| toEntityId 1218) )
        , ( toEntityId 1239, GeoLocation "Bugaragara" (Just <| toEntityId 1238) )
        , ( toEntityId 1248, GeoLocation "Kijabagwe" (Just <| toEntityId 1238) )
        , ( toEntityId 1255, GeoLocation "Muvumu" (Just <| toEntityId 1238) )
        , ( toEntityId 1266, GeoLocation "Rubona" (Just <| toEntityId 1238) )
        , ( toEntityId 1275, GeoLocation "Rutonde" (Just <| toEntityId 1238) )
        , ( toEntityId 1285, GeoLocation "Gahabwa" (Just <| toEntityId 1284) )
        , ( toEntityId 1290, GeoLocation "Misezero" (Just <| toEntityId 1284) )
        , ( toEntityId 1298, GeoLocation "Nyirabirori" (Just <| toEntityId 1284) )
        , ( toEntityId 1306, GeoLocation "Taba" (Just <| toEntityId 1284) )
        , ( toEntityId 1313, GeoLocation "Barari" (Just <| toEntityId 1284) )
        ]


getGeoVillages : EveryDict GeoLocationId GeoLocation
getGeoVillages =
    EveryDict.fromList <|
        [ ( toEntityId 5, GeoLocation "Birambo" (Just <| toEntityId 4) )
        , ( toEntityId 6, GeoLocation "Gitwa" (Just <| toEntityId 4) )
        , ( toEntityId 7, GeoLocation "Kirwa" (Just <| toEntityId 4) )
        , ( toEntityId 8, GeoLocation "Nyarubande" (Just <| toEntityId 4) )
        , ( toEntityId 10, GeoLocation "Buhuga" (Just <| toEntityId 9) )
        , ( toEntityId 11, GeoLocation "Butereri" (Just <| toEntityId 9) )
        , ( toEntityId 12, GeoLocation "Gasakuza" (Just <| toEntityId 9) )
        , ( toEntityId 13, GeoLocation "Kirwa" (Just <| toEntityId 9) )
        , ( toEntityId 14, GeoLocation "Rubaga" (Just <| toEntityId 9) )
        , ( toEntityId 15, GeoLocation "Rugendabari" (Just <| toEntityId 9) )
        , ( toEntityId 16, GeoLocation "Rwinkuba" (Just <| toEntityId 9) )
        , ( toEntityId 18, GeoLocation "Gatoke" (Just <| toEntityId 17) )
        , ( toEntityId 19, GeoLocation "Kamina" (Just <| toEntityId 17) )
        , ( toEntityId 20, GeoLocation "Karambi" (Just <| toEntityId 17) )
        , ( toEntityId 21, GeoLocation "Nyagasozi" (Just <| toEntityId 17) )
        , ( toEntityId 22, GeoLocation "Ruboza" (Just <| toEntityId 17) )
        , ( toEntityId 24, GeoLocation "Bunyangezi" (Just <| toEntityId 23) )
        , ( toEntityId 25, GeoLocation "Kajereri" (Just <| toEntityId 23) )
        , ( toEntityId 26, GeoLocation "Kamina" (Just <| toEntityId 23) )
        , ( toEntityId 27, GeoLocation "Mwendo" (Just <| toEntityId 23) )
        , ( toEntityId 28, GeoLocation "Nyarubuye" (Just <| toEntityId 23) )
        , ( toEntityId 29, GeoLocation "Rwankuba" (Just <| toEntityId 23) )
        , ( toEntityId 31, GeoLocation "Gasaso" (Just <| toEntityId 30) )
        , ( toEntityId 32, GeoLocation "Kirabo" (Just <| toEntityId 30) )
        , ( toEntityId 33, GeoLocation "Munyinya" (Just <| toEntityId 30) )
        , ( toEntityId 34, GeoLocation "Ngezi" (Just <| toEntityId 30) )
        , ( toEntityId 35, GeoLocation "Rusebeya" (Just <| toEntityId 30) )
        , ( toEntityId 36, GeoLocation "Wimfizi" (Just <| toEntityId 30) )
        , ( toEntityId 38, GeoLocation "Kabuga" (Just <| toEntityId 37) )
        , ( toEntityId 39, GeoLocation "Kamonyi" (Just <| toEntityId 37) )
        , ( toEntityId 40, GeoLocation "Karaba" (Just <| toEntityId 37) )
        , ( toEntityId 41, GeoLocation "Mugunga" (Just <| toEntityId 37) )
        , ( toEntityId 42, GeoLocation "Rutenga" (Just <| toEntityId 37) )
        , ( toEntityId 44, GeoLocation "Bukinga" (Just <| toEntityId 43) )
        , ( toEntityId 45, GeoLocation "Gashirwe" (Just <| toEntityId 43) )
        , ( toEntityId 46, GeoLocation "Kabaya" (Just <| toEntityId 43) )
        , ( toEntityId 47, GeoLocation "Kabugiri" (Just <| toEntityId 43) )
        , ( toEntityId 48, GeoLocation "Rurangara" (Just <| toEntityId 43) )
        , ( toEntityId 51, GeoLocation "Mucumazo" (Just <| toEntityId 50) )
        , ( toEntityId 52, GeoLocation "Ntarabana" (Just <| toEntityId 50) )
        , ( toEntityId 53, GeoLocation "Nyamasuka" (Just <| toEntityId 50) )
        , ( toEntityId 54, GeoLocation "Rubuguma" (Just <| toEntityId 50) )
        , ( toEntityId 78, GeoLocation "Buhuri" (Just <| toEntityId 50) )
        , ( toEntityId 79, GeoLocation "Bukamba" (Just <| toEntityId 50) )
        , ( toEntityId 80, GeoLocation "Bushagashi" (Just <| toEntityId 50) )
        , ( toEntityId 81, GeoLocation "Gatare" (Just <| toEntityId 50) )
        , ( toEntityId 56, GeoLocation "Akanduga" (Just <| toEntityId 55) )
        , ( toEntityId 57, GeoLocation "Burengo" (Just <| toEntityId 55) )
        , ( toEntityId 58, GeoLocation "Bushyama" (Just <| toEntityId 55) )
        , ( toEntityId 59, GeoLocation "Matovu" (Just <| toEntityId 55) )
        , ( toEntityId 60, GeoLocation "Mbogo" (Just <| toEntityId 55) )
        , ( toEntityId 61, GeoLocation "Murambi" (Just <| toEntityId 55) )
        , ( toEntityId 62, GeoLocation "Rwahi" (Just <| toEntityId 55) )
        , ( toEntityId 63, GeoLocation "Shyunga" (Just <| toEntityId 55) )
        , ( toEntityId 65, GeoLocation "Buhara" (Just <| toEntityId 64) )
        , ( toEntityId 66, GeoLocation "Gaseke" (Just <| toEntityId 64) )
        , ( toEntityId 67, GeoLocation "Karambo" (Just <| toEntityId 64) )
        , ( toEntityId 68, GeoLocation "Karoli" (Just <| toEntityId 64) )
        , ( toEntityId 69, GeoLocation "Musasa" (Just <| toEntityId 64) )
        , ( toEntityId 70, GeoLocation "Ntobwe" (Just <| toEntityId 64) )
        , ( toEntityId 71, GeoLocation "Vumandi" (Just <| toEntityId 64) )
        , ( toEntityId 73, GeoLocation "Baramba" (Just <| toEntityId 72) )
        , ( toEntityId 74, GeoLocation "Gikamba" (Just <| toEntityId 72) )
        , ( toEntityId 75, GeoLocation "Gitaba" (Just <| toEntityId 72) )
        , ( toEntityId 76, GeoLocation "Kavumu" (Just <| toEntityId 72) )
        , ( toEntityId 77, GeoLocation "Tumba" (Just <| toEntityId 72) )
        , ( toEntityId 84, GeoLocation "Buraza" (Just <| toEntityId 83) )
        , ( toEntityId 85, GeoLocation "Busoga" (Just <| toEntityId 83) )
        , ( toEntityId 86, GeoLocation "Karombero" (Just <| toEntityId 83) )
        , ( toEntityId 87, GeoLocation "Muhaza" (Just <| toEntityId 83) )
        , ( toEntityId 88, GeoLocation "Mushirarungu" (Just <| toEntityId 83) )
        , ( toEntityId 89, GeoLocation "Ntaraga" (Just <| toEntityId 83) )
        , ( toEntityId 90, GeoLocation "Rutaramiro" (Just <| toEntityId 83) )
        , ( toEntityId 92, GeoLocation "Butaraga" (Just <| toEntityId 91) )
        , ( toEntityId 93, GeoLocation "Gatoki" (Just <| toEntityId 91) )
        , ( toEntityId 94, GeoLocation "Gatorero" (Just <| toEntityId 91) )
        , ( toEntityId 95, GeoLocation "Kabungwe" (Just <| toEntityId 91) )
        , ( toEntityId 96, GeoLocation "Karenge" (Just <| toEntityId 91) )
        , ( toEntityId 97, GeoLocation "Muhororo" (Just <| toEntityId 91) )
        , ( toEntityId 98, GeoLocation "Musebeya" (Just <| toEntityId 91) )
        , ( toEntityId 99, GeoLocation "Tongoburo" (Just <| toEntityId 91) )
        , ( toEntityId 101, GeoLocation "Bukuba" (Just <| toEntityId 100) )
        , ( toEntityId 102, GeoLocation "Gahama" (Just <| toEntityId 100) )
        , ( toEntityId 103, GeoLocation "Gatare" (Just <| toEntityId 100) )
        , ( toEntityId 104, GeoLocation "Musebeya" (Just <| toEntityId 100) )
        , ( toEntityId 105, GeoLocation "Rugaragara" (Just <| toEntityId 100) )
        , ( toEntityId 106, GeoLocation "Rwobe" (Just <| toEntityId 100) )
        , ( toEntityId 108, GeoLocation "Cyabingo" (Just <| toEntityId 107) )
        , ( toEntityId 109, GeoLocation "Gishubi" (Just <| toEntityId 107) )
        , ( toEntityId 110, GeoLocation "Kambare" (Just <| toEntityId 107) )
        , ( toEntityId 111, GeoLocation "Kanyamukenke" (Just <| toEntityId 107) )
        , ( toEntityId 112, GeoLocation "Mucaca" (Just <| toEntityId 107) )
        , ( toEntityId 113, GeoLocation "Mutanda" (Just <| toEntityId 107) )
        , ( toEntityId 115, GeoLocation "Kigote" (Just <| toEntityId 114) )
        , ( toEntityId 116, GeoLocation "Muramba" (Just <| toEntityId 114) )
        , ( toEntityId 117, GeoLocation "Murehe" (Just <| toEntityId 114) )
        , ( toEntityId 118, GeoLocation "Nyabisika" (Just <| toEntityId 114) )
        , ( toEntityId 119, GeoLocation "Nyamugali" (Just <| toEntityId 114) )
        , ( toEntityId 120, GeoLocation "Rugendabare" (Just <| toEntityId 114) )
        , ( toEntityId 121, GeoLocation "Rukore" (Just <| toEntityId 114) )
        , ( toEntityId 124, GeoLocation "Mucuro" (Just <| toEntityId 123) )
        , ( toEntityId 125, GeoLocation "Murambi" (Just <| toEntityId 123) )
        , ( toEntityId 126, GeoLocation "Ndora" (Just <| toEntityId 123) )
        , ( toEntityId 127, GeoLocation "Rusebeya" (Just <| toEntityId 123) )
        , ( toEntityId 167, GeoLocation "Buyagiro" (Just <| toEntityId 123) )
        , ( toEntityId 168, GeoLocation "Gatwa" (Just <| toEntityId 123) )
        , ( toEntityId 169, GeoLocation "Gihemba" (Just <| toEntityId 123) )
        , ( toEntityId 170, GeoLocation "Gikerera" (Just <| toEntityId 123) )
        , ( toEntityId 171, GeoLocation "Karambi" (Just <| toEntityId 123) )
        , ( toEntityId 172, GeoLocation "Karorero" (Just <| toEntityId 123) )
        , ( toEntityId 129, GeoLocation "Bukanka" (Just <| toEntityId 128) )
        , ( toEntityId 130, GeoLocation "Cyandago" (Just <| toEntityId 128) )
        , ( toEntityId 131, GeoLocation "Gitenga" (Just <| toEntityId 128) )
        , ( toEntityId 132, GeoLocation "Kamatare" (Just <| toEntityId 128) )
        , ( toEntityId 133, GeoLocation "Murama" (Just <| toEntityId 128) )
        , ( toEntityId 134, GeoLocation "Murambi" (Just <| toEntityId 128) )
        , ( toEntityId 135, GeoLocation "Musave" (Just <| toEntityId 128) )
        , ( toEntityId 136, GeoLocation "Ntobwe" (Just <| toEntityId 128) )
        , ( toEntityId 137, GeoLocation "Rurambi" (Just <| toEntityId 128) )
        , ( toEntityId 138, GeoLocation "Rusuri" (Just <| toEntityId 128) )
        , ( toEntityId 140, GeoLocation "Bwimba" (Just <| toEntityId 139) )
        , ( toEntityId 141, GeoLocation "Gahondo" (Just <| toEntityId 139) )
        , ( toEntityId 142, GeoLocation "Gashigwe" (Just <| toEntityId 139) )
        , ( toEntityId 143, GeoLocation "Gishyinguro" (Just <| toEntityId 139) )
        , ( toEntityId 144, GeoLocation "Kaniga" (Just <| toEntityId 139) )
        , ( toEntityId 145, GeoLocation "Kanyiramanyana" (Just <| toEntityId 139) )
        , ( toEntityId 146, GeoLocation "Karambi" (Just <| toEntityId 139) )
        , ( toEntityId 147, GeoLocation "Karehe" (Just <| toEntityId 139) )
        , ( toEntityId 148, GeoLocation "Karuganda" (Just <| toEntityId 139) )
        , ( toEntityId 149, GeoLocation "Mbizi" (Just <| toEntityId 139) )
        , ( toEntityId 150, GeoLocation "Mbogo" (Just <| toEntityId 139) )
        , ( toEntityId 151, GeoLocation "Muyira" (Just <| toEntityId 139) )
        , ( toEntityId 152, GeoLocation "Ryabazungu" (Just <| toEntityId 139) )
        , ( toEntityId 154, GeoLocation "Akarugamba" (Just <| toEntityId 153) )
        , ( toEntityId 155, GeoLocation "Busingiryi" (Just <| toEntityId 153) )
        , ( toEntityId 156, GeoLocation "Kabaya" (Just <| toEntityId 153) )
        , ( toEntityId 157, GeoLocation "Kageyo" (Just <| toEntityId 153) )
        , ( toEntityId 158, GeoLocation "Kakinungu" (Just <| toEntityId 153) )
        , ( toEntityId 159, GeoLocation "Kivumu" (Just <| toEntityId 153) )
        , ( toEntityId 160, GeoLocation "Mazinga" (Just <| toEntityId 153) )
        , ( toEntityId 161, GeoLocation "Murambi" (Just <| toEntityId 153) )
        , ( toEntityId 162, GeoLocation "Museke" (Just <| toEntityId 153) )
        , ( toEntityId 163, GeoLocation "Nyamabuye" (Just <| toEntityId 153) )
        , ( toEntityId 164, GeoLocation "Ruberano" (Just <| toEntityId 153) )
        , ( toEntityId 165, GeoLocation "Sitwe" (Just <| toEntityId 153) )
        , ( toEntityId 166, GeoLocation "Umujyi Wa Gakenke" (Just <| toEntityId 153) )
        , ( toEntityId 175, GeoLocation "Gasanzwe" (Just <| toEntityId 174) )
        , ( toEntityId 176, GeoLocation "Kabwika" (Just <| toEntityId 174) )
        , ( toEntityId 177, GeoLocation "Kamurambo" (Just <| toEntityId 174) )
        , ( toEntityId 178, GeoLocation "Kanwa" (Just <| toEntityId 174) )
        , ( toEntityId 179, GeoLocation "Rubuga" (Just <| toEntityId 174) )
        , ( toEntityId 215, GeoLocation "Buhira" (Just <| toEntityId 174) )
        , ( toEntityId 216, GeoLocation "Buturuba" (Just <| toEntityId 174) )
        , ( toEntityId 181, GeoLocation "Gaseke" (Just <| toEntityId 180) )
        , ( toEntityId 182, GeoLocation "Gatwa" (Just <| toEntityId 180) )
        , ( toEntityId 183, GeoLocation "Gitaba" (Just <| toEntityId 180) )
        , ( toEntityId 184, GeoLocation "Kabere" (Just <| toEntityId 180) )
        , ( toEntityId 185, GeoLocation "Kabugomba" (Just <| toEntityId 180) )
        , ( toEntityId 186, GeoLocation "Kibara" (Just <| toEntityId 180) )
        , ( toEntityId 187, GeoLocation "Murambo" (Just <| toEntityId 180) )
        , ( toEntityId 189, GeoLocation "Busaro" (Just <| toEntityId 188) )
        , ( toEntityId 190, GeoLocation "Bushita" (Just <| toEntityId 188) )
        , ( toEntityId 191, GeoLocation "Gasharu" (Just <| toEntityId 188) )
        , ( toEntityId 192, GeoLocation "Gihanga" (Just <| toEntityId 188) )
        , ( toEntityId 193, GeoLocation "Kangomba" (Just <| toEntityId 188) )
        , ( toEntityId 196, GeoLocation "Kanteko" (Just <| toEntityId 188) )
        , ( toEntityId 198, GeoLocation "Murambi" (Just <| toEntityId 188) )
        , ( toEntityId 200, GeoLocation "Mwisha" (Just <| toEntityId 188) )
        , ( toEntityId 201, GeoLocation "Rutenderi" (Just <| toEntityId 188) )
        , ( toEntityId 195, GeoLocation "Bwiyando" (Just <| toEntityId 194) )
        , ( toEntityId 197, GeoLocation "Gashinge" (Just <| toEntityId 194) )
        , ( toEntityId 199, GeoLocation "Kadehero" (Just <| toEntityId 194) )
        , ( toEntityId 202, GeoLocation "Masoro" (Just <| toEntityId 194) )
        , ( toEntityId 203, GeoLocation "Mukira" (Just <| toEntityId 194) )
        , ( toEntityId 204, GeoLocation "Nyamure" (Just <| toEntityId 194) )
        , ( toEntityId 205, GeoLocation "Rugarama" (Just <| toEntityId 194) )
        , ( toEntityId 206, GeoLocation "Rugendabari" (Just <| toEntityId 194) )
        , ( toEntityId 207, GeoLocation "Ruhore" (Just <| toEntityId 194) )
        , ( toEntityId 209, GeoLocation "Gahihi" (Just <| toEntityId 208) )
        , ( toEntityId 210, GeoLocation "Gikoro" (Just <| toEntityId 208) )
        , ( toEntityId 211, GeoLocation "Kara" (Just <| toEntityId 208) )
        , ( toEntityId 212, GeoLocation "Kirambo" (Just <| toEntityId 208) )
        , ( toEntityId 213, GeoLocation "Murandi" (Just <| toEntityId 208) )
        , ( toEntityId 214, GeoLocation "Nyamataha" (Just <| toEntityId 208) )
        , ( toEntityId 219, GeoLocation "Kinoko" (Just <| toEntityId 218) )
        , ( toEntityId 220, GeoLocation "Murambi" (Just <| toEntityId 218) )
        , ( toEntityId 221, GeoLocation "Mwanza" (Just <| toEntityId 218) )
        , ( toEntityId 222, GeoLocation "Nyabushishiri" (Just <| toEntityId 218) )
        , ( toEntityId 223, GeoLocation "Nyagisozi" (Just <| toEntityId 218) )
        , ( toEntityId 246, GeoLocation "Buhanga" (Just <| toEntityId 218) )
        , ( toEntityId 247, GeoLocation "Gitega" (Just <| toEntityId 218) )
        , ( toEntityId 225, GeoLocation "Buhimbi" (Just <| toEntityId 224) )
        , ( toEntityId 226, GeoLocation "Cyifuzo" (Just <| toEntityId 224) )
        , ( toEntityId 227, GeoLocation "Gitaba" (Just <| toEntityId 224) )
        , ( toEntityId 228, GeoLocation "Karama" (Just <| toEntityId 224) )
        , ( toEntityId 229, GeoLocation "Mugandu" (Just <| toEntityId 224) )
        , ( toEntityId 230, GeoLocation "Rugeshi" (Just <| toEntityId 224) )
        , ( toEntityId 231, GeoLocation "Rusasa" (Just <| toEntityId 224) )
        , ( toEntityId 232, GeoLocation "Rutake" (Just <| toEntityId 224) )
        , ( toEntityId 234, GeoLocation "Bukerera" (Just <| toEntityId 233) )
        , ( toEntityId 235, GeoLocation "Bunyironko" (Just <| toEntityId 233) )
        , ( toEntityId 236, GeoLocation "Kabusoro" (Just <| toEntityId 233) )
        , ( toEntityId 237, GeoLocation "Kibonwa" (Just <| toEntityId 233) )
        , ( toEntityId 238, GeoLocation "Rubona" (Just <| toEntityId 233) )
        , ( toEntityId 239, GeoLocation "Rurumbya" (Just <| toEntityId 233) )
        , ( toEntityId 241, GeoLocation "Burega" (Just <| toEntityId 240) )
        , ( toEntityId 242, GeoLocation "Gatongo" (Just <| toEntityId 240) )
        , ( toEntityId 243, GeoLocation "Gitovu" (Just <| toEntityId 240) )
        , ( toEntityId 244, GeoLocation "Nyabikenke" (Just <| toEntityId 240) )
        , ( toEntityId 245, GeoLocation "Rwampali" (Just <| toEntityId 240) )
        , ( toEntityId 250, GeoLocation "Gasebeya" (Just <| toEntityId 249) )
        , ( toEntityId 251, GeoLocation "Gashishi" (Just <| toEntityId 249) )
        , ( toEntityId 252, GeoLocation "Gitwe" (Just <| toEntityId 249) )
        , ( toEntityId 253, GeoLocation "Kabuye" (Just <| toEntityId 249) )
        , ( toEntityId 254, GeoLocation "Kanshenge" (Just <| toEntityId 249) )
        , ( toEntityId 255, GeoLocation "Kanyirantege" (Just <| toEntityId 249) )
        , ( toEntityId 256, GeoLocation "Marira" (Just <| toEntityId 249) )
        , ( toEntityId 257, GeoLocation "Nyarungu" (Just <| toEntityId 249) )
        , ( toEntityId 258, GeoLocation "Raro" (Just <| toEntityId 249) )
        , ( toEntityId 259, GeoLocation "Rugari" (Just <| toEntityId 249) )
        , ( toEntityId 260, GeoLocation "Ruhehe" (Just <| toEntityId 249) )
        , ( toEntityId 261, GeoLocation "Runeka" (Just <| toEntityId 249) )
        , ( toEntityId 263, GeoLocation "Bucyaba" (Just <| toEntityId 262) )
        , ( toEntityId 264, GeoLocation "Bugogo" (Just <| toEntityId 262) )
        , ( toEntityId 265, GeoLocation "Kidomo" (Just <| toEntityId 262) )
        , ( toEntityId 266, GeoLocation "Kintobo" (Just <| toEntityId 262) )
        , ( toEntityId 267, GeoLocation "Njugi" (Just <| toEntityId 262) )
        , ( toEntityId 268, GeoLocation "Nyamusongati" (Just <| toEntityId 262) )
        , ( toEntityId 269, GeoLocation "Rugeshi" (Just <| toEntityId 262) )
        , ( toEntityId 270, GeoLocation "Rutagara" (Just <| toEntityId 262) )
        , ( toEntityId 272, GeoLocation "Buhinda" (Just <| toEntityId 271) )
        , ( toEntityId 273, GeoLocation "Gatare" (Just <| toEntityId 271) )
        , ( toEntityId 274, GeoLocation "Horero" (Just <| toEntityId 271) )
        , ( toEntityId 275, GeoLocation "Kabyaza" (Just <| toEntityId 271) )
        , ( toEntityId 276, GeoLocation "Karingorera" (Just <| toEntityId 271) )
        , ( toEntityId 277, GeoLocation "Mbatabata" (Just <| toEntityId 271) )
        , ( toEntityId 278, GeoLocation "Mwasha" (Just <| toEntityId 271) )
        , ( toEntityId 279, GeoLocation "Ryabirere" (Just <| toEntityId 271) )
        , ( toEntityId 281, GeoLocation "Kabutwa" (Just <| toEntityId 280) )
        , ( toEntityId 282, GeoLocation "Karangara" (Just <| toEntityId 280) )
        , ( toEntityId 283, GeoLocation "Kinyababa" (Just <| toEntityId 280) )
        , ( toEntityId 284, GeoLocation "Rungu" (Just <| toEntityId 280) )
        , ( toEntityId 285, GeoLocation "Rusasa" (Just <| toEntityId 280) )
        , ( toEntityId 286, GeoLocation "Rusumo" (Just <| toEntityId 280) )
        , ( toEntityId 287, GeoLocation "Rwata" (Just <| toEntityId 280) )
        , ( toEntityId 288, GeoLocation "Taba" (Just <| toEntityId 280) )
        , ( toEntityId 291, GeoLocation "Bataga" (Just <| toEntityId 290) )
        , ( toEntityId 292, GeoLocation "Bumbeja" (Just <| toEntityId 290) )
        , ( toEntityId 293, GeoLocation "Bushumba" (Just <| toEntityId 290) )
        , ( toEntityId 294, GeoLocation "Cyumba" (Just <| toEntityId 290) )
        , ( toEntityId 295, GeoLocation "Gasovu" (Just <| toEntityId 290) )
        , ( toEntityId 296, GeoLocation "Gatare" (Just <| toEntityId 290) )
        , ( toEntityId 297, GeoLocation "Gatorero" (Just <| toEntityId 290) )
        , ( toEntityId 298, GeoLocation "Gishingo" (Just <| toEntityId 290) )
        , ( toEntityId 299, GeoLocation "Kigarama" (Just <| toEntityId 290) )
        , ( toEntityId 300, GeoLocation "Mugamba" (Just <| toEntityId 290) )
        ]
            ++ [ ( toEntityId 301, GeoLocation "Nyiramuhimba" (Just <| toEntityId 290) )
               , ( toEntityId 302, GeoLocation "Rwamiko" (Just <| toEntityId 290) )
               , ( toEntityId 303, GeoLocation "Ryarurimbura" (Just <| toEntityId 290) )
               , ( toEntityId 305, GeoLocation "Bukondo" (Just <| toEntityId 304) )
               , ( toEntityId 306, GeoLocation "Bukunga" (Just <| toEntityId 304) )
               , ( toEntityId 307, GeoLocation "Bukweto" (Just <| toEntityId 304) )
               , ( toEntityId 308, GeoLocation "Kabuye" (Just <| toEntityId 304) )
               , ( toEntityId 309, GeoLocation "Kavumu" (Just <| toEntityId 304) )
               , ( toEntityId 310, GeoLocation "Mubuga" (Just <| toEntityId 304) )
               , ( toEntityId 311, GeoLocation "Mwiyanike" (Just <| toEntityId 304) )
               , ( toEntityId 312, GeoLocation "Nyabigugu" (Just <| toEntityId 304) )
               , ( toEntityId 314, GeoLocation "Gatembe" (Just <| toEntityId 313) )
               , ( toEntityId 315, GeoLocation "Kabuhunu" (Just <| toEntityId 313) )
               , ( toEntityId 316, GeoLocation "Kabutare" (Just <| toEntityId 313) )
               , ( toEntityId 317, GeoLocation "Karambi" (Just <| toEntityId 313) )
               , ( toEntityId 318, GeoLocation "Karenge" (Just <| toEntityId 313) )
               , ( toEntityId 319, GeoLocation "Marembo" (Just <| toEntityId 313) )
               , ( toEntityId 320, GeoLocation "Nyiramisabike" (Just <| toEntityId 313) )
               , ( toEntityId 323, GeoLocation "Bigogwe" (Just <| toEntityId 322) )
               , ( toEntityId 324, GeoLocation "Buhuga" (Just <| toEntityId 322) )
               , ( toEntityId 339, GeoLocation "Cyintare" (Just <| toEntityId 322) )
               , ( toEntityId 340, GeoLocation "Nyarubuye" (Just <| toEntityId 322) )
               , ( toEntityId 326, GeoLocation "Kamomo" (Just <| toEntityId 325) )
               , ( toEntityId 327, GeoLocation "Kavumu" (Just <| toEntityId 325) )
               , ( toEntityId 328, GeoLocation "Kintarure" (Just <| toEntityId 325) )
               , ( toEntityId 329, GeoLocation "Munyege" (Just <| toEntityId 325) )
               , ( toEntityId 330, GeoLocation "Rugeshi" (Just <| toEntityId 325) )
               , ( toEntityId 331, GeoLocation "Rwakirari" (Just <| toEntityId 325) )
               , ( toEntityId 354, GeoLocation "Buranga" (Just <| toEntityId 325) )
               , ( toEntityId 333, GeoLocation "Buhayo" (Just <| toEntityId 332) )
               , ( toEntityId 334, GeoLocation "Kabara" (Just <| toEntityId 332) )
               , ( toEntityId 335, GeoLocation "Kivuruga" (Just <| toEntityId 332) )
               , ( toEntityId 336, GeoLocation "Masoro" (Just <| toEntityId 332) )
               , ( toEntityId 337, GeoLocation "Musekera" (Just <| toEntityId 332) )
               , ( toEntityId 338, GeoLocation "Ngarama" (Just <| toEntityId 332) )
               , ( toEntityId 342, GeoLocation "Bushoka" (Just <| toEntityId 341) )
               , ( toEntityId 343, GeoLocation "Kabuhoma" (Just <| toEntityId 341) )
               , ( toEntityId 344, GeoLocation "Kamwumba" (Just <| toEntityId 341) )
               , ( toEntityId 345, GeoLocation "Nturo" (Just <| toEntityId 341) )
               , ( toEntityId 346, GeoLocation "Nyarungu" (Just <| toEntityId 341) )
               , ( toEntityId 348, GeoLocation "Gasave" (Just <| toEntityId 347) )
               , ( toEntityId 349, GeoLocation "Karuhunge" (Just <| toEntityId 347) )
               , ( toEntityId 350, GeoLocation "Mugali" (Just <| toEntityId 347) )
               , ( toEntityId 351, GeoLocation "Rurambo" (Just <| toEntityId 347) )
               , ( toEntityId 352, GeoLocation "Rutamba" (Just <| toEntityId 347) )
               , ( toEntityId 353, GeoLocation "Rwamabare" (Just <| toEntityId 347) )
               , ( toEntityId 357, GeoLocation "Gabiro" (Just <| toEntityId 356) )
               , ( toEntityId 358, GeoLocation "Gashingiro" (Just <| toEntityId 356) )
               , ( toEntityId 359, GeoLocation "Kabeza" (Just <| toEntityId 356) )
               , ( toEntityId 360, GeoLocation "Kanamo" (Just <| toEntityId 356) )
               , ( toEntityId 361, GeoLocation "Karambi" (Just <| toEntityId 356) )
               , ( toEntityId 362, GeoLocation "Mubuga" (Just <| toEntityId 356) )
               , ( toEntityId 363, GeoLocation "Nyamiyaga" (Just <| toEntityId 356) )
               , ( toEntityId 364, GeoLocation "Rugendabari" (Just <| toEntityId 356) )
               , ( toEntityId 365, GeoLocation "Ryarugema" (Just <| toEntityId 356) )
               , ( toEntityId 367, GeoLocation "Bugari" (Just <| toEntityId 366) )
               , ( toEntityId 368, GeoLocation "Bweramana" (Just <| toEntityId 366) )
               , ( toEntityId 369, GeoLocation "Gashyushya" (Just <| toEntityId 366) )
               , ( toEntityId 370, GeoLocation "Gatovu" (Just <| toEntityId 366) )
               , ( toEntityId 371, GeoLocation "Muhororo" (Just <| toEntityId 366) )
               , ( toEntityId 372, GeoLocation "Munini" (Just <| toEntityId 366) )
               , ( toEntityId 373, GeoLocation "Muyaga" (Just <| toEntityId 366) )
               , ( toEntityId 374, GeoLocation "Nyangoma" (Just <| toEntityId 366) )
               , ( toEntityId 375, GeoLocation "Ruganda" (Just <| toEntityId 366) )
               , ( toEntityId 376, GeoLocation "Ruhanga" (Just <| toEntityId 366) )
               , ( toEntityId 378, GeoLocation "Gihita" (Just <| toEntityId 377) )
               , ( toEntityId 379, GeoLocation "Gitaba" (Just <| toEntityId 377) )
               , ( toEntityId 380, GeoLocation "Kabuyora" (Just <| toEntityId 377) )
               , ( toEntityId 381, GeoLocation "Kagando" (Just <| toEntityId 377) )
               , ( toEntityId 382, GeoLocation "Karambi" (Just <| toEntityId 377) )
               , ( toEntityId 383, GeoLocation "Mataba" (Just <| toEntityId 377) )
               , ( toEntityId 384, GeoLocation "Mwanza" (Just <| toEntityId 377) )
               , ( toEntityId 385, GeoLocation "Nkurazo" (Just <| toEntityId 377) )
               , ( toEntityId 388, GeoLocation "Gahombo" (Just <| toEntityId 387) )
               , ( toEntityId 389, GeoLocation "Gahunda" (Just <| toEntityId 387) )
               , ( toEntityId 390, GeoLocation "Gasangwa" (Just <| toEntityId 387) )
               , ( toEntityId 391, GeoLocation "Gihinga" (Just <| toEntityId 387) )
               , ( toEntityId 392, GeoLocation "Kabarima" (Just <| toEntityId 387) )
               , ( toEntityId 393, GeoLocation "Kigeyo" (Just <| toEntityId 387) )
               , ( toEntityId 394, GeoLocation "Mbogo" (Just <| toEntityId 387) )
               , ( toEntityId 396, GeoLocation "Gihororo" (Just <| toEntityId 395) )
               , ( toEntityId 397, GeoLocation "Gitwa" (Just <| toEntityId 395) )
               , ( toEntityId 398, GeoLocation "Kanka" (Just <| toEntityId 395) )
               , ( toEntityId 399, GeoLocation "Kivuba" (Just <| toEntityId 395) )
               , ( toEntityId 400, GeoLocation "Nyabitare" (Just <| toEntityId 395) )
               , ( toEntityId 402, GeoLocation "Gisovu" (Just <| toEntityId 401) )
               , ( toEntityId 403, GeoLocation "Kabuga" (Just <| toEntityId 401) )
               , ( toEntityId 404, GeoLocation "Musave" (Just <| toEntityId 401) )
               , ( toEntityId 405, GeoLocation "Nyanza" (Just <| toEntityId 401) )
               , ( toEntityId 406, GeoLocation "Nyarubuye" (Just <| toEntityId 401) )
               , ( toEntityId 408, GeoLocation "Bukonde" (Just <| toEntityId 407) )
               , ( toEntityId 409, GeoLocation "Gaharo" (Just <| toEntityId 407) )
               , ( toEntityId 410, GeoLocation "Gitaragwe" (Just <| toEntityId 407) )
               , ( toEntityId 411, GeoLocation "Munihi" (Just <| toEntityId 407) )
               , ( toEntityId 412, GeoLocation "Mutara" (Just <| toEntityId 407) )
               , ( toEntityId 413, GeoLocation "Ndegamire" (Just <| toEntityId 407) )
               , ( toEntityId 414, GeoLocation "Sarabuye" (Just <| toEntityId 407) )
               , ( toEntityId 417, GeoLocation "Rwezamenyo" (Just <| toEntityId 416) )
               , ( toEntityId 453, GeoLocation "Cyarubayi" (Just <| toEntityId 416) )
               , ( toEntityId 454, GeoLocation "Karambi" (Just <| toEntityId 416) )
               , ( toEntityId 455, GeoLocation "Muhororo" (Just <| toEntityId 416) )
               , ( toEntityId 456, GeoLocation "Nturo" (Just <| toEntityId 416) )
               , ( toEntityId 419, GeoLocation "Kamasanze" (Just <| toEntityId 418) )
               , ( toEntityId 420, GeoLocation "Kamunyana" (Just <| toEntityId 418) )
               , ( toEntityId 421, GeoLocation "Karambo" (Just <| toEntityId 418) )
               , ( toEntityId 422, GeoLocation "Nganzo" (Just <| toEntityId 418) )
               , ( toEntityId 423, GeoLocation "Rutaraga" (Just <| toEntityId 418) )
               , ( toEntityId 425, GeoLocation "Kabuga" (Just <| toEntityId 424) )
               , ( toEntityId 426, GeoLocation "Kanaba" (Just <| toEntityId 424) )
               , ( toEntityId 427, GeoLocation "Nemba" (Just <| toEntityId 424) )
               , ( toEntityId 428, GeoLocation "Nyagasozi" (Just <| toEntityId 424) )
               , ( toEntityId 429, GeoLocation "Rusebeya" (Just <| toEntityId 424) )
               , ( toEntityId 431, GeoLocation "Gacemeri" (Just <| toEntityId 430) )
               , ( toEntityId 432, GeoLocation "Gasovu" (Just <| toEntityId 430) )
               , ( toEntityId 433, GeoLocation "Gatonde" (Just <| toEntityId 430) )
               , ( toEntityId 434, GeoLocation "Kabuhoro" (Just <| toEntityId 430) )
               , ( toEntityId 435, GeoLocation "Muhororo" (Just <| toEntityId 430) )
               , ( toEntityId 437, GeoLocation "Kiraro" (Just <| toEntityId 436) )
               , ( toEntityId 438, GeoLocation "Nyakazenga" (Just <| toEntityId 436) )
               , ( toEntityId 439, GeoLocation "Nyundo" (Just <| toEntityId 436) )
               , ( toEntityId 440, GeoLocation "Rubona" (Just <| toEntityId 436) )
               , ( toEntityId 442, GeoLocation "Biraro" (Just <| toEntityId 441) )
               , ( toEntityId 443, GeoLocation "Bushoka" (Just <| toEntityId 441) )
               , ( toEntityId 444, GeoLocation "Gashubi" (Just <| toEntityId 441) )
               , ( toEntityId 445, GeoLocation "Kabiganda" (Just <| toEntityId 441) )
               , ( toEntityId 448, GeoLocation "Kanyinya" (Just <| toEntityId 441) )
               , ( toEntityId 447, GeoLocation "Cyinama" (Just <| toEntityId 446) )
               , ( toEntityId 449, GeoLocation "Giheta" (Just <| toEntityId 446) )
               , ( toEntityId 450, GeoLocation "Nyagahondo" (Just <| toEntityId 446) )
               , ( toEntityId 451, GeoLocation "Nyakagezi" (Just <| toEntityId 446) )
               , ( toEntityId 452, GeoLocation "Rwimpiri" (Just <| toEntityId 446) )
               , ( toEntityId 459, GeoLocation "Nketsi" (Just <| toEntityId 458) )
               , ( toEntityId 503, GeoLocation "Gahama" (Just <| toEntityId 458) )
               , ( toEntityId 504, GeoLocation "Gatare" (Just <| toEntityId 458) )
               , ( toEntityId 505, GeoLocation "Gitaba" (Just <| toEntityId 458) )
               , ( toEntityId 506, GeoLocation "Kimanama" (Just <| toEntityId 458) )
               , ( toEntityId 461, GeoLocation "Gahabwa" (Just <| toEntityId 460) )
               , ( toEntityId 462, GeoLocation "Gahinga" (Just <| toEntityId 460) )
               , ( toEntityId 463, GeoLocation "Gahondo" (Just <| toEntityId 460) )
               , ( toEntityId 464, GeoLocation "Gasiza" (Just <| toEntityId 460) )
               , ( toEntityId 465, GeoLocation "Kabeza" (Just <| toEntityId 460) )
               , ( toEntityId 467, GeoLocation "Base" (Just <| toEntityId 466) )
               , ( toEntityId 468, GeoLocation "Gihinga" (Just <| toEntityId 466) )
               , ( toEntityId 469, GeoLocation "Karehe" (Just <| toEntityId 466) )
               , ( toEntityId 470, GeoLocation "Samuduha" (Just <| toEntityId 466) )
               , ( toEntityId 471, GeoLocation "Taba" (Just <| toEntityId 466) )
               , ( toEntityId 473, GeoLocation "Cura" (Just <| toEntityId 472) )
               , ( toEntityId 474, GeoLocation "Gitwa" (Just <| toEntityId 472) )
               , ( toEntityId 475, GeoLocation "Huro" (Just <| toEntityId 472) )
               , ( toEntityId 476, GeoLocation "Kabuga" (Just <| toEntityId 472) )
               , ( toEntityId 477, GeoLocation "Rubona" (Just <| toEntityId 472) )
               , ( toEntityId 479, GeoLocation "Akara" (Just <| toEntityId 478) )
               , ( toEntityId 480, GeoLocation "Cyenda" (Just <| toEntityId 478) )
               , ( toEntityId 481, GeoLocation "Giteme" (Just <| toEntityId 478) )
               , ( toEntityId 482, GeoLocation "Karobagire" (Just <| toEntityId 478) )
               , ( toEntityId 484, GeoLocation "Buhinya" (Just <| toEntityId 483) )
               , ( toEntityId 485, GeoLocation "Gakuyu" (Just <| toEntityId 483) )
               , ( toEntityId 486, GeoLocation "Kigali" (Just <| toEntityId 483) )
               , ( toEntityId 487, GeoLocation "Musenyi" (Just <| toEntityId 483) )
               , ( toEntityId 489, GeoLocation "Gisozi" (Just <| toEntityId 488) )
               , ( toEntityId 490, GeoLocation "Kinyonzo" (Just <| toEntityId 488) )
               , ( toEntityId 491, GeoLocation "Mubuga" (Just <| toEntityId 488) )
               , ( toEntityId 492, GeoLocation "Ranzi" (Just <| toEntityId 488) )
               , ( toEntityId 493, GeoLocation "Ruganda" (Just <| toEntityId 488) )
               , ( toEntityId 495, GeoLocation "Cyimbogo" (Just <| toEntityId 494) )
               , ( toEntityId 496, GeoLocation "Kanyana" (Just <| toEntityId 494) )
               , ( toEntityId 497, GeoLocation "Ruhorobero" (Just <| toEntityId 494) )
               , ( toEntityId 499, GeoLocation "Busake" (Just <| toEntityId 498) )
               , ( toEntityId 500, GeoLocation "Gikikira" (Just <| toEntityId 498) )
               , ( toEntityId 501, GeoLocation "Kibirizi" (Just <| toEntityId 498) )
               , ( toEntityId 502, GeoLocation "Nyakabanda" (Just <| toEntityId 498) )
               , ( toEntityId 509, GeoLocation "Bumba" (Just <| toEntityId 508) )
               , ( toEntityId 510, GeoLocation "Buzu" (Just <| toEntityId 508) )
               , ( toEntityId 511, GeoLocation "Gikoro" (Just <| toEntityId 508) )
               , ( toEntityId 512, GeoLocation "Gitovu" (Just <| toEntityId 508) )
               , ( toEntityId 513, GeoLocation "Gitwe" (Just <| toEntityId 508) )
               , ( toEntityId 514, GeoLocation "Mataba" (Just <| toEntityId 508) )
               , ( toEntityId 515, GeoLocation "Shiru" (Just <| toEntityId 508) )
               , ( toEntityId 517, GeoLocation "Gitanda" (Just <| toEntityId 516) )
               , ( toEntityId 518, GeoLocation "Kabingo" (Just <| toEntityId 516) )
               , ( toEntityId 519, GeoLocation "Kiyebe" (Just <| toEntityId 516) )
               , ( toEntityId 520, GeoLocation "Muramba" (Just <| toEntityId 516) )
               , ( toEntityId 521, GeoLocation "Ruhoko" (Just <| toEntityId 516) )
               , ( toEntityId 522, GeoLocation "Sanzare" (Just <| toEntityId 516) )
               , ( toEntityId 524, GeoLocation "Gikombe" (Just <| toEntityId 523) )
               , ( toEntityId 525, GeoLocation "Kibingo" (Just <| toEntityId 523) )
               , ( toEntityId 526, GeoLocation "Mahaha" (Just <| toEntityId 523) )
               , ( toEntityId 527, GeoLocation "Mugera" (Just <| toEntityId 523) )
               , ( toEntityId 529, GeoLocation "Muhororo" (Just <| toEntityId 528) )
               , ( toEntityId 530, GeoLocation "Nganzo" (Just <| toEntityId 528) )
               , ( toEntityId 531, GeoLocation "Ngoma" (Just <| toEntityId 528) )
               , ( toEntityId 532, GeoLocation "Nyarubuye" (Just <| toEntityId 528) )
               , ( toEntityId 533, GeoLocation "Vugangoma" (Just <| toEntityId 528) )
               , ( toEntityId 535, GeoLocation "Bukwera" (Just <| toEntityId 534) )
               , ( toEntityId 536, GeoLocation "Businde" (Just <| toEntityId 534) )
               , ( toEntityId 537, GeoLocation "Gikombe" (Just <| toEntityId 534) )
               , ( toEntityId 538, GeoLocation "Mutoyi" (Just <| toEntityId 534) )
               , ( toEntityId 539, GeoLocation "Ranzi" (Just <| toEntityId 534) )
               , ( toEntityId 542, GeoLocation "Kagano" (Just <| toEntityId 541) )
               , ( toEntityId 543, GeoLocation "Muguguri" (Just <| toEntityId 541) )
               , ( toEntityId 544, GeoLocation "Nyagasozi" (Just <| toEntityId 541) )
               , ( toEntityId 545, GeoLocation "Rubayo" (Just <| toEntityId 541) )
               , ( toEntityId 546, GeoLocation "Ruhondo" (Just <| toEntityId 541) )
               , ( toEntityId 579, GeoLocation "Gitabi" (Just <| toEntityId 541) )
               , ( toEntityId 580, GeoLocation "Gitoke" (Just <| toEntityId 541) )
               , ( toEntityId 548, GeoLocation "Bitaba" (Just <| toEntityId 547) )
               , ( toEntityId 549, GeoLocation "Cyinturo" (Just <| toEntityId 547) )
               , ( toEntityId 550, GeoLocation "Gacaca" (Just <| toEntityId 547) )
               , ( toEntityId 551, GeoLocation "Gihororo" (Just <| toEntityId 547) )
               , ( toEntityId 552, GeoLocation "Kabere" (Just <| toEntityId 547) )
               , ( toEntityId 553, GeoLocation "Mafubo" (Just <| toEntityId 547) )
               , ( toEntityId 554, GeoLocation "Nyagahondo" (Just <| toEntityId 547) )
               , ( toEntityId 555, GeoLocation "Nyarubande" (Just <| toEntityId 547) )
               , ( toEntityId 557, GeoLocation "Curugusi" (Just <| toEntityId 556) )
               , ( toEntityId 558, GeoLocation "Gasave" (Just <| toEntityId 556) )
               , ( toEntityId 559, GeoLocation "Gitabi" (Just <| toEntityId 556) )
               , ( toEntityId 560, GeoLocation "Kabatezi" (Just <| toEntityId 556) )
               , ( toEntityId 561, GeoLocation "Kasheshe" (Just <| toEntityId 556) )
               , ( toEntityId 562, GeoLocation "Runyinya" (Just <| toEntityId 556) )
               , ( toEntityId 563, GeoLocation "Rusororo" (Just <| toEntityId 556) )
               , ( toEntityId 565, GeoLocation "Akamagaju" (Just <| toEntityId 564) )
               , ( toEntityId 566, GeoLocation "Gahondo" (Just <| toEntityId 564) )
               , ( toEntityId 567, GeoLocation "Munyinya" (Just <| toEntityId 564) )
               , ( toEntityId 568, GeoLocation "Murambi" (Just <| toEntityId 564) )
               , ( toEntityId 569, GeoLocation "Rugarama" (Just <| toEntityId 564) )
               , ( toEntityId 570, GeoLocation "Rugege" (Just <| toEntityId 564) )
               , ( toEntityId 571, GeoLocation "Sezuku" (Just <| toEntityId 564) )
               , ( toEntityId 573, GeoLocation "Butambwe" (Just <| toEntityId 572) )
               , ( toEntityId 574, GeoLocation "Kanini" (Just <| toEntityId 572) )
               , ( toEntityId 575, GeoLocation "Kavuza" (Just <| toEntityId 572) )
               , ( toEntityId 576, GeoLocation "Mubuga" (Just <| toEntityId 572) )
               , ( toEntityId 577, GeoLocation "Mwirika" (Just <| toEntityId 572) )
               , ( toEntityId 578, GeoLocation "Mwurire" (Just <| toEntityId 572) )
               , ( toEntityId 583, GeoLocation "Buranga" (Just <| toEntityId 582) )
               , ( toEntityId 584, GeoLocation "Burego" (Just <| toEntityId 582) )
               , ( toEntityId 585, GeoLocation "Butare" (Just <| toEntityId 582) )
               , ( toEntityId 586, GeoLocation "Kanyansyo" (Just <| toEntityId 582) )
               , ( toEntityId 587, GeoLocation "Muganwa" (Just <| toEntityId 582) )
               , ( toEntityId 588, GeoLocation "Mukaka" (Just <| toEntityId 582) )
               , ( toEntityId 589, GeoLocation "Rukoji" (Just <| toEntityId 582) )
               , ( toEntityId 591, GeoLocation "Bitare" (Just <| toEntityId 590) )
               , ( toEntityId 592, GeoLocation "Bukurura" (Just <| toEntityId 590) )
               , ( toEntityId 593, GeoLocation "Kabaya" (Just <| toEntityId 590) )
               , ( toEntityId 594, GeoLocation "Kilimbi" (Just <| toEntityId 590) )
               , ( toEntityId 596, GeoLocation "Gisagara" (Just <| toEntityId 595) )
               , ( toEntityId 597, GeoLocation "Kabushara" (Just <| toEntityId 595) )
               , ( toEntityId 598, GeoLocation "Kamatete" (Just <| toEntityId 595) )
               , ( toEntityId 599, GeoLocation "Kanama" (Just <| toEntityId 595) )
               , ( toEntityId 600, GeoLocation "Kanunga" (Just <| toEntityId 595) )
               , ( toEntityId 601, GeoLocation "Kanzoka" (Just <| toEntityId 595) )
               , ( toEntityId 602, GeoLocation "Karukara" (Just <| toEntityId 595) )
               , ( toEntityId 603, GeoLocation "Kirehe" (Just <| toEntityId 595) )
               , ( toEntityId 604, GeoLocation "Mushubi" (Just <| toEntityId 595) )
               , ( toEntityId 605, GeoLocation "Nyamyumba" (Just <| toEntityId 595) )
               , ( toEntityId 607, GeoLocation "Cyahafi" (Just <| toEntityId 606) )
               , ( toEntityId 608, GeoLocation "Gatare" (Just <| toEntityId 606) )
               ]
            ++ [ ( toEntityId 609, GeoLocation "Kabingo" (Just <| toEntityId 606) )
               , ( toEntityId 610, GeoLocation "Kabuye" (Just <| toEntityId 606) )
               , ( toEntityId 611, GeoLocation "Kamuvunyi" (Just <| toEntityId 606) )
               , ( toEntityId 612, GeoLocation "Kiruhura" (Just <| toEntityId 606) )
               , ( toEntityId 613, GeoLocation "Kiryamo" (Just <| toEntityId 606) )
               , ( toEntityId 614, GeoLocation "Munyege" (Just <| toEntityId 606) )
               , ( toEntityId 615, GeoLocation "Musange" (Just <| toEntityId 606) )
               , ( toEntityId 616, GeoLocation "Ntakabavu" (Just <| toEntityId 606) )
               , ( toEntityId 617, GeoLocation "Nyamiyaga" (Just <| toEntityId 606) )
               , ( toEntityId 620, GeoLocation "Nkoto" (Just <| toEntityId 619) )
               , ( toEntityId 621, GeoLocation "Rugaragara" (Just <| toEntityId 619) )
               , ( toEntityId 649, GeoLocation "Congoli" (Just <| toEntityId 619) )
               , ( toEntityId 650, GeoLocation "Cyoganyoni" (Just <| toEntityId 619) )
               , ( toEntityId 651, GeoLocation "Gitaba" (Just <| toEntityId 619) )
               , ( toEntityId 652, GeoLocation "Kabare" (Just <| toEntityId 619) )
               , ( toEntityId 653, GeoLocation "Kibirizi" (Just <| toEntityId 619) )
               , ( toEntityId 623, GeoLocation "Bushoka" (Just <| toEntityId 622) )
               , ( toEntityId 624, GeoLocation "Gatwa" (Just <| toEntityId 622) )
               , ( toEntityId 625, GeoLocation "Kabingo" (Just <| toEntityId 622) )
               , ( toEntityId 626, GeoLocation "Karango" (Just <| toEntityId 622) )
               , ( toEntityId 627, GeoLocation "Nyamugari" (Just <| toEntityId 622) )
               , ( toEntityId 628, GeoLocation "Rumasa" (Just <| toEntityId 622) )
               , ( toEntityId 630, GeoLocation "Gatagara" (Just <| toEntityId 629) )
               , ( toEntityId 631, GeoLocation "Gihura" (Just <| toEntityId 629) )
               , ( toEntityId 632, GeoLocation "Gitonde" (Just <| toEntityId 629) )
               , ( toEntityId 633, GeoLocation "Kinyonzo" (Just <| toEntityId 629) )
               , ( toEntityId 634, GeoLocation "Mubuga" (Just <| toEntityId 629) )
               , ( toEntityId 635, GeoLocation "Murehe" (Just <| toEntityId 629) )
               , ( toEntityId 637, GeoLocation "Bariza" (Just <| toEntityId 636) )
               , ( toEntityId 638, GeoLocation "Gahondo" (Just <| toEntityId 636) )
               , ( toEntityId 639, GeoLocation "Gataba" (Just <| toEntityId 636) )
               , ( toEntityId 640, GeoLocation "Mugambazi" (Just <| toEntityId 636) )
               , ( toEntityId 641, GeoLocation "Ngayake" (Just <| toEntityId 636) )
               , ( toEntityId 642, GeoLocation "Nyakarambi" (Just <| toEntityId 636) )
               , ( toEntityId 644, GeoLocation "Gatare" (Just <| toEntityId 643) )
               , ( toEntityId 645, GeoLocation "Gisizi" (Just <| toEntityId 643) )
               , ( toEntityId 646, GeoLocation "Mabago" (Just <| toEntityId 643) )
               , ( toEntityId 647, GeoLocation "Mugwato" (Just <| toEntityId 643) )
               , ( toEntityId 648, GeoLocation "Nyarunyinya" (Just <| toEntityId 643) )
               , ( toEntityId 656, GeoLocation "Bushoka" (Just <| toEntityId 655) )
               , ( toEntityId 657, GeoLocation "Mazinga" (Just <| toEntityId 655) )
               , ( toEntityId 658, GeoLocation "Murori" (Just <| toEntityId 655) )
               , ( toEntityId 659, GeoLocation "Nyakabungo" (Just <| toEntityId 655) )
               , ( toEntityId 660, GeoLocation "Rugamba" (Just <| toEntityId 655) )
               , ( toEntityId 662, GeoLocation "Bumonyo A" (Just <| toEntityId 661) )
               , ( toEntityId 663, GeoLocation "Gahama" (Just <| toEntityId 661) )
               , ( toEntityId 664, GeoLocation "Gataba" (Just <| toEntityId 661) )
               , ( toEntityId 665, GeoLocation "Kebero" (Just <| toEntityId 661) )
               , ( toEntityId 666, GeoLocation "Kibaya" (Just <| toEntityId 661) )
               , ( toEntityId 668, GeoLocation "Burinda" (Just <| toEntityId 667) )
               , ( toEntityId 669, GeoLocation "Gakindo" (Just <| toEntityId 667) )
               , ( toEntityId 670, GeoLocation "Gapfura" (Just <| toEntityId 667) )
               , ( toEntityId 671, GeoLocation "Gitwe" (Just <| toEntityId 667) )
               , ( toEntityId 672, GeoLocation "Kidomo" (Just <| toEntityId 667) )
               , ( toEntityId 673, GeoLocation "Nyagahama" (Just <| toEntityId 667) )
               , ( toEntityId 674, GeoLocation "Rurambi" (Just <| toEntityId 667) )
               , ( toEntityId 676, GeoLocation "Buharabuye" (Just <| toEntityId 675) )
               , ( toEntityId 677, GeoLocation "Karuhunge" (Just <| toEntityId 675) )
               , ( toEntityId 678, GeoLocation "Kirehe" (Just <| toEntityId 675) )
               , ( toEntityId 679, GeoLocation "Nyange" (Just <| toEntityId 675) )
               , ( toEntityId 681, GeoLocation "Bukingo" (Just <| toEntityId 680) )
               , ( toEntityId 682, GeoLocation "Bumonyo B" (Just <| toEntityId 680) )
               , ( toEntityId 683, GeoLocation "Gisovu" (Just <| toEntityId 680) )
               , ( toEntityId 684, GeoLocation "Nyundo" (Just <| toEntityId 680) )
               , ( toEntityId 685, GeoLocation "Tane" (Just <| toEntityId 680) )
               , ( toEntityId 687, GeoLocation "Bukiza" (Just <| toEntityId 686) )
               , ( toEntityId 688, GeoLocation "Buyora" (Just <| toEntityId 686) )
               , ( toEntityId 689, GeoLocation "Bwanamo" (Just <| toEntityId 686) )
               , ( toEntityId 690, GeoLocation "Ninda" (Just <| toEntityId 686) )
               , ( toEntityId 693, GeoLocation "Kabuye" (Just <| toEntityId 692) )
               , ( toEntityId 694, GeoLocation "Kara" (Just <| toEntityId 692) )
               , ( toEntityId 695, GeoLocation "Kivumu" (Just <| toEntityId 692) )
               , ( toEntityId 697, GeoLocation "Gisenyi" (Just <| toEntityId 696) )
               , ( toEntityId 698, GeoLocation "Gisiza" (Just <| toEntityId 696) )
               , ( toEntityId 699, GeoLocation "Kanzuki" (Just <| toEntityId 696) )
               , ( toEntityId 700, GeoLocation "Nyakagezi" (Just <| toEntityId 696) )
               , ( toEntityId 702, GeoLocation "Kineza" (Just <| toEntityId 701) )
               , ( toEntityId 703, GeoLocation "Mataba" (Just <| toEntityId 701) )
               , ( toEntityId 704, GeoLocation "Mwifuzo" (Just <| toEntityId 701) )
               , ( toEntityId 705, GeoLocation "Nyagasozi" (Just <| toEntityId 701) )
               , ( toEntityId 706, GeoLocation "Rugarama" (Just <| toEntityId 701) )
               , ( toEntityId 708, GeoLocation "Kabeza" (Just <| toEntityId 707) )
               , ( toEntityId 709, GeoLocation "Kabona" (Just <| toEntityId 707) )
               , ( toEntityId 710, GeoLocation "Karambi" (Just <| toEntityId 707) )
               , ( toEntityId 711, GeoLocation "Murambi" (Just <| toEntityId 707) )
               , ( toEntityId 712, GeoLocation "Nganzo" (Just <| toEntityId 707) )
               , ( toEntityId 714, GeoLocation "Bushoka" (Just <| toEntityId 713) )
               , ( toEntityId 715, GeoLocation "Buzoza" (Just <| toEntityId 713) )
               , ( toEntityId 716, GeoLocation "Gisanze" (Just <| toEntityId 713) )
               , ( toEntityId 717, GeoLocation "Gitongo" (Just <| toEntityId 713) )
               , ( toEntityId 718, GeoLocation "Nyabitare" (Just <| toEntityId 713) )
               , ( toEntityId 720, GeoLocation "Gahinga" (Just <| toEntityId 719) )
               , ( toEntityId 721, GeoLocation "Gikongoro" (Just <| toEntityId 719) )
               , ( toEntityId 722, GeoLocation "Kirwa" (Just <| toEntityId 719) )
               , ( toEntityId 723, GeoLocation "Nkoto" (Just <| toEntityId 719) )
               , ( toEntityId 724, GeoLocation "Nyangoyi" (Just <| toEntityId 719) )
               , ( toEntityId 726, GeoLocation "Giheta" (Just <| toEntityId 725) )
               , ( toEntityId 727, GeoLocation "Karushashi" (Just <| toEntityId 725) )
               , ( toEntityId 728, GeoLocation "Ngambi" (Just <| toEntityId 725) )
               , ( toEntityId 729, GeoLocation "Ruganda" (Just <| toEntityId 725) )
               , ( toEntityId 730, GeoLocation "Rwamabega" (Just <| toEntityId 725) )
               , ( toEntityId 732, GeoLocation "Gatare" (Just <| toEntityId 731) )
               , ( toEntityId 733, GeoLocation "Gatwa" (Just <| toEntityId 731) )
               , ( toEntityId 734, GeoLocation "Gihororo" (Just <| toEntityId 731) )
               , ( toEntityId 735, GeoLocation "Murara" (Just <| toEntityId 731) )
               , ( toEntityId 739, GeoLocation "Base" (Just <| toEntityId 738) )
               , ( toEntityId 740, GeoLocation "Cyondo" (Just <| toEntityId 738) )
               , ( toEntityId 741, GeoLocation "Gitovu" (Just <| toEntityId 738) )
               , ( toEntityId 742, GeoLocation "Kabahama" (Just <| toEntityId 738) )
               , ( toEntityId 743, GeoLocation "Kabeza" (Just <| toEntityId 738) )
               , ( toEntityId 744, GeoLocation "Karambi" (Just <| toEntityId 738) )
               , ( toEntityId 745, GeoLocation "Kiruli" (Just <| toEntityId 738) )
               , ( toEntityId 746, GeoLocation "Mutima" (Just <| toEntityId 738) )
               , ( toEntityId 748, GeoLocation "Bukangano" (Just <| toEntityId 747) )
               , ( toEntityId 749, GeoLocation "Buramba" (Just <| toEntityId 747) )
               , ( toEntityId 750, GeoLocation "Gihemba" (Just <| toEntityId 747) )
               , ( toEntityId 751, GeoLocation "Gitwa" (Just <| toEntityId 747) )
               , ( toEntityId 752, GeoLocation "Kabingo" (Just <| toEntityId 747) )
               , ( toEntityId 753, GeoLocation "Kabuga" (Just <| toEntityId 747) )
               , ( toEntityId 754, GeoLocation "Musenyi" (Just <| toEntityId 747) )
               , ( toEntityId 755, GeoLocation "Mushongi" (Just <| toEntityId 747) )
               , ( toEntityId 756, GeoLocation "Nyangoyi" (Just <| toEntityId 747) )
               , ( toEntityId 757, GeoLocation "Rubanda" (Just <| toEntityId 747) )
               , ( toEntityId 759, GeoLocation "Bushyiga" (Just <| toEntityId 758) )
               , ( toEntityId 760, GeoLocation "Gatete" (Just <| toEntityId 758) )
               , ( toEntityId 761, GeoLocation "Gihora" (Just <| toEntityId 758) )
               , ( toEntityId 762, GeoLocation "Gisiza" (Just <| toEntityId 758) )
               , ( toEntityId 763, GeoLocation "Kirwa" (Just <| toEntityId 758) )
               , ( toEntityId 764, GeoLocation "Mugenda I" (Just <| toEntityId 758) )
               , ( toEntityId 765, GeoLocation "Mugenda Ii" (Just <| toEntityId 758) )
               , ( toEntityId 766, GeoLocation "Nyamugali" (Just <| toEntityId 758) )
               , ( toEntityId 767, GeoLocation "Rugaragara" (Just <| toEntityId 758) )
               , ( toEntityId 768, GeoLocation "Rugerero" (Just <| toEntityId 758) )
               , ( toEntityId 771, GeoLocation "Gacyamo" (Just <| toEntityId 770) )
               , ( toEntityId 772, GeoLocation "Gashinge" (Just <| toEntityId 770) )
               , ( toEntityId 773, GeoLocation "Karambi" (Just <| toEntityId 770) )
               , ( toEntityId 774, GeoLocation "Karugaju" (Just <| toEntityId 770) )
               , ( toEntityId 775, GeoLocation "Kerera" (Just <| toEntityId 770) )
               , ( toEntityId 776, GeoLocation "Kibiraro" (Just <| toEntityId 770) )
               , ( toEntityId 777, GeoLocation "Kigabiro" (Just <| toEntityId 770) )
               , ( toEntityId 778, GeoLocation "Kigarama" (Just <| toEntityId 770) )
               , ( toEntityId 779, GeoLocation "Kisigiro" (Just <| toEntityId 770) )
               , ( toEntityId 780, GeoLocation "Mayaga" (Just <| toEntityId 770) )
               , ( toEntityId 781, GeoLocation "Muduha" (Just <| toEntityId 770) )
               , ( toEntityId 782, GeoLocation "Muhondo" (Just <| toEntityId 770) )
               , ( toEntityId 783, GeoLocation "Nyamiyaga" (Just <| toEntityId 770) )
               , ( toEntityId 784, GeoLocation "Runyinya" (Just <| toEntityId 770) )
               , ( toEntityId 786, GeoLocation "Bugoboka" (Just <| toEntityId 785) )
               , ( toEntityId 787, GeoLocation "Byerwa" (Just <| toEntityId 785) )
               , ( toEntityId 788, GeoLocation "Gasare" (Just <| toEntityId 785) )
               , ( toEntityId 789, GeoLocation "Gasharu" (Just <| toEntityId 785) )
               , ( toEntityId 790, GeoLocation "Gashinge" (Just <| toEntityId 785) )
               , ( toEntityId 791, GeoLocation "Gatete" (Just <| toEntityId 785) )
               , ( toEntityId 792, GeoLocation "Kantabo" (Just <| toEntityId 785) )
               , ( toEntityId 793, GeoLocation "Kanunga" (Just <| toEntityId 785) )
               , ( toEntityId 794, GeoLocation "Kizenga" (Just <| toEntityId 785) )
               , ( toEntityId 795, GeoLocation "Kiziba" (Just <| toEntityId 785) )
               , ( toEntityId 796, GeoLocation "Mataba" (Just <| toEntityId 785) )
               , ( toEntityId 797, GeoLocation "Mitabi" (Just <| toEntityId 785) )
               , ( toEntityId 798, GeoLocation "Mukarange" (Just <| toEntityId 785) )
               , ( toEntityId 799, GeoLocation "Rwamiko" (Just <| toEntityId 785) )
               , ( toEntityId 801, GeoLocation "Bugarama" (Just <| toEntityId 800) )
               , ( toEntityId 802, GeoLocation "Cyinzuzi" (Just <| toEntityId 800) )
               , ( toEntityId 803, GeoLocation "Gasango" (Just <| toEntityId 800) )
               , ( toEntityId 804, GeoLocation "Kiboha" (Just <| toEntityId 800) )
               , ( toEntityId 805, GeoLocation "Kivomo" (Just <| toEntityId 800) )
               , ( toEntityId 806, GeoLocation "Mwenene" (Just <| toEntityId 800) )
               , ( toEntityId 807, GeoLocation "Mwite" (Just <| toEntityId 800) )
               , ( toEntityId 808, GeoLocation "Ngange" (Just <| toEntityId 800) )
               , ( toEntityId 809, GeoLocation "Nyagisozi" (Just <| toEntityId 800) )
               , ( toEntityId 810, GeoLocation "Rubara" (Just <| toEntityId 800) )
               , ( toEntityId 811, GeoLocation "Rusine" (Just <| toEntityId 800) )
               , ( toEntityId 812, GeoLocation "Ryinzovu" (Just <| toEntityId 800) )
               , ( toEntityId 815, GeoLocation "Gitwa" (Just <| toEntityId 814) )
               , ( toEntityId 816, GeoLocation "Karambi" (Just <| toEntityId 814) )
               , ( toEntityId 817, GeoLocation "Remera" (Just <| toEntityId 814) )
               , ( toEntityId 818, GeoLocation "Ruhanga" (Just <| toEntityId 814) )
               , ( toEntityId 819, GeoLocation "Rulindo" (Just <| toEntityId 814) )
               , ( toEntityId 854, GeoLocation "Budaha" (Just <| toEntityId 814) )
               , ( toEntityId 855, GeoLocation "Buhande" (Just <| toEntityId 814) )
               , ( toEntityId 821, GeoLocation "Buramira" (Just <| toEntityId 820) )
               , ( toEntityId 822, GeoLocation "Cyiri" (Just <| toEntityId 820) )
               , ( toEntityId 823, GeoLocation "Gashiru" (Just <| toEntityId 820) )
               , ( toEntityId 824, GeoLocation "Karambo" (Just <| toEntityId 820) )
               , ( toEntityId 825, GeoLocation "Kigamba" (Just <| toEntityId 820) )
               , ( toEntityId 826, GeoLocation "Kivomo" (Just <| toEntityId 820) )
               , ( toEntityId 827, GeoLocation "Ngarama" (Just <| toEntityId 820) )
               , ( toEntityId 828, GeoLocation "Rugote" (Just <| toEntityId 820) )
               , ( toEntityId 830, GeoLocation "Gitaba" (Just <| toEntityId 829) )
               , ( toEntityId 831, GeoLocation "Muduha" (Just <| toEntityId 829) )
               , ( toEntityId 832, GeoLocation "Murambo" (Just <| toEntityId 829) )
               , ( toEntityId 833, GeoLocation "Rebero" (Just <| toEntityId 829) )
               , ( toEntityId 834, GeoLocation "Rwanzu" (Just <| toEntityId 829) )
               , ( toEntityId 836, GeoLocation "Buvumo" (Just <| toEntityId 835) )
               , ( toEntityId 837, GeoLocation "Buyogoma" (Just <| toEntityId 835) )
               , ( toEntityId 838, GeoLocation "Gatare" (Just <| toEntityId 835) )
               , ( toEntityId 839, GeoLocation "Marembo" (Just <| toEntityId 835) )
               , ( toEntityId 840, GeoLocation "Muko" (Just <| toEntityId 835) )
               , ( toEntityId 841, GeoLocation "Mukoto" (Just <| toEntityId 835) )
               , ( toEntityId 842, GeoLocation "Rusave" (Just <| toEntityId 835) )
               , ( toEntityId 844, GeoLocation "Bubiro" (Just <| toEntityId 843) )
               , ( toEntityId 845, GeoLocation "Byimana" (Just <| toEntityId 843) )
               , ( toEntityId 846, GeoLocation "Gatenga" (Just <| toEntityId 843) )
               , ( toEntityId 847, GeoLocation "Gifuba" (Just <| toEntityId 843) )
               , ( toEntityId 848, GeoLocation "Karambi" (Just <| toEntityId 843) )
               , ( toEntityId 849, GeoLocation "Nyenyeri" (Just <| toEntityId 843) )
               , ( toEntityId 850, GeoLocation "Nyirangarama" (Just <| toEntityId 843) )
               , ( toEntityId 851, GeoLocation "Remera" (Just <| toEntityId 843) )
               , ( toEntityId 852, GeoLocation "Tare" (Just <| toEntityId 843) )
               , ( toEntityId 853, GeoLocation "Terambere" (Just <| toEntityId 843) )
               , ( toEntityId 858, GeoLocation "Gashana" (Just <| toEntityId 857) )
               , ( toEntityId 859, GeoLocation "Gatwa" (Just <| toEntityId 857) )
               , ( toEntityId 860, GeoLocation "Karambo" (Just <| toEntityId 857) )
               , ( toEntityId 861, GeoLocation "Kibanda" (Just <| toEntityId 857) )
               , ( toEntityId 862, GeoLocation "Rugarama" (Just <| toEntityId 857) )
               , ( toEntityId 864, GeoLocation "Gasave" (Just <| toEntityId 863) )
               , ( toEntityId 865, GeoLocation "Giko" (Just <| toEntityId 863) )
               , ( toEntityId 866, GeoLocation "Kankanga" (Just <| toEntityId 863) )
               , ( toEntityId 867, GeoLocation "Karambi" (Just <| toEntityId 863) )
               , ( toEntityId 868, GeoLocation "Ryanyirakayobe" (Just <| toEntityId 863) )
               , ( toEntityId 870, GeoLocation "Bunyana" (Just <| toEntityId 869) )
               , ( toEntityId 871, GeoLocation "Gatare" (Just <| toEntityId 869) )
               , ( toEntityId 872, GeoLocation "Gatenderi" (Just <| toEntityId 869) )
               , ( toEntityId 873, GeoLocation "Gipfundo" (Just <| toEntityId 869) )
               , ( toEntityId 874, GeoLocation "Gitabura" (Just <| toEntityId 869) )
               , ( toEntityId 875, GeoLocation "Shagasha" (Just <| toEntityId 869) )
               , ( toEntityId 877, GeoLocation "Gitaba" (Just <| toEntityId 876) )
               , ( toEntityId 878, GeoLocation "Munini" (Just <| toEntityId 876) )
               , ( toEntityId 879, GeoLocation "Nyarubuye" (Just <| toEntityId 876) )
               , ( toEntityId 880, GeoLocation "Remera" (Just <| toEntityId 876) )
               , ( toEntityId 881, GeoLocation "Rutabo" (Just <| toEntityId 876) )
               , ( toEntityId 883, GeoLocation "Cyasenge" (Just <| toEntityId 882) )
               , ( toEntityId 884, GeoLocation "Kajeneni" (Just <| toEntityId 882) )
               , ( toEntityId 885, GeoLocation "Karambi" (Just <| toEntityId 882) )
               , ( toEntityId 886, GeoLocation "Karambo" (Just <| toEntityId 882) )
               , ( toEntityId 887, GeoLocation "Kavumo" (Just <| toEntityId 882) )
               , ( toEntityId 888, GeoLocation "Kigarama" (Just <| toEntityId 882) )
               , ( toEntityId 890, GeoLocation "Gakoma" (Just <| toEntityId 889) )
               , ( toEntityId 891, GeoLocation "Mataba" (Just <| toEntityId 889) )
               , ( toEntityId 892, GeoLocation "Murambo" (Just <| toEntityId 889) )
               , ( toEntityId 893, GeoLocation "Nyamwiza" (Just <| toEntityId 889) )
               , ( toEntityId 894, GeoLocation "Nyarubuye" (Just <| toEntityId 889) )
               , ( toEntityId 896, GeoLocation "Gahondo" (Just <| toEntityId 895) )
               , ( toEntityId 897, GeoLocation "Gikingo" (Just <| toEntityId 895) )
               , ( toEntityId 898, GeoLocation "Kagozi" (Just <| toEntityId 895) )
               , ( toEntityId 899, GeoLocation "Karambi" (Just <| toEntityId 895) )
               , ( toEntityId 900, GeoLocation "Kimagali" (Just <| toEntityId 895) )
               , ( toEntityId 903, GeoLocation "Gatagara" (Just <| toEntityId 902) )
               , ( toEntityId 904, GeoLocation "Gihinga" (Just <| toEntityId 902) )
               , ( toEntityId 905, GeoLocation "Kamatongo" (Just <| toEntityId 902) )
               , ( toEntityId 906, GeoLocation "Kanyoni" (Just <| toEntityId 902) )
               , ( toEntityId 907, GeoLocation "Kavumu" (Just <| toEntityId 902) )
               , ( toEntityId 908, GeoLocation "Kigarama" (Just <| toEntityId 902) )
               , ( toEntityId 909, GeoLocation "Nyakabanga" (Just <| toEntityId 902) )
               , ( toEntityId 910, GeoLocation "Rugaragara" (Just <| toEntityId 902) )
               , ( toEntityId 912, GeoLocation "Cyanya" (Just <| toEntityId 911) )
               ]
            ++ [ ( toEntityId 913, GeoLocation "Gitabage" (Just <| toEntityId 911) )
               , ( toEntityId 914, GeoLocation "Karambo" (Just <| toEntityId 911) )
               , ( toEntityId 915, GeoLocation "Marembo" (Just <| toEntityId 911) )
               , ( toEntityId 916, GeoLocation "Ngabitsinze" (Just <| toEntityId 911) )
               , ( toEntityId 917, GeoLocation "Nyamugali" (Just <| toEntityId 911) )
               , ( toEntityId 918, GeoLocation "Remera" (Just <| toEntityId 911) )
               , ( toEntityId 919, GeoLocation "Rusagara" (Just <| toEntityId 911) )
               , ( toEntityId 921, GeoLocation "Gasekabuye" (Just <| toEntityId 920) )
               , ( toEntityId 922, GeoLocation "Gaseke" (Just <| toEntityId 920) )
               , ( toEntityId 923, GeoLocation "Gasizi" (Just <| toEntityId 920) )
               , ( toEntityId 924, GeoLocation "Gihuke" (Just <| toEntityId 920) )
               , ( toEntityId 925, GeoLocation "Kirambo" (Just <| toEntityId 920) )
               , ( toEntityId 926, GeoLocation "Munini" (Just <| toEntityId 920) )
               , ( toEntityId 927, GeoLocation "Munoga" (Just <| toEntityId 920) )
               , ( toEntityId 928, GeoLocation "Musenyi" (Just <| toEntityId 920) )
               , ( toEntityId 931, GeoLocation "Kibogora" (Just <| toEntityId 930) )
               , ( toEntityId 932, GeoLocation "Nyagatovu" (Just <| toEntityId 930) )
               , ( toEntityId 933, GeoLocation "Sove" (Just <| toEntityId 930) )
               , ( toEntityId 951, GeoLocation "Gitandi" (Just <| toEntityId 930) )
               , ( toEntityId 952, GeoLocation "Karambo" (Just <| toEntityId 930) )
               , ( toEntityId 953, GeoLocation "Karengeri" (Just <| toEntityId 930) )
               , ( toEntityId 954, GeoLocation "Kibande" (Just <| toEntityId 930) )
               , ( toEntityId 935, GeoLocation "Buyaga" (Just <| toEntityId 934) )
               , ( toEntityId 936, GeoLocation "Gahinga" (Just <| toEntityId 934) )
               , ( toEntityId 937, GeoLocation "Kibuye" (Just <| toEntityId 934) )
               , ( toEntityId 938, GeoLocation "Kidomo" (Just <| toEntityId 934) )
               , ( toEntityId 939, GeoLocation "Murambo" (Just <| toEntityId 934) )
               , ( toEntityId 940, GeoLocation "Nganzo" (Just <| toEntityId 934) )
               , ( toEntityId 941, GeoLocation "Rugaragara" (Just <| toEntityId 934) )
               , ( toEntityId 942, GeoLocation "Rusayu" (Just <| toEntityId 934) )
               , ( toEntityId 944, GeoLocation "Kabanda" (Just <| toEntityId 943) )
               , ( toEntityId 945, GeoLocation "Karambi" (Just <| toEntityId 943) )
               , ( toEntityId 946, GeoLocation "Kirwa" (Just <| toEntityId 943) )
               , ( toEntityId 947, GeoLocation "Kivumu" (Just <| toEntityId 943) )
               , ( toEntityId 948, GeoLocation "Nturo" (Just <| toEntityId 943) )
               , ( toEntityId 949, GeoLocation "Nyabisasa" (Just <| toEntityId 943) )
               , ( toEntityId 950, GeoLocation "Sakara" (Just <| toEntityId 943) )
               , ( toEntityId 957, GeoLocation "Akamiyove" (Just <| toEntityId 956) )
               , ( toEntityId 958, GeoLocation "Barayi" (Just <| toEntityId 956) )
               , ( toEntityId 959, GeoLocation "Bunahi" (Just <| toEntityId 956) )
               , ( toEntityId 960, GeoLocation "Gisekuru" (Just <| toEntityId 956) )
               , ( toEntityId 961, GeoLocation "Kinihira" (Just <| toEntityId 956) )
               , ( toEntityId 962, GeoLocation "Ndorandi" (Just <| toEntityId 956) )
               , ( toEntityId 964, GeoLocation "Buhita" (Just <| toEntityId 963) )
               , ( toEntityId 965, GeoLocation "Bwishya" (Just <| toEntityId 963) )
               , ( toEntityId 966, GeoLocation "Gatembe" (Just <| toEntityId 963) )
               , ( toEntityId 967, GeoLocation "Magezi" (Just <| toEntityId 963) )
               , ( toEntityId 968, GeoLocation "Mutoyi" (Just <| toEntityId 963) )
               , ( toEntityId 969, GeoLocation "Ntunguru" (Just <| toEntityId 963) )
               , ( toEntityId 971, GeoLocation "Buhunde" (Just <| toEntityId 970) )
               , ( toEntityId 972, GeoLocation "Cyogo" (Just <| toEntityId 970) )
               , ( toEntityId 973, GeoLocation "Gatare" (Just <| toEntityId 970) )
               , ( toEntityId 974, GeoLocation "Kigali" (Just <| toEntityId 970) )
               , ( toEntityId 975, GeoLocation "Kiyebe" (Just <| toEntityId 970) )
               , ( toEntityId 977, GeoLocation "Kabuga" (Just <| toEntityId 976) )
               , ( toEntityId 978, GeoLocation "Karambi" (Just <| toEntityId 976) )
               , ( toEntityId 979, GeoLocation "Kirwa" (Just <| toEntityId 976) )
               , ( toEntityId 980, GeoLocation "Ndusu" (Just <| toEntityId 976) )
               , ( toEntityId 981, GeoLocation "Rugundu" (Just <| toEntityId 976) )
               , ( toEntityId 982, GeoLocation "Taba" (Just <| toEntityId 976) )
               , ( toEntityId 985, GeoLocation "Songa" (Just <| toEntityId 984) )
               , ( toEntityId 986, GeoLocation "Wamahoro" (Just <| toEntityId 984) )
               , ( toEntityId 1019, GeoLocation "Gakenke" (Just <| toEntityId 984) )
               , ( toEntityId 1020, GeoLocation "Gatete" (Just <| toEntityId 984) )
               , ( toEntityId 1021, GeoLocation "Gatovu" (Just <| toEntityId 984) )
               , ( toEntityId 1022, GeoLocation "Kabeza" (Just <| toEntityId 984) )
               , ( toEntityId 1023, GeoLocation "Karambi" (Just <| toEntityId 984) )
               , ( toEntityId 988, GeoLocation "Gaseke" (Just <| toEntityId 987) )
               , ( toEntityId 989, GeoLocation "Gasharu" (Just <| toEntityId 987) )
               , ( toEntityId 990, GeoLocation "Nyantabo" (Just <| toEntityId 987) )
               , ( toEntityId 991, GeoLocation "Runyinya" (Just <| toEntityId 987) )
               , ( toEntityId 992, GeoLocation "Rwintare" (Just <| toEntityId 987) )
               , ( toEntityId 994, GeoLocation "Gako" (Just <| toEntityId 993) )
               , ( toEntityId 995, GeoLocation "Kibuye" (Just <| toEntityId 993) )
               , ( toEntityId 996, GeoLocation "Kirenge" (Just <| toEntityId 993) )
               , ( toEntityId 997, GeoLocation "Murambi" (Just <| toEntityId 993) )
               , ( toEntityId 998, GeoLocation "Nyakarekare" (Just <| toEntityId 993) )
               , ( toEntityId 999, GeoLocation "Rutabo" (Just <| toEntityId 993) )
               , ( toEntityId 1001, GeoLocation "Akamanama" (Just <| toEntityId 1000) )
               , ( toEntityId 1002, GeoLocation "Gishinge" (Just <| toEntityId 1000) )
               , ( toEntityId 1003, GeoLocation "Karambi" (Just <| toEntityId 1000) )
               , ( toEntityId 1004, GeoLocation "Kibingwe" (Just <| toEntityId 1000) )
               , ( toEntityId 1005, GeoLocation "Mugomero" (Just <| toEntityId 1000) )
               , ( toEntityId 1006, GeoLocation "Ryarubuguza" (Just <| toEntityId 1000) )
               , ( toEntityId 1008, GeoLocation "Cyasuri" (Just <| toEntityId 1007) )
               , ( toEntityId 1009, GeoLocation "Kibanda" (Just <| toEntityId 1007) )
               , ( toEntityId 1010, GeoLocation "Nyamiyaga" (Just <| toEntityId 1007) )
               , ( toEntityId 1011, GeoLocation "Rugarama" (Just <| toEntityId 1007) )
               , ( toEntityId 1012, GeoLocation "Rusongati" (Just <| toEntityId 1007) )
               , ( toEntityId 1013, GeoLocation "Rusumo" (Just <| toEntityId 1007) )
               , ( toEntityId 1015, GeoLocation "Kabere" (Just <| toEntityId 1014) )
               , ( toEntityId 1016, GeoLocation "Ndago" (Just <| toEntityId 1014) )
               , ( toEntityId 1017, GeoLocation "Ruberano" (Just <| toEntityId 1014) )
               , ( toEntityId 1018, GeoLocation "Rwili" (Just <| toEntityId 1014) )
               , ( toEntityId 1026, GeoLocation "Umutagata" (Just <| toEntityId 1025) )
               , ( toEntityId 1051, GeoLocation "Agasharu" (Just <| toEntityId 1025) )
               , ( toEntityId 1052, GeoLocation "Amataba" (Just <| toEntityId 1025) )
               , ( toEntityId 1053, GeoLocation "Nyabinyana" (Just <| toEntityId 1025) )
               , ( toEntityId 1054, GeoLocation "Rusine" (Just <| toEntityId 1025) )
               , ( toEntityId 1055, GeoLocation "Umubuga" (Just <| toEntityId 1025) )
               , ( toEntityId 1028, GeoLocation "Gisiza" (Just <| toEntityId 1027) )
               , ( toEntityId 1029, GeoLocation "Kanunga" (Just <| toEntityId 1027) )
               , ( toEntityId 1030, GeoLocation "Karambi" (Just <| toEntityId 1027) )
               , ( toEntityId 1031, GeoLocation "Kigarama" (Just <| toEntityId 1027) )
               , ( toEntityId 1032, GeoLocation "Nyakibande" (Just <| toEntityId 1027) )
               , ( toEntityId 1033, GeoLocation "Nyakizu" (Just <| toEntityId 1027) )
               , ( toEntityId 1034, GeoLocation "Rubaya" (Just <| toEntityId 1027) )
               , ( toEntityId 1036, GeoLocation "Gacyamo" (Just <| toEntityId 1035) )
               , ( toEntityId 1037, GeoLocation "Marenge" (Just <| toEntityId 1035) )
               , ( toEntityId 1038, GeoLocation "Nyakabungo" (Just <| toEntityId 1035) )
               , ( toEntityId 1039, GeoLocation "Rukurazo" (Just <| toEntityId 1035) )
               , ( toEntityId 1041, GeoLocation "Gasenga" (Just <| toEntityId 1040) )
               , ( toEntityId 1042, GeoLocation "Musega" (Just <| toEntityId 1040) )
               , ( toEntityId 1043, GeoLocation "Nyarurembo" (Just <| toEntityId 1040) )
               , ( toEntityId 1044, GeoLocation "Rebero" (Just <| toEntityId 1040) )
               , ( toEntityId 1046, GeoLocation "Kabeza" (Just <| toEntityId 1045) )
               , ( toEntityId 1047, GeoLocation "Kabuga" (Just <| toEntityId 1045) )
               , ( toEntityId 1048, GeoLocation "Kigomwa" (Just <| toEntityId 1045) )
               , ( toEntityId 1049, GeoLocation "Marembo" (Just <| toEntityId 1045) )
               , ( toEntityId 1050, GeoLocation "Rusenyi" (Just <| toEntityId 1045) )
               , ( toEntityId 1058, GeoLocation "Buhira" (Just <| toEntityId 1057) )
               , ( toEntityId 1059, GeoLocation "Bukoro" (Just <| toEntityId 1057) )
               , ( toEntityId 1060, GeoLocation "Gasama" (Just <| toEntityId 1057) )
               , ( toEntityId 1061, GeoLocation "Gihonga" (Just <| toEntityId 1057) )
               , ( toEntityId 1062, GeoLocation "Kalindi" (Just <| toEntityId 1057) )
               , ( toEntityId 1063, GeoLocation "Kibamba" (Just <| toEntityId 1057) )
               , ( toEntityId 1064, GeoLocation "Kibaya" (Just <| toEntityId 1057) )
               , ( toEntityId 1065, GeoLocation "Kinini Ya Mbogo" (Just <| toEntityId 1057) )
               , ( toEntityId 1066, GeoLocation "Ruhanya" (Just <| toEntityId 1057) )
               , ( toEntityId 1067, GeoLocation "Rwambogo" (Just <| toEntityId 1057) )
               , ( toEntityId 1069, GeoLocation "Bukongi" (Just <| toEntityId 1068) )
               , ( toEntityId 1070, GeoLocation "Buraro" (Just <| toEntityId 1068) )
               , ( toEntityId 1071, GeoLocation "Buyanja" (Just <| toEntityId 1068) )
               , ( toEntityId 1072, GeoLocation "Gitaba" (Just <| toEntityId 1068) )
               , ( toEntityId 1073, GeoLocation "Nkurura" (Just <| toEntityId 1068) )
               , ( toEntityId 1074, GeoLocation "Nyakabuye" (Just <| toEntityId 1068) )
               , ( toEntityId 1075, GeoLocation "Rwambogo" (Just <| toEntityId 1068) )
               , ( toEntityId 1077, GeoLocation "Gasovu" (Just <| toEntityId 1076) )
               , ( toEntityId 1078, GeoLocation "Gikombe" (Just <| toEntityId 1076) )
               , ( toEntityId 1079, GeoLocation "Yaramba" (Just <| toEntityId 1076) )
               , ( toEntityId 1080, GeoLocation "Kibungo" (Just <| toEntityId 1076) )
               , ( toEntityId 1081, GeoLocation "Muhora" (Just <| toEntityId 1076) )
               , ( toEntityId 1082, GeoLocation "Nyakabembe" (Just <| toEntityId 1076) )
               , ( toEntityId 1083, GeoLocation "Gisha" (Just <| toEntityId 1076) )
               , ( toEntityId 1085, GeoLocation "Gakoma" (Just <| toEntityId 1084) )
               , ( toEntityId 1086, GeoLocation "Gicumbi" (Just <| toEntityId 1084) )
               , ( toEntityId 1087, GeoLocation "Gitaba" (Just <| toEntityId 1084) )
               , ( toEntityId 1088, GeoLocation "Karehe" (Just <| toEntityId 1084) )
               , ( toEntityId 1089, GeoLocation "Munini" (Just <| toEntityId 1084) )
               , ( toEntityId 1090, GeoLocation "Ruhondo" (Just <| toEntityId 1084) )
               , ( toEntityId 1091, GeoLocation "Rurenge" (Just <| toEntityId 1084) )
               , ( toEntityId 1092, GeoLocation "Rutonde" (Just <| toEntityId 1084) )
               , ( toEntityId 1095, GeoLocation "Gahama" (Just <| toEntityId 1094) )
               , ( toEntityId 1096, GeoLocation "Gashinge" (Just <| toEntityId 1094) )
               , ( toEntityId 1097, GeoLocation "Kigarama" (Just <| toEntityId 1094) )
               , ( toEntityId 1098, GeoLocation "Nyarurembo" (Just <| toEntityId 1094) )
               , ( toEntityId 1099, GeoLocation "Ruri" (Just <| toEntityId 1094) )
               , ( toEntityId 1127, GeoLocation "Amahoro" (Just <| toEntityId 1094) )
               , ( toEntityId 1128, GeoLocation "Buliza" (Just <| toEntityId 1094) )
               , ( toEntityId 1101, GeoLocation "Iraro" (Just <| toEntityId 1100) )
               , ( toEntityId 1102, GeoLocation "Kabeza" (Just <| toEntityId 1100) )
               , ( toEntityId 1103, GeoLocation "Kabuga" (Just <| toEntityId 1100) )
               , ( toEntityId 1104, GeoLocation "Munyinya" (Just <| toEntityId 1100) )
               , ( toEntityId 1105, GeoLocation "Mutabo" (Just <| toEntityId 1100) )
               , ( toEntityId 1106, GeoLocation "Ntyaba" (Just <| toEntityId 1100) )
               , ( toEntityId 1107, GeoLocation "Rurama" (Just <| toEntityId 1100) )
               , ( toEntityId 1109, GeoLocation "Gashubi" (Just <| toEntityId 1108) )
               , ( toEntityId 1110, GeoLocation "Karambo" (Just <| toEntityId 1108) )
               , ( toEntityId 1111, GeoLocation "Karwa" (Just <| toEntityId 1108) )
               , ( toEntityId 1112, GeoLocation "Mayange" (Just <| toEntityId 1108) )
               , ( toEntityId 1113, GeoLocation "Nyagisozi" (Just <| toEntityId 1108) )
               , ( toEntityId 1114, GeoLocation "Rebero" (Just <| toEntityId 1108) )
               , ( toEntityId 1117, GeoLocation "Ruhunga" (Just <| toEntityId 1116) )
               , ( toEntityId 1118, GeoLocation "Taba" (Just <| toEntityId 1116) )
               , ( toEntityId 1120, GeoLocation "Agatare" (Just <| toEntityId 1119) )
               , ( toEntityId 1121, GeoLocation "Akarambi" (Just <| toEntityId 1119) )
               , ( toEntityId 1122, GeoLocation "Amataba" (Just <| toEntityId 1119) )
               , ( toEntityId 1123, GeoLocation "Gisiza" (Just <| toEntityId 1119) )
               , ( toEntityId 1124, GeoLocation "Kabeza" (Just <| toEntityId 1119) )
               , ( toEntityId 1125, GeoLocation "Karambo" (Just <| toEntityId 1119) )
               , ( toEntityId 1126, GeoLocation "Kigarama" (Just <| toEntityId 1119) )
               , ( toEntityId 1131, GeoLocation "Gatete" (Just <| toEntityId 1130) )
               , ( toEntityId 1132, GeoLocation "Kagarama" (Just <| toEntityId 1130) )
               , ( toEntityId 1133, GeoLocation "Kirambo" (Just <| toEntityId 1130) )
               , ( toEntityId 1134, GeoLocation "Kiruli" (Just <| toEntityId 1130) )
               , ( toEntityId 1135, GeoLocation "Nyabuko" (Just <| toEntityId 1130) )
               , ( toEntityId 1136, GeoLocation "Rubona" (Just <| toEntityId 1130) )
               , ( toEntityId 1138, GeoLocation "Butare" (Just <| toEntityId 1137) )
               , ( toEntityId 1139, GeoLocation "Jyambere" (Just <| toEntityId 1137) )
               , ( toEntityId 1140, GeoLocation "Kagwa" (Just <| toEntityId 1137) )
               , ( toEntityId 1141, GeoLocation "Karambi" (Just <| toEntityId 1137) )
               , ( toEntityId 1142, GeoLocation "Marebe" (Just <| toEntityId 1137) )
               , ( toEntityId 1143, GeoLocation "Nyakagezi" (Just <| toEntityId 1137) )
               , ( toEntityId 1145, GeoLocation "Cyabasigi" (Just <| toEntityId 1144) )
               , ( toEntityId 1146, GeoLocation "Kiboha" (Just <| toEntityId 1144) )
               , ( toEntityId 1147, GeoLocation "Kigina" (Just <| toEntityId 1144) )
               , ( toEntityId 1148, GeoLocation "Mwishya" (Just <| toEntityId 1144) )
               , ( toEntityId 1149, GeoLocation "Nyakibyeyi" (Just <| toEntityId 1144) )
               , ( toEntityId 1150, GeoLocation "Riryi" (Just <| toEntityId 1144) )
               , ( toEntityId 1151, GeoLocation "Rukoma" (Just <| toEntityId 1144) )
               , ( toEntityId 1152, GeoLocation "Sakara" (Just <| toEntityId 1144) )
               , ( toEntityId 1154, GeoLocation "Busizi" (Just <| toEntityId 1153) )
               , ( toEntityId 1155, GeoLocation "Gaseke" (Just <| toEntityId 1153) )
               , ( toEntityId 1156, GeoLocation "Kirungu" (Just <| toEntityId 1153) )
               , ( toEntityId 1157, GeoLocation "Muyange" (Just <| toEntityId 1153) )
               , ( toEntityId 1158, GeoLocation "Ngaru" (Just <| toEntityId 1153) )
               , ( toEntityId 1159, GeoLocation "Nyaruvumu" (Just <| toEntityId 1153) )
               , ( toEntityId 1160, GeoLocation "Rushayu" (Just <| toEntityId 1153) )
               , ( toEntityId 1161, GeoLocation "Rushubi" (Just <| toEntityId 1153) )
               , ( toEntityId 1164, GeoLocation "Kivubwe" (Just <| toEntityId 1163) )
               , ( toEntityId 1165, GeoLocation "Kiyanza I" (Just <| toEntityId 1163) )
               , ( toEntityId 1166, GeoLocation "Nombe" (Just <| toEntityId 1163) )
               , ( toEntityId 1167, GeoLocation "Nyagisozi" (Just <| toEntityId 1163) )
               , ( toEntityId 1168, GeoLocation "Nyamurema" (Just <| toEntityId 1163) )
               , ( toEntityId 1169, GeoLocation "Nyarurama" (Just <| toEntityId 1163) )
               , ( toEntityId 1187, GeoLocation "Gatobotobo" (Just <| toEntityId 1163) )
               , ( toEntityId 1188, GeoLocation "Kabirizi" (Just <| toEntityId 1163) )
               , ( toEntityId 1171, GeoLocation "Burambi" (Just <| toEntityId 1170) )
               , ( toEntityId 1172, GeoLocation "Gitwa" (Just <| toEntityId 1170) )
               , ( toEntityId 1173, GeoLocation "Kamuhororo" (Just <| toEntityId 1170) )
               , ( toEntityId 1174, GeoLocation "Karera" (Just <| toEntityId 1170) )
               , ( toEntityId 1175, GeoLocation "Kayenzi" (Just <| toEntityId 1170) )
               , ( toEntityId 1176, GeoLocation "Kibeho" (Just <| toEntityId 1170) )
               , ( toEntityId 1177, GeoLocation "Rusekabuye" (Just <| toEntityId 1170) )
               , ( toEntityId 1179, GeoLocation "Bikamba" (Just <| toEntityId 1178) )
               , ( toEntityId 1180, GeoLocation "Cyamutara" (Just <| toEntityId 1178) )
               , ( toEntityId 1181, GeoLocation "Gitambi" (Just <| toEntityId 1178) )
               , ( toEntityId 1182, GeoLocation "Kazi" (Just <| toEntityId 1178) )
               , ( toEntityId 1183, GeoLocation "Nyakambu" (Just <| toEntityId 1178) )
               , ( toEntityId 1184, GeoLocation "Nyarubuye" (Just <| toEntityId 1178) )
               , ( toEntityId 1185, GeoLocation "Rukore" (Just <| toEntityId 1178) )
               , ( toEntityId 1186, GeoLocation "Rusasa" (Just <| toEntityId 1178) )
               , ( toEntityId 1191, GeoLocation "Kabgayi" (Just <| toEntityId 1190) )
               , ( toEntityId 1192, GeoLocation "Kabingo" (Just <| toEntityId 1190) )
               , ( toEntityId 1193, GeoLocation "Kamiyove" (Just <| toEntityId 1190) )
               , ( toEntityId 1194, GeoLocation "Kivomo" (Just <| toEntityId 1190) )
               , ( toEntityId 1195, GeoLocation "Murwa" (Just <| toEntityId 1190) )
               , ( toEntityId 1196, GeoLocation "Nyenyeri" (Just <| toEntityId 1190) )
               , ( toEntityId 1197, GeoLocation "Rukingu" (Just <| toEntityId 1190) )
               , ( toEntityId 1198, GeoLocation "Shyondwe" (Just <| toEntityId 1190) )
               , ( toEntityId 1200, GeoLocation "Bushyana" (Just <| toEntityId 1199) )
               , ( toEntityId 1201, GeoLocation "Gatiba" (Just <| toEntityId 1199) )
               , ( toEntityId 1202, GeoLocation "Gatwa" (Just <| toEntityId 1199) )
               , ( toEntityId 1203, GeoLocation "Kadendegeri" (Just <| toEntityId 1199) )
               , ( toEntityId 1204, GeoLocation "Kavumo" (Just <| toEntityId 1199) )
               , ( toEntityId 1205, GeoLocation "Mwana" (Just <| toEntityId 1199) )
               , ( toEntityId 1207, GeoLocation "Gahwazi" (Just <| toEntityId 1206) )
               , ( toEntityId 1208, GeoLocation "Gakubo" (Just <| toEntityId 1206) )
               , ( toEntityId 1209, GeoLocation "Kabera" (Just <| toEntityId 1206) )
               , ( toEntityId 1210, GeoLocation "Mataba" (Just <| toEntityId 1206) )
               , ( toEntityId 1211, GeoLocation "Mutungo" (Just <| toEntityId 1206) )
               , ( toEntityId 1213, GeoLocation "Kibare" (Just <| toEntityId 1212) )
               , ( toEntityId 1214, GeoLocation "Mujebe" (Just <| toEntityId 1212) )
               , ( toEntityId 1215, GeoLocation "Musave" (Just <| toEntityId 1212) )
               , ( toEntityId 1216, GeoLocation "Nyarusebeya" (Just <| toEntityId 1212) )
               , ( toEntityId 1217, GeoLocation "Ruhanga" (Just <| toEntityId 1212) )
               ]
            ++ [ ( toEntityId 1220, GeoLocation "Karambi" (Just <| toEntityId 1219) )
               , ( toEntityId 1221, GeoLocation "Karenge" (Just <| toEntityId 1219) )
               , ( toEntityId 1222, GeoLocation "Kingazi" (Just <| toEntityId 1219) )
               , ( toEntityId 1223, GeoLocation "Nyakarama" (Just <| toEntityId 1219) )
               , ( toEntityId 1236, GeoLocation "Bitare" (Just <| toEntityId 1219) )
               , ( toEntityId 1237, GeoLocation "Gahondo" (Just <| toEntityId 1219) )
               , ( toEntityId 1225, GeoLocation "Gifumba" (Just <| toEntityId 1224) )
               , ( toEntityId 1226, GeoLocation "Kabunigu" (Just <| toEntityId 1224) )
               , ( toEntityId 1227, GeoLocation "Kabuye" (Just <| toEntityId 1224) )
               , ( toEntityId 1228, GeoLocation "Nkanga" (Just <| toEntityId 1224) )
               , ( toEntityId 1229, GeoLocation "Ntakara" (Just <| toEntityId 1224) )
               , ( toEntityId 1230, GeoLocation "Rwintare" (Just <| toEntityId 1224) )
               , ( toEntityId 1232, GeoLocation "Kigarama" (Just <| toEntityId 1231) )
               , ( toEntityId 1233, GeoLocation "Kinini-rusiga" (Just <| toEntityId 1231) )
               , ( toEntityId 1234, GeoLocation "Ntaruka" (Just <| toEntityId 1231) )
               , ( toEntityId 1235, GeoLocation "Rebero" (Just <| toEntityId 1231) )
               , ( toEntityId 1240, GeoLocation "Gatimba" (Just <| toEntityId 1239) )
               , ( toEntityId 1241, GeoLocation "Gatwa" (Just <| toEntityId 1239) )
               , ( toEntityId 1242, GeoLocation "Gisiza" (Just <| toEntityId 1239) )
               , ( toEntityId 1243, GeoLocation "Kabaraza" (Just <| toEntityId 1239) )
               , ( toEntityId 1244, GeoLocation "Kigarama" (Just <| toEntityId 1239) )
               , ( toEntityId 1245, GeoLocation "Kiziranyenzi" (Just <| toEntityId 1239) )
               , ( toEntityId 1246, GeoLocation "Nyakaruri" (Just <| toEntityId 1239) )
               , ( toEntityId 1247, GeoLocation "Nyarushinya" (Just <| toEntityId 1239) )
               , ( toEntityId 1249, GeoLocation "Gaseke" (Just <| toEntityId 1248) )
               , ( toEntityId 1250, GeoLocation "Kabagabaga" (Just <| toEntityId 1248) )
               , ( toEntityId 1251, GeoLocation "Kabakene" (Just <| toEntityId 1248) )
               , ( toEntityId 1252, GeoLocation "Nyamugari" (Just <| toEntityId 1248) )
               , ( toEntityId 1253, GeoLocation "Rimwe" (Just <| toEntityId 1248) )
               , ( toEntityId 1254, GeoLocation "Rugendabari" (Just <| toEntityId 1248) )
               , ( toEntityId 1256, GeoLocation "Cyikera" (Just <| toEntityId 1255) )
               , ( toEntityId 1257, GeoLocation "Kagunda" (Just <| toEntityId 1255) )
               , ( toEntityId 1258, GeoLocation "Karama" (Just <| toEntityId 1255) )
               , ( toEntityId 1259, GeoLocation "Kavoma" (Just <| toEntityId 1255) )
               , ( toEntityId 1260, GeoLocation "Kirurumo" (Just <| toEntityId 1255) )
               , ( toEntityId 1261, GeoLocation "Kivili" (Just <| toEntityId 1255) )
               , ( toEntityId 1262, GeoLocation "Mukumba" (Just <| toEntityId 1255) )
               , ( toEntityId 1263, GeoLocation "Muvumu" (Just <| toEntityId 1255) )
               , ( toEntityId 1264, GeoLocation "Nyabubare" (Just <| toEntityId 1255) )
               , ( toEntityId 1265, GeoLocation "Ruhanga" (Just <| toEntityId 1255) )
               , ( toEntityId 1267, GeoLocation "Bwimo" (Just <| toEntityId 1266) )
               , ( toEntityId 1268, GeoLocation "Gishyita" (Just <| toEntityId 1266) )
               , ( toEntityId 1269, GeoLocation "Kigali" (Just <| toEntityId 1266) )
               , ( toEntityId 1270, GeoLocation "Ngona" (Just <| toEntityId 1266) )
               , ( toEntityId 1271, GeoLocation "Nyabitare" (Just <| toEntityId 1266) )
               , ( toEntityId 1272, GeoLocation "Nyarunyinya" (Just <| toEntityId 1266) )
               , ( toEntityId 1273, GeoLocation "Nyarusange" (Just <| toEntityId 1266) )
               , ( toEntityId 1274, GeoLocation "Rwahi" (Just <| toEntityId 1266) )
               , ( toEntityId 1276, GeoLocation "Bugarura" (Just <| toEntityId 1275) )
               , ( toEntityId 1277, GeoLocation "Mwagiro" (Just <| toEntityId 1275) )
               , ( toEntityId 1278, GeoLocation "Ngendo" (Just <| toEntityId 1275) )
               , ( toEntityId 1279, GeoLocation "Nyabisindu" (Just <| toEntityId 1275) )
               , ( toEntityId 1280, GeoLocation "Nyabyondo" (Just <| toEntityId 1275) )
               , ( toEntityId 1281, GeoLocation "Nyamirembe" (Just <| toEntityId 1275) )
               , ( toEntityId 1282, GeoLocation "Rutonde" (Just <| toEntityId 1275) )
               , ( toEntityId 1283, GeoLocation "Rweya" (Just <| toEntityId 1275) )
               , ( toEntityId 1286, GeoLocation "Kagusa" (Just <| toEntityId 1285) )
               , ( toEntityId 1287, GeoLocation "Mafene" (Just <| toEntityId 1285) )
               , ( toEntityId 1288, GeoLocation "Munyinya" (Just <| toEntityId 1285) )
               , ( toEntityId 1289, GeoLocation "Rushaki" (Just <| toEntityId 1285) )
               , ( toEntityId 1319, GeoLocation "Kabuga" (Just <| toEntityId 1285) )
               , ( toEntityId 1291, GeoLocation "Kanaba" (Just <| toEntityId 1290) )
               , ( toEntityId 1292, GeoLocation "Karambi" (Just <| toEntityId 1290) )
               , ( toEntityId 1293, GeoLocation "Kavumu" (Just <| toEntityId 1290) )
               , ( toEntityId 1294, GeoLocation "Marembo" (Just <| toEntityId 1290) )
               , ( toEntityId 1295, GeoLocation "Misezero" (Just <| toEntityId 1290) )
               , ( toEntityId 1296, GeoLocation "Rurambo" (Just <| toEntityId 1290) )
               , ( toEntityId 1297, GeoLocation "Taba" (Just <| toEntityId 1290) )
               , ( toEntityId 1299, GeoLocation "Bukinga" (Just <| toEntityId 1298) )
               , ( toEntityId 1300, GeoLocation "Gatare" (Just <| toEntityId 1298) )
               , ( toEntityId 1301, GeoLocation "Gatsinde" (Just <| toEntityId 1298) )
               , ( toEntityId 1302, GeoLocation "Gihanga" (Just <| toEntityId 1298) )
               , ( toEntityId 1303, GeoLocation "Murambi" (Just <| toEntityId 1298) )
               , ( toEntityId 1304, GeoLocation "Rugando" (Just <| toEntityId 1298) )
               , ( toEntityId 1305, GeoLocation "Rusura" (Just <| toEntityId 1298) )
               , ( toEntityId 1307, GeoLocation "Kamuragi" (Just <| toEntityId 1306) )
               , ( toEntityId 1308, GeoLocation "Mwili" (Just <| toEntityId 1306) )
               , ( toEntityId 1309, GeoLocation "Nkinda" (Just <| toEntityId 1306) )
               , ( toEntityId 1310, GeoLocation "Nyirambuga" (Just <| toEntityId 1306) )
               , ( toEntityId 1311, GeoLocation "Nyirataba" (Just <| toEntityId 1306) )
               , ( toEntityId 1312, GeoLocation "Ruvumba" (Just <| toEntityId 1306) )
               , ( toEntityId 1314, GeoLocation "Gaseke" (Just <| toEntityId 1313) )
               , ( toEntityId 1315, GeoLocation "Gashoro" (Just <| toEntityId 1313) )
               , ( toEntityId 1316, GeoLocation "Karambi" (Just <| toEntityId 1313) )
               , ( toEntityId 1317, GeoLocation "Kigarama" (Just <| toEntityId 1313) )
               , ( toEntityId 1318, GeoLocation "Rukore" (Just <| toEntityId 1313) )
               ]
