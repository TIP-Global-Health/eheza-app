# E-Heza Person → OpenMRS Patient — field mapping

The field-to-field mapping the OpenFN transform uses to turn an E-Heza
person into an OpenMRS REST `POST /ws/rest/v1/patient` payload.

Registration data maps **structurally** — OpenMRS Patient/Person fields,
identifiers, and PersonAttributes — **not** concept-coded observations.
(Concept coding via the OCL contract is for Phase 2 clinical data; UVL's
dictionary has no registration concepts anyway.)

Source of truth for the E-Heza side: `client/src/elm/Backend/Person/Model.elm`.

## Out of scope / not transferred

| E-Heza field | Reason |
|---|---|
| `hmisNumber` | Rwanda-only legacy field; never populated on Burundi |
| `ubudehe` | Rwanda-specific socioeconomic category; no OpenMRS target |
| `name` | Derived (`firstName` + `secondName`); OpenMRS builds the display name |
| `saveGPSLocation` | UI flag, not data |
| `healthCenterId`, `shard`, `deleted` | E-Heza operational/system fields |

## OpenMRS-side prerequisites (job configuration)

The transform writes to these OpenMRS metadata objects, referenced **by
UUID in the OpenFN job config** — placeholder UUIDs locally, UVL's real
UUIDs when the target switches. They must exist on the target instance:

- **PatientIdentifierType** — National ID. Plus OpenMRS's auto-generated
  *OpenMRS ID* (see Identifiers).
- **Location** — default identifier/registration location.
- **PersonAttributeTypes** — *Telephone Number* (standard in OpenMRS), and
  custom types for: Marital status, Education level, HIV status, Mode of
  delivery, Spouse name, Spouse phone, Next-of-kin name, Next-of-kin phone,
  Number of children.

## Mapping

### person.names

| E-Heza | OpenMRS | Notes |
|---|---|---|
| `firstName` | `person.names[0].givenName` | required |
| `secondName` | `person.names[0].familyName` | required |
| — | `person.names[0].preferred = true` | single preferred name |

### person — core demographics

| E-Heza | OpenMRS | Transform |
|---|---|---|
| `gender` | `person.gender` | value map: Male→`M`, Female→`F` |
| `birthDate` | `person.birthdate` | ISO `YYYY-MM-DD` |
| `isDateOfBirthEstimated` | `person.birthdateEstimated` | boolean |
| `avatarUrl` | *(deferred)* | person photo is a separate REST call — out of scope for the PoC |

### identifiers[]

| E-Heza | OpenMRS | Notes |
|---|---|---|
| `nationalIdNumber` | `identifiers[]`: `identifier`=value, `identifierType`=&lt;National ID UUID&gt;, `location`=&lt;Location UUID&gt;, `preferred`=true | omit the entry when `nationalIdNumber` is empty |
| — | `identifiers[]`: an auto-generated *OpenMRS ID* | OpenMRS requires every patient to have ≥1 identifier. Assign a generated OpenMRS ID (idgen module) so patients with no national ID still validate; when national ID is present it can be the preferred identifier instead. |

### person.addresses[]

Rwanda's 5-level hierarchy maps onto PersonAddress slots (Burundi's is
3-level — province/commune/colline). **Confirm the exact slot assignment
against the target instance's address-hierarchy configuration** — the
below is a sensible default.

| E-Heza | OpenMRS PersonAddress |
|---|---|
| `province` | `stateProvince` |
| `district` | `countyDistrict` |
| `sector` | `address3` |
| `cell` | `address2` |
| `village` | `cityVillage` |
| `registrationLatitude` | `latitude` |
| `registrationLongitude` | `longitude` |

### person.attributes[]

Each is a PersonAttribute: `{ attributeType: <UUID>, value: <string> }`.
For the PoC these are **string** attributes.

| E-Heza | PersonAttributeType | Transform |
|---|---|---|
| `telephoneNumber` | Telephone Number | string |
| `maritalStatus` | Marital status | value map |
| `educationLevel` | Education level | value map |
| `hivStatus` | HIV status | value map |
| `modeOfDelivery` | Mode of delivery | value map |
| `numberOfChildren` | Number of children | integer as string; usually empty — not collected on the registration form |
| `spouseName` | Spouse name | string |
| `spousePhoneNumber` | Spouse phone | string |
| `nextOfKinName` | Next-of-kin name | string |
| `nextOfKinPhoneNumber` | Next-of-kin phone | string |

Spouse / next-of-kin fields are only collected on the Rwanda site; on
Burundi they arrive empty and the attributes are omitted.

## Value maps

**gender** — Male→`M`, Female→`F`

**maritalStatus**

| E-Heza | value |
|---|---|
| Single | Single |
| Married | Married |
| Divorced | Divorced |
| Widowed | Widowed |
| LivingWithPartner | Living with partner |
| Religious | Religious |

**educationLevel**

| E-Heza | value |
|---|---|
| NoSchooling | No schooling |
| PrimarySchool | Primary school |
| VocationalTrainingSchool | Vocational training |
| SecondarySchool | Secondary school |
| DiplomaProgram | Diploma program |
| HigherEducation | Higher education |
| AdvancedDiploma | Advanced diploma |
| MastersDegree | Master's degree |

**hivStatus**

| E-Heza | value |
|---|---|
| Negative | Negative |
| Positive | Positive |
| NegativeDiscordantCouple | Negative (discordant couple) |
| HIVExposedInfant | HIV-exposed infant |
| Unknown | Unknown |

**modeOfDelivery**

| E-Heza | value |
|---|---|
| CesareanDelivery | Cesarean |
| VaginalDelivery (Spontaneous, episiotomy) | Spontaneous vaginal, with episiotomy |
| VaginalDelivery (Spontaneous, no episiotomy) | Spontaneous vaginal |
| VaginalDelivery (WithVacuumExtraction) | Vaginal, vacuum extraction |

## Notes for the transform (task #12)

- Incoming data = the E-Heza Drupal person REST representation
  (`HedleyRestfulPeople`); exact JSON keys are pinned when the export
  payload is defined.
- Omit empty optional fields — don't send null/blank attributes.
- All OpenMRS UUIDs come from job config — never hardcode — so the
  local→UVL switch is config-only.
- `nationalIdNumber`, `firstName`+`secondName`, `birthDate`, and
  `telephoneNumber` are also the identity-matching keys — see the
  identity-matching step.
- The created patient's OpenMRS UUID is written back to E-Heza by the
  load step.

## Deferred / production-only

- **Photo** (`avatarUrl`) — separate `personimage` REST call; not in the PoC.
- **Relationships** — spouse and next-of-kin are PersonAttributes for the
  PoC; the OpenMRS-native modeling is a `relationship` to another person.
- **Concept-coded observations** — HIV status / mode of delivery /
  education could be observations (the OCL/CIEL codes already exist)
  rather than string attributes; UVL's call.
