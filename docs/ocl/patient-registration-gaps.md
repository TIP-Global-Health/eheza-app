# Patient Registration concepts — coverage map

This file documents Patient Registration coverage, sourced from the `Person`
record in `client/src/elm/Backend/Person/Model.elm`. Patient Registration is
not an encounter type — it is the field set captured at first contact when a
person is enrolled in E-Heza. Twenty-seven candidate fields were evaluated
against PIH's OCL dictionary: 10 were minted as new `EH-PER-NNN` concepts, 1
collapsed onto a Prenatal-pass concept already in `TIP/EHEZA`, and 16 had no
clean PIH equivalent.

## New in this pass — `patient-registration-concepts.csv`

| EH id | Concept | E-Heza field | PIH | Confidence |
|---|---|---|---|---|
| `EH-PER-001` | First name | `Person.firstName` | `PIH:2725` (`Question/Text`, *"First name"*) | high |
| `EH-PER-002` | Second name | `Person.secondName` | `PIH:2724` (`Question/Text`, *"Last name"*) | high |
| `EH-PER-003` | National ID number | `Person.nationalIdNumber` | `PIH:13173` (`Question/Text`, *"Identification number"*) — **NARROWER-THAN** mapping (E-Heza's national ID is a subtype of PIH's generic identifier) | high |
| `EH-PER-004` | Gender | `Person.gender` | `PIH:2845` (`Question/Coded`, *"Gender"*) | high |
| `EH-PER-005` | HIV status | `Person.hivStatus` | `PIH:3753` (`Question/Coded`, *"HIV status"*) | high |
| `EH-PER-006` | Mode of delivery | `Person.modeOfDelivery` | `PIH:5630` (`Question/Coded`, *"Method of delivery"*) | high |
| `EH-PER-007` | Highest level of education | `Person.educationLevel` | `PIH:1688` (`Question/Coded`, *"Highest education level"*) | high |
| `EH-PER-008` | Marital status | `Person.maritalStatus` | `PIH:1054` (`Question/Coded`, *"Civil status"*) | high |
| `EH-PER-009` | Telephone number | `Person.telephoneNumber` | `PIH:2614` (`Question/Text`, *"Mobile phone number"*) | medium |
| `EH-PER-010` | Next of kin name | `Person.nextOfKinName` | `PIH:13517` (`Question/Text`, *"Next of kin name (text)"*) | high |

## Already represented in `TIP/EHEZA` — not re-created

| Person field | Reuses concept | Notes |
|---|---|---|
| `Person.numberOfChildren` | `EH-PRE-019` Total number of living children (PIH:11117) | The Prenatal pass already minted this concept against `ObstetricHistoryValue.liveChildren`; reusing it keeps the count of living children as a single `TIP/EHEZA` fact regardless of which form captured it. *Follow-up: extend `EH-PRE-019.eheza_field_path` from `ObstetricHistoryValue.liveChildren` to include `Person.numberOfChildren` so the reuse is traceable from either side. Deferred — this pass does not edit `prenatal-concepts.csv`.* |

## Low-confidence candidates — deferred for reviewer attention

| E-Heza field | Candidate PIH match | Why excluded |
|---|---|---|
| `Person.province` (and the same family `Person.district`, `Person.sector`) | `PIH:10379` *Address1* (`Question/Text`, part of `PIH:10378` "Birthplace address construct" ConvSet) — with `district → PIH:10380` Address2 and `sector → PIH:10381` Address3 as the same positional pattern | Positional mapping (province → Address1, district → Address2, sector → Address3) is defensible but lossy: PIH's Address1/2/3 carry no semantic meaning per administrative level, so a reviewer in another country reading the data wouldn't know which field is the largest admin division. Reviewer should decide whether to opt in. |
| `Person.spousePhoneNumber` | `PIH:2614` *Mobile phone number* (`Question/Text`) | Same generic mobile-phone concept as `EH-PER-009`; would lose the spouse-relationship qualifier. Defensible if reviewers accept that relationship context comes from the field name rather than the concept. |
| `Person.nextOfKinPhoneNumber` | `PIH:2614` *Mobile phone number* (`Question/Text`) | Same rationale as `Person.spousePhoneNumber`; loses the next-of-kin qualifier. Note that `EH-PER-010` *does* mint the next-of-kin *name* concept (`PIH:13517` "Next of kin name (text)") — there's just no kin-phone counterpart. |

## Confirmed gaps — no PIH equivalent found (2026-04-19 probe)

Searches run on `https://api.openconceptlab.org/orgs/PIH/sources/PIH/concepts/?q=<term>`
— logging the negative results so the next mapping pass doesn't re-search
the same terms.

### Personal identifiers — modeled in OCL/PIH as person attributes, not concepts

- **`Person.hmisNumber`** — searches `hmis`, `health management information`. Hits returned health-clinic / program concepts (`PIH:2069` *Health clinic*, `PIH:5489` *Mental health*, etc.) — none correspond to a generic HMIS identifier.
- **`Person.avatarUrl`** — search `patient photo`. PIH has `PIH:12070` *Patient* and similar but no patient-photo concept. Photos are typically person attributes (`person_attribute` table on the OpenMRS side), not dictionary concepts.
- **`Person.spouseName`** — search `spouse name`. Top hits were generic name concepts (`PIH:2725` *First name*) plus `PIH:5617` *Partner or Spouse* (Misc/N/A — a person-relationship marker, not a name field). No spouse-name-specific concept.

### Demographic metadata — stored in the Person table, not as concepts

- **`Person.birthDate`** — searches `date of birth`, `birthdate`. PIH's only DOB concept is `PIH:11141` *Date of birth of family member* (a family-member DOB, not the patient's own). The patient's own DOB is conventionally stored in OpenMRS's `person.birthdate` column, not as a concept observation.
- **`Person.isDateOfBirthEstimated`** — searches `estimated date of birth`, `birthdate estimated`, `estimated birthdate`. No equivalent flag concept; `person.birthdate_estimated` is a column on the OpenMRS Person table for the same reason.

### Rwanda-specific

- **`Person.ubudehe`** — searches `ubudehe`, `socioeconomic category`. No PIH equivalent; ubudehe is a Rwandan socioeconomic stratification system. Closest hit `PIH:1535` *Medication category* is unrelated.

### Administrative geography — no PIH coverage at the admin-division level

- **`Person.province`** / **`district`** / **`sector`** / **`cell`** / **`village`** — searches per field name plus `village cell`, `subdivision`. PIH only has positional address concepts (`PIH:10379/10380/10381` Address1/2/3 inside the `PIH:10378` "Birthplace address construct" ConvSet). The positional mapping is documented as a low-confidence candidate above; here we're confirming there is no semantic admin-division equivalent.

### GPS coordinates and consent — no patient-coordinate concepts

- **`Person.registrationLatitude`** / **`Person.registrationLongitude`** — searches `latitude`, `longitude`, `gps coordinates`. No coordinate concepts in PIH; lat/lon are typically location attributes, not observations.
- **`Person.saveGPSLocation`** — searches `gps consent`, `location consent`. PIH consent concepts are clinical-procedure-specific (`PIH:14242` *Consent given for sex*, `PIH:13701` *ARV consent form signed*, etc.) — none cover GPS-capture consent.
