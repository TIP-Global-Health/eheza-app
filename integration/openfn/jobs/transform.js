/**
 * Transform - E-Heza person payload -> OpenMRS REST `/patient` body.
 *
 * Input  (state.data): the person payload posted by the E-Heza Drupal
 *        trigger (see hedley_openmrs_build_person_payload in the
 *        hedley_openmrs module).
 * Output (state.openmrsPatient): an OpenMRS `POST /ws/rest/v1/patient`
 *        body, mapped per integration/patient-field-mapping.md.
 *        state.data is left intact for the identity-matching step.
 *
 * Adaptor: @openfn/language-common
 */

fn((state) => {
  const person = state.data || {};

  // OpenMRS metadata UUIDs. Prefer values supplied on state; the fallback
  // is the local PoC instance (integration/openmrs/openmrs-metadata.json).
  // To target another OpenMRS, supply state.openmrsMetadata.
  const meta = state.openmrsMetadata || {
    identifierType: {
      nationalId: 'b6cf43fd-32ab-4503-9104-a6030919c56e',
      openmrsId: '8d793bee-c2cc-11de-8d13-0010c6dffd0f',
    },
    location: '8d6c993e-c2cc-11de-8d13-0010c6dffd0f',
    attr: {
      telephone: '028aee41-1f87-4b21-a3a4-f61eaaa24b65',
      civilStatus: '8d871f2a-c2cc-11de-8d13-0010c6dffd0f',
      educationLevel: '941295f7-9388-4be9-acb8-1f3a12ba72cb',
      hivStatus: '4c517673-0cd6-487a-9963-fe16c732edd2',
      modeOfDelivery: '46d3453d-5eb9-4505-a134-9add10c88eab',
      spouseName: '20dc9f1f-c16c-46a1-a235-2bd1b7356507',
      spousePhone: 'f59158d9-a1a8-4eaf-b634-6644b3bddc4f',
      nextOfKinName: '22ea36b6-ada5-45b2-b67f-983842fc818e',
      nextOfKinPhone: '8bd9b615-79ff-48ed-8d0f-056f64cff068',
      numberOfChildren: '4bfe967a-ae49-4e14-91df-e652e8e8ac79',
    },
  };

  // Value maps - keys are the raw E-Heza Drupal field values.
  const genderMap = { male: 'M', female: 'F' };
  const hivStatusMap = {
    'hiv-exposed-infant': 'HIV-exposed infant',
    negative: 'Negative',
    'negative-dc': 'Negative (discordant couple)',
    positive: 'Positive',
    unknown: 'Unknown',
  };
  const maritalStatusMap = {
    divorced: 'Divorced',
    married: 'Married',
    single: 'Single',
    widowed: 'Widowed',
    'living-with-partner': 'Living with partner',
    religious: 'Religious',
  };
  const modeOfDeliveryMap = {
    'svd-episiotomy': 'Spontaneous vaginal, with episiotomy',
    'svd-no-episiotomy': 'Spontaneous vaginal',
    'vd-vacuum': 'Vaginal, vacuum extraction',
    'cesarean-delivery': 'Cesarean',
  };
  const educationLevelMap = {
    0: 'No schooling',
    1: 'Primary school',
    2: 'Vocational training',
    3: 'Secondary school',
    4: 'Diploma program',
    5: 'Higher education',
    6: 'Advanced diploma',
    7: "Master's degree",
  };

  const present = (v) => v !== null && v !== undefined && v !== '';

  // Person attributes (string PersonAttributes) - omit empty values.
  const attributes = [];
  const addAttr = (typeUuid, value) => {
    if (present(value)) {
      attributes.push({ attributeType: typeUuid, value: String(value) });
    }
  };
  addAttr(meta.attr.telephone, person.phone_number);
  addAttr(meta.attr.civilStatus, maritalStatusMap[person.marital_status]);
  addAttr(meta.attr.educationLevel, educationLevelMap[person.education_level]);
  addAttr(meta.attr.hivStatus, hivStatusMap[person.hiv_status]);
  addAttr(
    meta.attr.modeOfDelivery,
    modeOfDeliveryMap[person.mode_of_delivery]
  );
  addAttr(meta.attr.spouseName, person.spouse_name);
  addAttr(meta.attr.spousePhone, person.spouse_phone_number);
  addAttr(meta.attr.nextOfKinName, person.next_of_kin_name);
  addAttr(meta.attr.nextOfKinPhone, person.next_of_kin_phone_number);
  addAttr(meta.attr.numberOfChildren, person.number_of_children);

  // Identifiers - the national ID, when present.
  const identifiers = [];
  if (present(person.national_id)) {
    identifiers.push({
      identifier: String(person.national_id),
      identifierType: meta.identifierType.nationalId,
      location: meta.location,
      preferred: true,
    });
  }

  // OpenMRS rejects a patient with no identifier. When there is no national
  // ID (children, unregistered adults), fall back to an OpenMRS ID. The
  // transform does no HTTP, so it cannot call idgen itself - it emits an
  // `autoGenerate` placeholder and the load step resolves the value via
  // idgen (on the create branch only) before POSTing the patient.
  if (identifiers.length === 0) {
    identifiers.push({
      identifierType: meta.identifierType.openmrsId,
      location: meta.location,
      autoGenerate: true,
      preferred: true,
    });
  }

  // Address - Rwanda hierarchy onto PersonAddress slots.
  const address = {};
  if (present(person.province)) address.stateProvince = person.province;
  if (present(person.district)) address.countyDistrict = person.district;
  if (present(person.sector)) address.address3 = person.sector;
  if (present(person.cell)) address.address2 = person.cell;
  if (present(person.village)) address.cityVillage = person.village;
  if (present(person.latitude)) address.latitude = String(person.latitude);
  if (present(person.longitude)) {
    address.longitude = String(person.longitude);
  }

  const openmrsPerson = {
    names: [
      {
        givenName: person.first_name,
        familyName: person.second_name,
        preferred: true,
      },
    ],
    gender: genderMap[person.gender] || 'U',
    birthdate: (person.birth_date || '').slice(0, 10) || null,
    birthdateEstimated: !!person.birth_date_estimated,
    attributes,
  };
  if (Object.keys(address).length > 0) {
    openmrsPerson.addresses = [address];
  }

  const openmrsPatient = { person: openmrsPerson, identifiers };

  console.log(
    'Transformed E-Heza person',
    person.person_uuid,
    '-> OpenMRS patient'
  );

  return { ...state, openmrsPatient };
});
