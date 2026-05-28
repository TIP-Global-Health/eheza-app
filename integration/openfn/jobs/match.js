/**
 * Identity matching — find an existing OpenMRS patient for the incoming
 * E-Heza person, so the load step links to it instead of creating a
 * duplicate. See integration/identity-matching.md.
 *
 * Input  (state.data): the E-Heza person payload.
 *        (state.openmrsPatient): the transform output — passed through.
 *        (state.configuration): OpenMRS base URL + integration-user auth.
 * Output (state.match): { action, patientUuid, via, candidates }.
 *        An ambiguous result throws instead, failing the run for review.
 *
 * Adaptor: @openfn/language-http
 */

// Pure helpers — pulled out of fn() so the decision logic is testable.
const dateOnly = (v) => (v ? String(v).slice(0, 10) : null);
const summarize = (p) => ({
  uuid: p.uuid,
  display: p.display,
  birthdate: dateOnly(p.person && p.person.birthdate),
});
const resultsOf = (s) => (s.data && s.data.results) || [];

/**
 * Tier 1 decision — exact national-ID match.
 *
 * Returns a `link` decision for exactly one hit, `null` to fall through to
 * Tier 2 when no hit, and throws on >1 hit (national IDs are meant to be
 * unique).
 */
const decideByNationalId = (person, hits) => {
  if (hits.length === 1) {
    return {
      action: 'link',
      patientUuid: hits[0].uuid,
      via: 'national-id',
      candidates: hits.map(summarize),
    };
  }
  if (hits.length > 1) {
    throw new Error(
      'Ambiguous: national ID ' +
        person.national_id +
        ' matched ' +
        hits.length +
        ' OpenMRS patients'
    );
  }
  return null;
};

/**
 * Tier 2 decision — name search filtered by birth date.
 *
 * Returns a `link` for exactly one same-DOB candidate, a `create` when none,
 * and throws on >1 same-DOB candidates. With no birth date on the incoming
 * person any name hit is treated as ambiguous (no way to confirm).
 */
const decideByName = (person, hits) => {
  const birth = dateOnly(person.birth_date);
  if (!birth) {
    if (hits.length > 0) {
      throw new Error(
        'Ambiguous: no birth date to confirm a name match for ' +
          person.first_name +
          ' ' +
          person.second_name
      );
    }
    return { action: 'create', patientUuid: null, via: 'none', candidates: [] };
  }
  const candidates = hits.filter(
    (p) => dateOnly(p.person && p.person.birthdate) === birth
  );
  if (candidates.length === 0) {
    return {
      action: 'create',
      patientUuid: null,
      via: 'none',
      candidates: hits.map(summarize),
    };
  }
  if (candidates.length === 1) {
    return {
      action: 'link',
      patientUuid: candidates[0].uuid,
      via: 'name-birthdate',
      candidates: candidates.map(summarize),
    };
  }
  throw new Error(
    'Ambiguous: ' +
      candidates.length +
      ' OpenMRS patients match the name and birth date'
  );
};

fn(async (state) => {
  const person = state.data || {};
  const cfg = state.configuration || {};
  const searchUrl = cfg.openmrsBaseUrl + '/patient';
  const authHeaders = { Authorization: cfg.openmrsAuth };
  const REP = 'custom:(uuid,display,person:(uuid,gender,birthdate))';

  let match;

  // Tier 1 — national ID. An exact identifier match is decisive.
  if (person.national_id) {
    const res = await get(searchUrl, {
      query: { identifier: person.national_id, v: REP },
      headers: authHeaders,
    })(state);
    match = decideByNationalId(person, resultsOf(res));
  }

  // Tier 2 — name + birth date. Runs only when Tier 1 did not decide.
  if (!match) {
    if (!person.first_name || !person.second_name) {
      throw new Error('Cannot match: the person payload is missing a name');
    }
    const res = await get(searchUrl, {
      query: {
        q: person.first_name + ' ' + person.second_name,
        v: REP,
      },
      headers: authHeaders,
    })(state);
    match = decideByName(person, resultsOf(res));
  }

  console.log(
    'Identity match',
    person.person_uuid,
    '->',
    match.action,
    'via',
    match.via,
    match.patientUuid || ''
  );

  // Keep state.data and state.openmrsPatient intact for the load step.
  return { ...state, data: person, match };
});
