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

fn(async (state) => {
  const person = state.data || {};

  // Representation that carries the person birthdate used by Tier 2.
  const REP = 'custom:(uuid,display,person:(uuid,gender,birthdate))';

  const dateOnly = (v) => (v ? String(v).slice(0, 10) : null);
  const summarize = (p) => ({
    uuid: p.uuid,
    display: p.display,
    birthdate: dateOnly(p.person && p.person.birthdate),
  });
  const resultsOf = (s) => (s.data && s.data.results) || [];

  let match;

  // Tier 1 — national ID. An exact identifier match is decisive.
  if (person.national_id) {
    const hits = resultsOf(
      await get('/patient', {
        query: { identifier: person.national_id, v: REP },
      })(state)
    );
    if (hits.length === 1) {
      match = {
        action: 'link',
        patientUuid: hits[0].uuid,
        via: 'national-id',
        candidates: hits.map(summarize),
      };
    } else if (hits.length > 1) {
      throw new Error(
        'Ambiguous: national ID ' +
          person.national_id +
          ' matched ' +
          hits.length +
          ' OpenMRS patients'
      );
    }
  }

  // Tier 2 — name + birth date. Runs only when Tier 1 did not decide.
  if (!match) {
    if (!person.first_name || !person.second_name) {
      throw new Error('Cannot match: the person payload is missing a name');
    }
    const hits = resultsOf(
      await get('/patient', {
        query: {
          q: person.first_name + ' ' + person.second_name,
          v: REP,
        },
      })(state)
    );
    const birth = dateOnly(person.birth_date);

    if (!birth) {
      // No birth date to confirm a name match — a hit cannot be trusted.
      if (hits.length > 0) {
        throw new Error(
          'Ambiguous: no birth date to confirm a name match for ' +
            person.first_name +
            ' ' +
            person.second_name
        );
      }
      match = { action: 'create', patientUuid: null, via: 'none', candidates: [] };
    } else {
      const candidates = hits.filter(
        (p) => dateOnly(p.person && p.person.birthdate) === birth
      );
      if (candidates.length === 0) {
        match = {
          action: 'create',
          patientUuid: null,
          via: 'none',
          candidates: hits.map(summarize),
        };
      } else if (candidates.length === 1) {
        match = {
          action: 'link',
          patientUuid: candidates[0].uuid,
          via: 'name-birthdate',
          candidates: candidates.map(summarize),
        };
      } else {
        throw new Error(
          'Ambiguous: ' +
            candidates.length +
            ' OpenMRS patients match the name and birth date'
        );
      }
    }
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
