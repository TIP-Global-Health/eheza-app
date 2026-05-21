#!/usr/bin/env python3
"""Provision the local OpenMRS for the E-Heza -> OpenFN integration.

Idempotently creates (if missing):
  - an integration service user,
  - a National ID patient-identifier type,
  - the person-attribute types the OpenFN transform writes to,
and writes the resolved metadata UUIDs to openmrs-metadata.json — the
config the OpenFN job consumes.

This targets the local PoC OpenMRS. UVL's real instance has its own
metadata; this script doubles as the spec for what UVL must provide.

    python3 provision.py
"""
import base64
import json
import sys
import urllib.error
import urllib.parse
import urllib.request

BASE = 'http://localhost:8090/openmrs/ws/rest/v1'
# Local PoC instance — the documented default OpenMRS admin credentials.
ADMIN_AUTH = base64.b64encode(b'admin:Admin123').decode()

INTEGRATION_USER = 'openfn'
INTEGRATION_PASSWORD = 'Openfn-Integration-1'
INTEGRATION_ROLE = 'Privilege Level: Full'

OUT = '/var/www/html/ihangane/integration/openmrs/openmrs-metadata.json'

# Person-attribute types the transform writes to (display name -> description).
# "Civil Status" already ships with OpenMRS and is reused for marital status.
ATTR_TYPES = [
    ('Telephone Number', "Patient's telephone number"),
    ('Education Level', "Patient's highest level of education"),
    ('HIV Status', "Patient's HIV serostatus"),
    ('Mode of Delivery', 'Mode by which the patient was delivered'),
    ('Spouse Name', "Name of the patient's spouse or partner"),
    ('Spouse Phone Number', "Telephone number of the patient's spouse"),
    ('Next of Kin Name', "Name of the patient's next of kin"),
    ('Next of Kin Phone Number', "Telephone number of the next of kin"),
    ('Number of Children', 'Number of living children'),
]


def api(path, method='GET', body=None):
    """Call the OpenMRS REST API as admin."""
    data = json.dumps(body).encode() if body is not None else None
    req = urllib.request.Request(BASE + path, data=data, method=method)
    req.add_header('Authorization', 'Basic ' + ADMIN_AUTH)
    req.add_header('Content-Type', 'application/json')
    try:
        with urllib.request.urlopen(req, timeout=30) as r:
            raw = r.read().decode()
            return json.loads(raw) if raw else {}
    except urllib.error.HTTPError as e:
        print(f'  ! {method} {path} -> HTTP {e.code}: {e.read().decode()[:300]}',
              file=sys.stderr)
        raise


def find(resource, display):
    """Return the UUID of an existing resource matching display, or None.

    Lists the whole resource (metadata lists are small) and matches on
    display, rather than relying on the async search index — which would
    miss items created moments earlier in the same run.
    """
    res = api(f'/{resource}?v=default&limit=100')
    for r in res.get('results', []):
        if (r.get('display') or '').strip().lower() == display.strip().lower():
            return r['uuid']
    return None


def ensure_attr_type(name, description):
    uuid = find('personattributetype', name)
    if uuid:
        print(f'  = attr type "{name}" exists')
        return uuid
    created = api('/personattributetype', 'POST', {
        'name': name,
        'description': description,
        'format': 'java.lang.String',
    })
    print(f'  + attr type "{name}" created')
    return created['uuid']


def main():
    print('Provisioning local OpenMRS...', file=sys.stderr)

    # --- National ID identifier type ---
    national_id = find('patientidentifiertype', 'National ID')
    if national_id:
        print('  = identifier type "National ID" exists')
    else:
        national_id = api('/patientidentifiertype', 'POST', {
            'name': 'National ID',
            'description': "Patient's national identification number",
            'required': False,
        })['uuid']
        print('  + identifier type "National ID" created')

    openmrs_id = find('patientidentifiertype', 'OpenMRS Identification Number')
    default_location = find('location', 'Unknown Location')

    # --- person-attribute types ---
    attrs = {}
    civil_status = find('personattributetype', 'Civil Status')
    if civil_status:
        attrs['civil_status'] = civil_status
        print('  = attr type "Civil Status" exists (marital status)')
    key_by_name = {
        'Telephone Number': 'telephone',
        'Education Level': 'education_level',
        'HIV Status': 'hiv_status',
        'Mode of Delivery': 'mode_of_delivery',
        'Spouse Name': 'spouse_name',
        'Spouse Phone Number': 'spouse_phone',
        'Next of Kin Name': 'next_of_kin_name',
        'Next of Kin Phone Number': 'next_of_kin_phone',
        'Number of Children': 'number_of_children',
    }
    for name, description in ATTR_TYPES:
        attrs[key_by_name[name]] = ensure_attr_type(name, description)

    # --- integration user ---
    existing_user = api(f'/user?q={INTEGRATION_USER}&v=default')
    if existing_user.get('results'):
        print(f'  = user "{INTEGRATION_USER}" exists')
    else:
        role_uuid = find('role', INTEGRATION_ROLE)
        if not role_uuid:
            sys.exit(f'ERROR: role "{INTEGRATION_ROLE}" not found')
        api('/user', 'POST', {
            'username': INTEGRATION_USER,
            'password': INTEGRATION_PASSWORD,
            'person': {
                'names': [{'givenName': 'OpenFN', 'familyName': 'Integration'}],
                'gender': 'M',
            },
            'roles': [role_uuid],
        })
        print(f'  + user "{INTEGRATION_USER}" created')

    config = {
        '_comment': ('OpenMRS metadata UUIDs for the OpenFN job. Local PoC '
                     'values; UVL provisions its own. Password is not stored '
                     'here — it goes in the OpenFN job secrets.'),
        'base_url': BASE,
        'integration_user': INTEGRATION_USER,
        'identifier_types': {
            'national_id': national_id,
            'openmrs_id': openmrs_id,
        },
        'default_location': default_location,
        'person_attribute_types': attrs,
    }
    with open(OUT, 'w') as f:
        json.dump(config, f, indent=2)
        f.write('\n')

    print(f'\nWrote {OUT}', file=sys.stderr)
    print(f'Integration user: {INTEGRATION_USER} / {INTEGRATION_PASSWORD}',
          file=sys.stderr)


if __name__ == '__main__':
    main()
