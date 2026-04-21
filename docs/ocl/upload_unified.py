#!/usr/bin/env python3
"""Build OCL bulk-import JSON-lines from eheza-concepts-unified.csv +
uvl-mappings.csv, with delta-against-HEAD diffing.

Writes JSON-lines to stdout for rows that are new or changed; writes a
counts summary to stderr.

Usage:
    export OCL_API_TOKEN=...
    python3 docs/ocl/upload_unified.py > /tmp/delta.jsonl
    # then POST /tmp/delta.jsonl to /importers/bulk-import/?update_if_exists=true
    # (use multipart/form-data with field name 'file', not raw body)
    # parallel=1 is recommended on first runs to avoid intra-batch races
    # on locale-name uniqueness; parallel=4 is safe for delta runs.

The unified-master CSV schema (different from per-encounter files):
    id, translation_id, english, kinyarwanda, kirundi, somali,
    concept_class, datatype, description

Names locales: english, kinyarwanda (rw), kirundi (rn), somali (so).

OCL constraints handled by this script:

  1. (locale, FULLY_SPECIFIED-name) uniqueness per source. Different
     English concepts often share the same Kinyarwanda/Kirundi/Somali word
     in Translate.elm; the script tracks (locale, name.lower()) pairs
     already claimed on OCL HEAD plus those claimed earlier in the same
     batch, and demotes colliding names from FULLY_SPECIFIED to SHORT so
     the second+ concept still uploads with the synonym recorded.
     Comparison is case-insensitive because OCL's uniqueness check ignores
     case.
  2. concept_class must be one of OCL's accepted OpenMRS classes
     (Question, Symptom, Drug, Diagnosis, Finding, Test, Procedure, Misc,
     Coded clinical / socioeconomic, Pharmacologic Drug Class, etc.).
     Invented classes like "Coded answer" are rejected — keep the unified
     CSV's concept_class column to known-good values.
"""
import csv
import json
import os
import sys
import urllib.parse
import urllib.request
import pathlib

HERE = pathlib.Path(__file__).parent
REPO_BASE = HERE.parent.parent
UNIFIED_PATH = HERE / "eheza-concepts-unified.csv"
UVL_MAPPINGS_PATH = HERE / "uvl-mappings.csv"

OCL_BASE = "https://api.openconceptlab.org"
SOURCE_PATH = "/orgs/TIP-Global-Health/sources/EHEZA/HEAD"

OWNER = "TIP-Global-Health"
OWNER_TYPE = "Organization"
SOURCE = "EHEZA"

LOCALE_FIELDS = [("kinyarwanda", "rw"), ("kirundi", "rn"), ("somali", "so")]


def log(*a):
    print(*a, file=sys.stderr)


# ---------- OCL fetch ----------

def ocl_get(path, params=None):
    url = OCL_BASE + path
    if params:
        url += "?" + urllib.parse.urlencode(params)
    req = urllib.request.Request(url, headers={
        "Authorization": f"Token {os.environ['OCL_API_TOKEN']}",
        "Accept": "application/json",
    })
    with urllib.request.urlopen(req) as r:
        headers = dict(r.headers)
        body = json.loads(r.read())
    return body, headers


def fetch_all_concepts():
    out = []
    page = 1
    while True:
        body, headers = ocl_get(SOURCE_PATH + "/concepts/", {"page": page, "limit": 500, "verbose": "true"})
        if not isinstance(body, list) or not body:
            break
        out.extend(body)
        num_found = int(headers.get("num_found", len(out)))
        if len(out) >= num_found:
            break
        page += 1
    return out


def fetch_all_mappings():
    out = []
    page = 1
    while True:
        body, headers = ocl_get(SOURCE_PATH + "/mappings/", {"page": page, "limit": 500, "verbose": "true"})
        if not isinstance(body, list) or not body:
            break
        out.extend(body)
        num_found = int(headers.get("num_found", len(out)))
        if len(out) >= num_found:
            break
        page += 1
    return out


# ---------- payload builders ----------

def build_names(row, taken_by_owner):
    """Build the names array for a concept payload.

    `taken_by_owner` is a mutable dict mapping `(locale, name.lower())` →
    concept_id of whichever concept first claimed that name as
    FULLY_SPECIFIED. To avoid OCL's per-source per-locale FULLY_SPECIFIED
    uniqueness constraint, names that collide with a *different* concept's
    claim are demoted to `name_type=SHORT`. Self-collisions (re-uploading
    a concept's own existing names) are allowed — the concept keeps its
    own FULLY_SPECIFIED claim.

    Comparison is case-insensitive because OCL's uniqueness check ignores
    case.

    Mutates `taken_by_owner` to claim new FULLY_SPECIFIED entries so that
    subsequent rows in the same batch see them.
    """
    cid = row["id"]

    def claim(locale, value):
        key = (locale, value.lower())
        owner = taken_by_owner.get(key)
        if owner is None or owner == cid:
            taken_by_owner[key] = cid
            return "FULLY_SPECIFIED"
        return "SHORT"

    en_type = claim("en", row["english"])
    names = [{
        "name": row["english"],
        "locale": "en",
        "locale_preferred": en_type == "FULLY_SPECIFIED",
        "name_type": en_type,
    }]
    for col, loc in LOCALE_FIELDS:
        v = (row.get(col) or "").strip()
        if not v:
            continue
        nt = claim(loc, v)
        names.append({
            "name": v,
            "locale": loc,
            "locale_preferred": False,
            "name_type": nt,
        })
    return names


def concept_payload(row, taken_by_owner):
    return {
        "type": "Concept",
        "id": row["id"],
        "owner": OWNER,
        "owner_type": OWNER_TYPE,
        "source": SOURCE,
        "concept_class": row["concept_class"] or "Misc",
        "datatype": row["datatype"] or "N/A",
        "names": build_names(row, taken_by_owner),
        "descriptions": ([{
            "description": row["description"],
            "locale": "en",
            "description_type": "Definition",
        }] if (row.get("description") or "").strip() else []),
        "extras": {
            "eheza_field_path": row.get("translation_id", "") or "",
        },
    }


def mapping_payload(row):
    return {
        "type": "Mapping",
        "owner": row["owner"],
        "owner_type": row["owner_type"],
        "source": row["source"],
        "from_concept_url": row["from_concept_url"],
        "map_type": row["map_type"],
        "to_source_url": row["to_source_url"],
        "to_concept_code": row["to_concept_code"],
        "to_concept_name": row["to_concept_name"],
        "extras": {"match_confidence": row.get("confidence", "")},
    }


# ---------- signatures for delta ----------

def names_key(names):
    return frozenset(
        (n.get("name"), n.get("locale"), n.get("name_type"))
        for n in (names or [])
    )


def descriptions_key(descs):
    return frozenset(
        (d.get("description"), d.get("locale"))
        for d in (descs or [])
    )


def concept_signature(payload):
    return (
        payload.get("concept_class"),
        payload.get("datatype"),
        names_key(payload.get("names")),
        descriptions_key(payload.get("descriptions")),
        tuple(sorted((payload.get("extras") or {}).items())),
    )


def concept_signature_from_ocl(detail):
    return concept_signature({
        "concept_class": detail.get("concept_class"),
        "datatype": detail.get("datatype"),
        "names": detail.get("names") or [],
        "descriptions": detail.get("descriptions") or [],
        "extras": detail.get("extras") or {},
    })


def mapping_key(row_or_payload):
    return (
        row_or_payload["from_concept_url"].rstrip("/"),
        row_or_payload.get("map_type", "SAME-AS"),
        row_or_payload["to_source_url"].rstrip("/"),
        str(row_or_payload["to_concept_code"]),
    )


# ---------- main ----------

def main():
    log("Fetching OCL HEAD…")
    ocl_concepts = fetch_all_concepts()
    log(f"  fetched {len(ocl_concepts)} concepts")
    ocl_mappings = fetch_all_mappings()
    log(f"  fetched {len(ocl_mappings)} mappings")

    ocl_concept_by_id = {c["id"]: c for c in ocl_concepts}
    ocl_mapping_keys = {mapping_key(m) for m in ocl_mappings}

    # Seed locale-name uniqueness tracker with what's already on OCL as
    # FULLY_SPECIFIED. Map (locale, name.lower()) -> concept_id of the
    # current owner. Local rows that collide with a *different* concept's
    # claim get demoted to SHORT in build_names; self-collisions (re-uploading
    # a concept's own existing names) are allowed. Comparison is
    # case-insensitive because OCL ignores case.
    taken_by_owner = {}
    for c in ocl_concepts:
        for n in (c.get("names") or []):
            if n.get("name_type") == "FULLY_SPECIFIED" and n.get("name") and n.get("locale"):
                taken_by_owner.setdefault((n["locale"], n["name"].lower()), c["id"])
    log(f"  seeded {len(taken_by_owner)} OCL-claimed FULLY_SPECIFIED (locale, name) pairs")

    # ---- concepts ----
    new_count = updated_count = unchanged_count = 0
    sample_updates = []
    sample_new = []
    for row in csv.DictReader(open(UNIFIED_PATH)):
        payload = concept_payload(row, taken_by_owner)
        my_sig = concept_signature(payload)
        ocl_detail = ocl_concept_by_id.get(row["id"])
        if ocl_detail is None:
            print(json.dumps(payload))
            new_count += 1
            if len(sample_new) < 5:
                sample_new.append((row["id"], row["english"]))
        else:
            ocl_sig = concept_signature_from_ocl(ocl_detail)
            if my_sig == ocl_sig:
                unchanged_count += 1
            else:
                print(json.dumps(payload))
                updated_count += 1
                if len(sample_updates) < 5:
                    sample_updates.append((row["id"], row["english"]))

    log(f"\nConcepts:  new={new_count}  updated={updated_count}  unchanged={unchanged_count}")
    log("Sample new:")
    for cid, en in sample_new:
        log(f"  + {cid}  {en!r}")
    log("Sample updated:")
    for cid, en in sample_updates:
        log(f"  ~ {cid}  {en!r}")

    # ---- mappings ----
    new_map = unchanged_map = 0
    sample_new_map = []
    for row in csv.DictReader(open(UVL_MAPPINGS_PATH)):
        key = mapping_key(row)
        if key in ocl_mapping_keys:
            unchanged_map += 1
            continue
        print(json.dumps(mapping_payload(row)))
        new_map += 1
        if len(sample_new_map) < 5:
            sample_new_map.append(key)

    log(f"\nMappings:  new={new_map}  unchanged={unchanged_map}")
    for k in sample_new_map:
        log(f"  + {k[0].split('/')[-2]} -> UVL:{k[3]}")


if __name__ == "__main__":
    main()
