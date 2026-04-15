#!/usr/bin/env python3
"""Emit OCL bulk-import JSON-lines for only the delta between local CSVs and OCL HEAD.

Reads every `*-concepts.csv` / `*-mappings.csv` in this directory and compares
against what's currently in `TIP-Global-Health/EHEZA` on api.openconceptlab.org.
Writes one JSON object per line to stdout for rows that are new or changed;
writes a counts summary to stderr.

Usage:
    export OCL_API_TOKEN=...
    python3 docs/ocl/delta_upload.py > /tmp/delta.jsonl
    # then POST /tmp/delta.jsonl to /importers/bulk-import/?update_if_exists=true

Why this exists: submitting the full CSV set on every change causes OCL to mint
a new concept-version row for every already-uploaded concept even when content
is identical. Those ghost versions clutter source-wide search. Emitting only
the delta avoids that.
"""
import csv
import json
import os
import sys
import urllib.parse
import urllib.request
import pathlib

HERE = pathlib.Path(__file__).parent
OCL_BASE = "https://api.openconceptlab.org"
SOURCE_PATH = "/orgs/TIP-Global-Health/sources/EHEZA/HEAD"

# Reuse the existing row builders so output format stays consistent.
sys.path.insert(0, str(HERE))
from csv_to_ocl_json import concept_row, mapping_row  # noqa: E402


def log(*a):
    print(*a, file=sys.stderr)


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


def fetch_all(path):
    """Paginate `?page=N&limit=...` until exhausted. Returns the concatenated list."""
    out = []
    page = 1
    while True:
        body, headers = ocl_get(path, {"page": page, "limit": 500, "verbose": "true"})
        if not isinstance(body, list) or not body:
            break
        out.extend(body)
        num_found = int(headers.get("num_found", len(out)))
        if len(out) >= num_found:
            break
        page += 1
    return out


def names_key(names):
    """Hashable, order-insensitive signature of a names list."""
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
    """Reduce a concept payload (ours or OCL's) to comparable fields."""
    return (
        payload.get("concept_class"),
        payload.get("datatype"),
        names_key(payload.get("names")),
        descriptions_key(payload.get("descriptions")),
        tuple(sorted((payload.get("extras") or {}).items())),
    )


def concept_signature_from_csv(row):
    return concept_signature(concept_row(row))


def concept_signature_from_ocl(detail):
    # OCL's detail response shape matches what we submit closely enough that
    # names/descriptions/extras compare directly.
    return concept_signature({
        "concept_class": detail.get("concept_class"),
        "datatype": detail.get("datatype"),
        "names": detail.get("names") or [],
        "descriptions": detail.get("descriptions") or [],
        "extras": detail.get("extras") or {},
    })


def mapping_key(row_or_payload):
    """Identity key for a mapping row (ours or OCL's)."""
    # CSV row has from_concept_url + to_concept_code; OCL detail has them too.
    return (
        row_or_payload["from_concept_url"].rstrip("/"),
        row_or_payload.get("map_type", "SAME-AS"),
        row_or_payload["to_source_url"].rstrip("/"),
        str(row_or_payload["to_concept_code"]),
    )


def load_csv_concepts():
    rows = {}
    for path in sorted(HERE.glob("*-concepts.csv")):
        with path.open() as f:
            for r in csv.DictReader(f):
                rows[r["id"]] = r
    return rows


def load_csv_mappings():
    rows = {}
    for path in sorted(HERE.glob("*-mappings.csv")):
        with path.open() as f:
            for r in csv.DictReader(f):
                rows[mapping_key(r)] = r
    return rows


def main():
    if "OCL_API_TOKEN" not in os.environ:
        log("ERROR: set OCL_API_TOKEN")
        sys.exit(2)

    log("Loading CSVs ...")
    desired_concepts = load_csv_concepts()
    desired_mappings = load_csv_mappings()
    log(f"  CSV: {len(desired_concepts)} concepts / {len(desired_mappings)} mappings")

    log("Fetching OCL HEAD state ...")
    existing_concepts = {c["id"]: c for c in fetch_all(SOURCE_PATH + "/concepts/")}
    existing_mappings = {mapping_key(m): m for m in fetch_all(SOURCE_PATH + "/mappings/")}
    log(f"  OCL: {len(existing_concepts)} concepts / {len(existing_mappings)} mappings")

    new_c = changed_c = same_c = 0
    new_m = same_m = 0
    emitted = 0

    for cid, row in desired_concepts.items():
        if cid not in existing_concepts:
            print(json.dumps(concept_row(row)))
            new_c += 1
            emitted += 1
        else:
            if concept_signature_from_csv(row) != concept_signature_from_ocl(existing_concepts[cid]):
                print(json.dumps(concept_row(row)))
                changed_c += 1
                emitted += 1
            else:
                same_c += 1

    for key, row in desired_mappings.items():
        if key not in existing_mappings:
            print(json.dumps(mapping_row(row)))
            new_m += 1
            emitted += 1
        else:
            same_m += 1

    orphan_c = sorted(set(existing_concepts) - set(desired_concepts))
    orphan_m = len(set(existing_mappings) - set(desired_mappings))

    log("Diff summary:")
    log(f"  concepts: new={new_c}  changed={changed_c}  unchanged={same_c}  orphan-on-ocl={len(orphan_c)}")
    log(f"  mappings: new={new_m}                unchanged={same_m}  orphan-on-ocl={orphan_m}")
    log(f"  emitting {emitted} row(s) to stdout")
    if orphan_c:
        log(f"  orphan concept ids: {orphan_c[:10]}{' ...' if len(orphan_c) > 10 else ''}")
        log("  (orphans are intentionally not deleted; retire them manually if needed)")


if __name__ == "__main__":
    main()
