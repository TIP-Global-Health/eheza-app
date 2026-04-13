#!/usr/bin/env python3
"""Convert docs/ocl/*.csv into OCL bulk-import JSON-lines on stdout."""
import csv, json, sys, pathlib

HERE = pathlib.Path(__file__).parent

TRANSLATIONS = {}
tpath = HERE / "translations.jsonl"
if tpath.exists():
    with tpath.open() as f:
        for line in f:
            if line.strip():
                t = json.loads(line)
                TRANSLATIONS[t["id"]] = t

LOCALE_FIELDS = [("kinyarwanda", "rw"), ("kirundi", "rn"), ("somali", "so")]

def build_names(r):
    names = [{
        "name": r["name"],
        "locale": "en",
        "locale_preferred": True,
        "name_type": "FULLY_SPECIFIED",
    }]
    t = TRANSLATIONS.get(r["id"])
    if t:
        for field, locale in LOCALE_FIELDS:
            value = t.get(field)
            if value:
                names.append({
                    "name": value,
                    "locale": locale,
                    "locale_preferred": False,
                    "name_type": "FULLY_SPECIFIED",
                })
    return names

def concept_row(r):
    return {
        "type": "Concept",
        "id": r["id"],
        "owner": r["owner"],
        "owner_type": r["owner_type"],
        "source": r["source"],
        "concept_class": r["concept_class"],
        "datatype": r["datatype"],
        "names": build_names(r),
        "descriptions": ([{
            "description": r["description"],
            "locale": "en",
            "description_type": "Definition",
        }] if r.get("description") else []),
        "extras": {
            "eheza_category": r.get("eheza_category", ""),
            "eheza_field_path": r.get("eheza_field_path", ""),
            "match_confidence": r.get("confidence", ""),
        },
    }

def mapping_row(r):
    return {
        "type": "Mapping",
        "owner": r["owner"],
        "owner_type": r["owner_type"],
        "source": r["source"],
        "from_concept_url": r["from_concept_url"],
        "map_type": r["map_type"],
        "to_source_url": r["to_source_url"],
        "to_concept_code": r["to_concept_code"],
        "to_concept_name": r["to_concept_name"],
        "extras": {"match_confidence": r.get("confidence", "")},
    }

def main():
    for path in sorted(HERE.glob("*-concepts.csv")):
        with path.open() as f:
            for row in csv.DictReader(f):
                print(json.dumps(concept_row(row)))
    for path in sorted(HERE.glob("*-mappings.csv")):
        with path.open() as f:
            for row in csv.DictReader(f):
                print(json.dumps(mapping_row(row)))

if __name__ == "__main__":
    main()
