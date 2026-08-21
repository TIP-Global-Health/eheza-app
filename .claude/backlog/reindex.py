#!/usr/bin/env python3
"""Rebuild index.tsv from items/*.md. Never edit index.tsv by hand — edit the item.

status / severity / issue / PR are DERIVED from the entry's prose, so updating an entry
updates the index. tier is the one field that is not derivable; it lives in the item's
`<!-- ... -->` line and is set when the item is triaged.
"""
import re, glob, os, collections
os.chdir(os.path.dirname(os.path.abspath(__file__)))

PATTERNS = [
    (r'✅\s*IMPLEMENTED|⬖\s*PARTLY IMPLEMENTED', 'IMPLEMENTED'),
    (r'🅿\s*PARKED|\bDECLINED\b',                 'PARKED'),
    (r"❌\s*WON'?T-FIX|❌\s*DROPPED",              'WONTFIX'),
    (r'❌\s*REFUTED|❌\s*MOOT',                    'REFUTED'),
    (r'✅\s*SUPERSEDED|✅\s*CLOSED|❌\s*CLOSED|✅\s*DONE|✅\s*FIXED', 'CLOSED'),
    (r'⬖\s*SPLIT', 'SPLIT'), (r'⏭\s*SKIPPED', 'SKIPPED'),
    (r'\bMONITORING\b', 'MONITORING'),
    (r'\bSTALE\b', 'STALE'), (r'\bREADY\b', 'READY'),
]
def status_of(t):
    t = t.replace('*', '')          # markdown bold sits between the emoji and the word
    for pat, name in PATTERNS:
        if re.search(pat, t): return name
    return 'UNKNOWN'

rows = []
for path in glob.glob('items/*.md'):
    text = open(path).read()
    meta = dict(re.findall(r'(\w+):(\S*)', text.split('\n')[0])) if text.startswith('<!--') else {}
    head = next((l for l in text.split('\n') if l.startswith('### ')), '')
    iid  = meta.get('id') or os.path.basename(path)[:-3]
    st   = status_of(head)
    if st == 'UNKNOWN': st = status_of(text[:1200])
    sev  = (re.search(r'\b(HIGH|MED-HIGH|MED-LOW|MED|LOW)\b', head) or ['',''])[1] if re.search(r'\b(HIGH|MED-HIGH|MED-LOW|MED|LOW)\b', head) else ''
    title = re.sub(r'^### (?:B|TH|G)-\d+ [—-] ', '', head)
    title = re.split(r' [—-] (?=✅|🅿|❌|⬖|⏭|READY\b|STALE\b)', title)[0][:120]
    # an entry often cites historical PRs before its own; take the ref that follows
    # the status word, not the first one in the line
    scope = head[head.index('IMPLEMENTED'):] if 'IMPLEMENTED' in head else head
    def ref(pat, text):
        m = re.search(pat, text) or re.search(pat, head)
        return m.group(1) if m else ''
    rows.append([iid, meta.get('tier',''), st, sev,
                 ref(r'issue #(\d+)', scope), ref(r'PR #(\d+)', scope),
                 meta.get('round',''), title])

rows.sort(key=lambda r: (r[0].split('-')[0], int(r[0].split('-')[1])))
with open('index.tsv','w') as f:
    f.write('id\ttier\tstatus\tseverity\tissue\tpr\tround\ttitle\n')
    for r in rows: f.write('\t'.join(r) + '\n')
print(f'{len(rows)} items ->', dict(collections.Counter(r[2] for r in rows)))
