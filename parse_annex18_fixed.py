"""Parse Annexure XVIII of the MoSPI Flash Report (May 2024) -- fixed state detection.

Differences to parse_annex18.py:
  * Section headings (state and sector) are recognised by their BOLD font, not by
    "all capital letters". Wrapped project names are regular font, so they can no
    longer be mistaken for a state.
  * A heading in capitals is a STATE, a heading in title case is a SECTOR.
  * Every state heading is checked against a whitelist of Indian states / union
    territories (plus the report's own 'MULTI STATE' category). Headings that are
    clipped by the PDF (e.g. 'RAJASTHA') are mapped to the unique whitelist entry
    they start with.
  * page number and the line position are kept so rows can be traced back.

Usage: python parse_annex18_fixed.py path/to/FlashReport_May_2024.pdf out.csv
"""
import csv
import re
import sys

import pdfplumber

COLS = {'sl': (25, 50), 'proj': (52, 200), 'appr': (205, 250),
        'doc': (295, 345), 'cost': (400, 450), 'exp': (505, 560)}

STATES = [
    'ANDHRA PRADESH', 'ARUNACHAL PRADESH', 'ASSAM', 'BIHAR', 'CHHATTISGARH', 'GOA',
    'GUJARAT', 'HARYANA', 'HIMACHAL PRADESH', 'JHARKHAND', 'KARNATAKA', 'KERALA',
    'MADHYA PRADESH', 'MAHARASHTRA', 'MANIPUR', 'MEGHALAYA', 'MIZORAM', 'NAGALAND',
    'ODISHA', 'PUNJAB', 'RAJASTHAN', 'SIKKIM', 'TAMIL NADU', 'TELANGANA', 'TRIPURA',
    'UTTAR PRADESH', 'UTTARAKHAND', 'WEST BENGAL',
    # union territories
    'ANDAMAN AND NICOBAR ISLANDS', 'CHANDIGARH', 'DADRA AND NAGAR HAVELI AND DAMAN AND DIU',
    'DELHI', 'JAMMU AND KASHMIR', 'LADAKH', 'LAKSHADWEEP', 'PUDUCHERRY',
    # report category for projects that span several states
    'MULTI STATE',
]


def colof(x):
    for k, (a, b) in COLS.items():
        if a <= x < b:
            return k
    return None


def norm_state(txt):
    """Return (whitelist_state or None, how) for a heading text."""
    t = re.sub(r'[^A-Z ]', '', txt.upper()).strip()
    t = re.sub(r'\s+', ' ', t)
    if t in STATES:
        return t, 'exact'
    # clipped heading: accept if it is the start of exactly one whitelist entry
    if len(t) >= 5:
        hits = [s for s in STATES if s.startswith(t)]
        if len(hits) == 1:
            return hits[0], 'prefix'
    # alternative spellings seen in government data
    alias = {'ORISSA': 'ODISHA', 'PUDUCHERY': 'PUDUCHERRY', 'PONDICHERRY': 'PUDUCHERRY',
             'UTTARANCHAL': 'UTTARAKHAND', 'JAMMU KASHMIR': 'JAMMU AND KASHMIR',
             'MULTISTATE': 'MULTI STATE', 'MULTI-STATE': 'MULTI STATE'}
    if t in alias:
        return alias[t], 'alias'
    return None, 'none'


def parse(pdf_path, first=466, last=574):
    pdf = pdfplumber.open(pdf_path)
    recs, headings = [], []
    state = sector = None
    cur = None
    for pno in range(first - 1, last):
        page = pdf.pages[pno]
        lines = {}
        for w in page.extract_words(extra_attrs=['fontname']):
            lines.setdefault(round(w['top'] / 4), []).append(w)
        for key in sorted(lines):
            ws = sorted(lines[key], key=lambda x: x['x0'])
            cells, bold = {}, {}
            for w in ws:
                c = colof(w['x0'])
                if c:
                    cells.setdefault(c, []).append(w['text'])
                    bold.setdefault(c, []).append('Bold' in w['fontname'])
            cells = {k: ' '.join(v) for k, v in cells.items()}
            if not cells:
                continue
            ptxt = cells.get('proj', '')
            only_proj = set(cells) == {'proj'}
            is_bold = only_proj and all(bold['proj'])
            if is_bold and not cells.get('sl'):
                if ptxt.isupper():
                    st, how = norm_state(ptxt)
                    headings.append((pno + 1, 'STATE', ptxt, st, how))
                    state = st if st else 'UNKNOWN:' + ptxt
                else:
                    headings.append((pno + 1, 'SECTOR', ptxt, ptxt, 'bold'))
                    sector = ptxt
                continue
            if re.match(r'^\d{1,4}$', cells.get('sl', '')):
                if cur:
                    recs.append(cur)
                cur = {'sl': int(cells['sl']), 'page': pno + 1, 'state': state,
                       'sector': sector, 'name': ptxt, 'approval': cells.get('appr'),
                       'doc': [cells.get('doc')], 'cost': [cells.get('cost')],
                       'exp': [cells.get('exp')]}
            elif cur and cur['approval'] and not cells.get('sl') and not ptxt and \
                    re.match(r'^\d{1,2}/\d{4}$', cells.get('appr', '')):
                # A row whose serial number AND project name are blank in the report
                # itself (printing gap), but whose date and cost columns are filled.
                recs.append(cur)
                cur = {'sl': cur['sl'] + 1, 'page': pno + 1, 'state': state,
                       'sector': sector, 'name': '(name missing in report)',
                       'approval': cells.get('appr'), 'doc': [cells.get('doc')],
                       'cost': [cells.get('cost')], 'exp': [cells.get('exp')],
                       'name_missing': True}
            elif cur:
                if ptxt:
                    cur['name'] += ' ' + ptxt
                for k in ('doc', 'cost', 'exp'):
                    if cells.get(k):
                        cur[k].append(cells[k])
    if cur:
        recs.append(cur)
    pdf.close()
    return recs, headings


def clean(v):
    if v is None:
        return None
    v = v.strip().strip('()[]').replace(',', '')
    return None if v in ('-', '') else v


def by_bracket(tokens, plain_ok):
    """Sort the values of one combined column by their brackets, not by position.

    plain   -> first value without brackets   (original / money spent)
    (paren) -> value in round brackets        (revised / reported overrun)
    [brack] -> value in square brackets       (anticipated / time overrun)
    plain_ok: regex a plain value must match (rejects page numbers in the footer).
    """
    plain = paren = brack = None
    for t in tokens:
        if t is None:
            continue
        t = t.strip()
        if t.startswith('['):
            brack = brack or t
        elif t.startswith('('):
            paren = paren or t
        elif plain is None and re.match(plain_ok, t):
            plain = t
    return [plain, paren, brack]


DATE_RE = r'^\d{1,2}/\d{4}$'
NUM_RE = r'^-?[\d,]+(\.\d+)?$'


def tidy(r):
    d = by_bracket(r['doc'], DATE_RE)
    c = by_bracket(r['cost'], NUM_RE)
    e = by_bracket(r['exp'], NUM_RE)
    return {
        'sl': r['sl'], 'page': r['page'], 'state': r['state'], 'sector': r['sector'],
        'name': ' '.join(r['name'].split()),
        'name_missing': bool(r.get('name_missing', False)),
        'approval': clean(r['approval']),
        'doc_original': clean(d[0]), 'doc_revised': clean(d[1]), 'doc_anticipated': clean(d[2]),
        'cost_original': clean(c[0]), 'cost_revised': clean(c[1]), 'cost_anticipated': clean(c[2]),
        'expenditure': clean(e[0]), 'cost_overrun_cr': clean(e[1]), 'time_overrun_m': clean(e[2]),
    }


if __name__ == '__main__':
    path = sys.argv[1]
    out = sys.argv[2]
    recs, headings = parse(path)
    rows = [tidy(r) for r in recs]
    with open(out, 'w', newline='', encoding='utf-8') as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)
    with open(out.replace('.csv', '_headings.csv'), 'w', newline='', encoding='utf-8') as f:
        w = csv.writer(f)
        w.writerow(['page', 'kind', 'raw_text', 'mapped', 'how'])
        w.writerows(headings)
    print('parsed rows:', len(rows), '| headings:', len(headings))
