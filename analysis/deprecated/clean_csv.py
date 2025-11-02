import sys, csv, re
inp, out = sys.argv[1], sys.argv[2]
with open(inp, newline='', encoding='utf-8') as inf, open(out, 'w', newline='', encoding='utf-8') as outf:
    r = csv.reader(inf)
    w = csv.writer(outf)
    for row in r:
        new = []
        for s in row:
            s = '' if s is None else s
            s = re.sub(r'[\r\n]+', ' ', s)
            s = re.sub(r'\s+', ' ', s).strip()
            if s.lower() in ('null', 'nan'):
                s = ''
            new.append(s)
        if any(cell != '' for cell in new):
            w.writerow(new)
