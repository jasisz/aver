import sys
b = open(sys.argv[1], 'rb').read()


def uleb(i):
    r = s = 0
    while True:
        x = b[i]
        i += 1
        r |= (x & 0x7f) << s
        s += 7
        if x < 128:
            return r, i


i = 8
secs = []
while i < len(b):
    sid = b[i]
    sz, j = uleb(i + 1)
    secs.append((sid, j, sz))
    i = j + sz
names = {0: 'custom', 1: 'type', 2: 'import', 3: 'func', 4: 'table', 5: 'mem', 6: 'global',
         7: 'export', 8: 'start', 9: 'elem', 10: 'code', 11: 'data', 12: 'datacount'}
print('module', len(b))
for sid, j, sz in secs:
    print(names.get(sid, sid), 'offset', j, 'size', sz)
for sid, j, sz in secs:
    if sid == 10:
        n, k = uleb(j)
        L = []
        for _ in range(n):
            s, k2 = uleb(k)
            L.append(s + (k2 - k))
            k = k2 + s
        L.sort()
        print('code entries', n, 'sum', sum(L), 'max5', L[-5:], 'p50', L[n // 2],
              'p90', L[int(n * .9)], 'p99', L[int(n * .99)])
        print('sum len^2/2 (MB)', sum(x * x for x in L) / 2 / 1e6)
