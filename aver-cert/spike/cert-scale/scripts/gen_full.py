"""One declaration per plan entry: the full per-plan check, packed code."""
import sys
build, n, per = sys.argv[1], int(sys.argv[2]), int(sys.argv[3])
names = []
for k in range(0, n, per):
    nm = f'ProtoFull{k}'
    with open(f'{build}/{nm}.lean', 'w') as f:
        f.write('import ProtoFullDefs\n\nset_option maxRecDepth 200000\nset_option maxHeartbeats 0\n\n'
                'namespace AverCert.Proto\n\n')
        for i in range(k, min(n, k + per)):
            f.write(f'theorem entry_{i} : entryPackedAt {i} = true := by decide +kernel\n')
        f.write('\nend AverCert.Proto\n')
    names.append(nm)
print(' '.join(names))
