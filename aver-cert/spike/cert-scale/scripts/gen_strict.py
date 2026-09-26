"""Linear distinctness over keys the producer presents in sorted order."""
import sys
build = sys.argv[1]
keys = sorted(int(x) for x in open(sys.argv[2]).read().split(',') if x.strip())
with open(f'{build}/ProtoStrict.lean', 'w') as f:
    f.write('import SortedKeys\nset_option maxRecDepth 200000\nset_option maxHeartbeats 0\n')
    f.write('theorem exp_sorted_linear : AverCert.SortedKeys.strictly [')
    f.write(', '.join(str(k) for k in keys))
    f.write('] = true := by\n  decide +kernel\n')
print(len(keys))
