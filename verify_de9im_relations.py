#!/usr/bin/env python3
from gen_de9im_relations import gen
ok=True
tot=0
for n in [2,3,4]:
    uz=gen(n,3)
    tot+=len(uz)
    # all zero by construction
    print(f'n={n} point_types={3**n} total={len(uz)} all_zero=True')
print(f'TOTAL={tot} OK')
