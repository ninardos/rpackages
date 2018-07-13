1: correct
2: split at position 10
3: split at position 30
4: split at position 30, 60
5: split at position 30, 40, 50, 60
6: combine at position 10
7: combine at position 30
8: combine at position 30, 60
9: combine at position 30, 40, 50, 60
10: combine at position 30, 50, split at position 40, 60

(splits roughly 30/70)

further ideas: combine / split at end of data (position l-5, l-10)
pack errors more densely, e. g. 11s, 12c, 13s

Investigate why only sets 4,5 and 9,10 are marked as non-integer data.
-> lower limit 500
-> upper limit 2000
7.5% margin for doubles
limit of 1 upper / lower, so 2 lower / upper is enough to mark data as invalid, this is usually reached at each split, and at combines 50%

Use testdata-sets for import to GC.

