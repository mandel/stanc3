  $ $TESTDIR/..//../../_build/default/stanc.exe "$TESTDIR/..//map_rect/bad_data_r_type.stan"
  Semantic error at file ".*/examples-bad/map_rect/..//map_rect/bad_data_r_type.stan", line 19, characters 8-68: (re)
  Ill-typed arguments supplied to function map_rect. Available signatures: 
  ((vector, vector, data real[], data int[]) => vector, vector, vector[], data real[][], data int[][]) => vector
  Instead supplied arguments of incompatible type: (vector, vector, real[], int[]) => vector, vector, vector[], real, int[][].
  [1]
