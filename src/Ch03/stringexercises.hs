module StringExercises where

-- will not compile since d is not in scope
area d = pi * (r * r)
r = d / 2

-- compiles, but when called gives an error that d is not in scope
area d = pi * (r * r)
  where r = d / 2
