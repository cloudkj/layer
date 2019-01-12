(require-extension srfi-78)

(include "test/common.scm")
(include "src/functions.scm")
(include "src/vectors.scm")
(include "src/pooling.scm")

(check (pool (f64vector 1 1 2 4 5 6 7 8 3 2 1 0 1 2 3 4)
             '(4 4 1)
             '(2 2)
             2
             (poolings "max"))
       => (f64vector 6 8 3 4))

(check (pool (f64vector 1 0 2 3 4 6 6 8 3 1 1 0 1 2 2 4)
             '(4 4 1)
             '(2 2)
             2
             (poolings "max"))
       => (f64vector 6 8 3 4))

(check (pool (f64vector 1 1 2 4 5 6 7 8 3 2 1 0 1 2 3 4)
             '(4 4 1)
             '(2 2)
             2
             (poolings "avg"))
       => (f64vector 3.25 5.25 2 2))

(check (pool (f64vector 1 0 2 3 4 6 6 8 3 1 1 0 1 2 2 4)
             '(4 4 1)
             '(2 2)
             2
             (poolings "avg"))
       => (f64vector 2.75 4.75 1.75 1.75))
