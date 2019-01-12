(require-extension srfi-4 srfi-78)

(include "test/common.scm")
(include "src/vectors.scm")

(define (*2 x) (* 2 x))

(check (create-f64vector '(1 2 3) 3) => (f64vector 1 2 3))
(check (create-f64vector '(1 2 3) 2) => (f64vector 1 2))

(check (f64vector-map! *2 (f64vector 1 2 3)) => (f64vector 2 4 6))
(check (f64vector-map! *2 (f64vector 1 2 3) 0 2) => (f64vector 2 4 3))

(let ((v (f64vector 1 2 3)))
  (check (f64vector-fold-map! *2 + 0 v) => 12.0)
  (check v => (f64vector 2 4 6)))

(let ((v (f64vector 1 2 3)))
  (check (f64vector-fold-map! *2 + 0 v 0 2) => 6.0)
  (check v => (f64vector 2 4 3)))
