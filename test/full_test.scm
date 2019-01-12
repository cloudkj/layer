(require-extension srfi-78)

(include "test/common.scm")
(include "src/vectors.scm")
(include "src/full.scm")

(check (create-input '(0.05 0.1) '(2) #f) => (f64vector 0.05 0.1))
(check (create-input '(0.05 0.1) '(2) #t) => (f64vector 1 0.05 0.1))

(check (forward (f64vector 1 0.05 0.1)
                (f64vector 0.35 0.35 0.15 0.25 0.2 0.3)
                '(2)
                2
                #t)
       => (f64vector 0.3775 0.3925))

(check (forward (f64vector 1 0.593269992 0.596884378)
                (f64vector 0.6 0.6 0.4 0.5 0.45 0.55)
                '(2)
                2
                #t)
       (=> f64vector-approx=?)
       (f64vector 1.105905967 1.224921402))
