(declare (unit vectors))

(use srfi-4)

;;; Vectors

;; Creates and initializes a vector of size len with a list of values
(define (create-f64vector values len)
  (let loop ((v (make-f64vector len))
             (i 0)
             (vals values))
    (if (>= i len)
        v
        (begin
          (f64vector-set! v i (car vals))
          (loop v (+ i 1) (cdr vals))))))

;; Destructively maps a function f over a vector.
(define f64vector-map!
  (let ((fn (lambda (f v from to)
              (cond ((< from 0)                  (error "index out of vector bounds" from))
                    ((> to (f64vector-length v)) (error "index out of vector bounds" to))
                    (else
                     (let loop ((i from))
                       (if (>= i to)
                           v
                           (begin
                             (f64vector-set! v i (f (f64vector-ref v i)))
                             (loop (+ i 1))))))))))
    (case-lambda
     ((f v)         (fn f v 0 (f64vector-length v)))
     ((f v from to) (fn f v from to)))))

;; Destructively maps a function f1 over a vector while folding a function f2
;; over the outputs and returns the resulting value.
(define f64vector-fold-map!
  (let ((fn (lambda (f1 f2 x0 v from to)
              (cond ((< from 0)                  (error "index out of vector bounds" from))
                    ((> to (f64vector-length v)) (error "index out of vector bounds" to))
                    (else
                     (let loop ((i from)
                                (x x0))
                       (if (>= i to)
                           x
                           (let ((xi (f1 (f64vector-ref v i))))
                             (f64vector-set! v i xi)
                             (loop (+ i 1) (f2 x xi))))))))))
    (case-lambda
     ((f1 f2 x0 v)         (fn f1 f2 x0 v 0 (f64vector-length v)))
     ((f1 f2 x0 v from to) (fn f1 f2 x0 v from to)))))
