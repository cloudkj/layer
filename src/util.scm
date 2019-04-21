;;; Utility functions

(declare (unit util))

;;; Logging

;; Logs a variadic list of values by printing to stderr
(define (util-log . args)
  (let ((port (current-error-port)))
    (let loop ((a args))
      (if (= (length a) 0)
          (newline port)
          (begin
            (display (car a) port)
            (display " " port)
            (loop (cdr a)))))))

;; Logs the 2D representation of a list of numeric values as an h-by-w matrix
(define (util-log-list-as-matrix v h w)
  (let loop ((vals v)
             (i 0)
             (j 0)
             (s ""))
    (cond ((>= i h) (print s))
          ((>= j w) (loop vals (+ i 1) 0 (string-append s "\n")))
          (else (loop (cdr vals) i (+ j 1)
                      (string-append s " "
                                     (if (= (car vals) 0.0)
                                         "_"
                                         (number->string
                                          (modulo
                                           (inexact->exact (floor (* 10 (car vals))))
                                           10)))))))))
