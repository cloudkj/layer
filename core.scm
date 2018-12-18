(declare (unit core))

(use blas input-parse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Options handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (option-exists? name options)
  (any (lambda (o) (equal? (car o) name)) options))

(define (option-value name options)
  (let ((option (find (lambda (o) (equal? (car o) name)) options)))
    (when option (cdr option))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I/O
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-output v separator)
  (let ((len (f64vector-length v)))
    (let loop ((i 0))
      (if (>= i len)
          (display #\newline)
          (begin
            (when (> i 0) (display separator))
            (display (f64vector-ref v i))
            (loop (+ i 1)))))))

(define (read-input f)
  (let ((line (read-line)))
    (when (not (eof-object? line))
          (begin
            (f (map string->number (string-split line ",")))
            (read-input f)))))

(define (read-shape str)
  (map string->number (string-split str ",")))

;; Returns a numeric vector of the weights, and optionally biases, in the given
;; input files.
(define (read-weights weights biases)
  (define (read port)
    (let loop ((values '()))
      (let ((token (string->number (next-token '() '(#\, #\newline *eof*) "" port)))
            (c (read-char port)))
        (if (eof-object? c)
            values
            (loop (cons token values))))))
  (let* ((w (read (open-input-file weights)))
         ;; Append biases to end of since values are in reverse order
         ;; TODO: assert `biases` is of shape 1xC where C is number of columns in `weights`
         ;; TODO: assert count of biases = 1
         (w (if biases (append w (read (open-input-file biases))) w)))
    (create-f64vector-reverse w (length w))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Initializes and sets a vector with values
(define (create-f64vector values len)
  (let loop ((v (make-f64vector len))
             (i 0)
             (vals values))
    (if (>= i len)
        v
        (begin
          (f64vector-set! v i (car vals))
          (loop v (+ i 1) (cdr vals))))))

;; Initializes and sets a vector with values, in reverse order
(define (create-f64vector-reverse values len)
  (let loop ((v (make-f64vector len))
             (i (- len 1))
             (vals values))
    (if (< i 0)
        v
        (begin
          (f64vector-set! v i (car vals))
          (loop v (- i 1) (cdr vals))))))

(define (f64v-fold f init v)
  (define (helper i accum)
    (if (>= i (f64vector-length v))
        accum
        (helper (+ i 1) (f accum (f64vector-ref v i)))))
  (helper 0 init))

;; TODO: change param order
;; TODO: make choice between pure/destructive vector ops, trade off efficiency
(define (f64v-map! v f)
  (let loop ((i 0))
    (if (>= i (f64vector-length v))
        v
        (begin
          (f64vector-set! v i (f (f64vector-ref v i)))
          (loop (+ i 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (relu! v #!optional n)
  (f64v-map! v (lambda (z) (max 0 z))))

(define (sigmoid! v #!optional n)
  (f64v-map! v (lambda (z) (/ 1 (+ 1 (exp (- z)))))))

;; Apply softmax to n elements at a time
(define (softmax-n! v n)
  (let loop ((i 0))
    (if (>= i (f64vector-length v))
        v
        ;; Exponentiate each element and sum the values
        (let ((sum (let expo ((j i)
                              (sum 0))
                     (if (>= j (+ i n))
                         sum
                         (let ((ej (exp (f64vector-ref v j))))
                           (f64vector-set! v j ej)
                           (expo (+ j 1) (+ sum ej)))))))
          ;; Normalize each element
          (let normalize ((j i))
            (if (>= j (+ i n))
                (loop (+ i n))
                (begin
                  (f64vector-set! v j (/ (f64vector-ref v j) sum))
                  (normalize (+ j 1)))))))))

(define (softmax! v #!optional n)
  (if n
      (softmax-n! v n)
      (let* ((numer (f64v-map! (dcopy v) exp))
             (denom (f64v-fold (lambda (sum x) (+ sum x)) 0 numer)))
        (f64v-map! numer (lambda (x) (/ x denom))))))

(define (activations a)
  (cond ((equal? a "relu") relu!)
        ((equal? a "sigmoid") sigmoid!)
        ((equal? a "softmax") softmax!)
        (else (lambda (x #!optional n) x))))
