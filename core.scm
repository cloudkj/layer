(declare (unit core))

(use blas input-parse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debugging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Prints the 2D representation of a list of values as an h-by-w matrix
(define (print-2d v h w)
  (let loop ((vals v)
             (i 0)
             (j 0)
             (s ""))
    (cond ((>= i h) (print s))
          ((>= j w) (loop vals (+ i 1) 0 (string-append s "\n")))
          (else (loop (cdr vals) i (+ j 1)
                      (string-append s " " (if (= (car vals) 0.0) "_" "x")))))))

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

(define (read-input f)
  (let ((line (read-line)))
    (when (not (eof-object? line))
          (begin
            (f (map string->number (string-split line ",")))
            (read-input f)))))

(define (read-shape str)
  (map string->number (string-split str ",")))

;; Returns pair where the first element is the count of number of lines, and
;; the second element is the list of numeric values representing the weights.
(define (read-weights weights biases)
  (define (read port)
    (let loop ((count 0)
               (values '()))
      (let ((token (string->number (next-token '() '(#\, #\newline *eof*) "" port)))
            (c (read-char port)))
        (cond ((eof-object? c) (cons count values))
              ((equal? c #\,)
               (loop count (cons token values)))
              (else
               (loop (+ count 1) (cons token values)))))))
  (if biases
      (let* ((b (read (open-input-file biases)))
             (w (read (open-input-file weights)))
             (count (+ (car w) (car b)))
             ;; Append biases to end of since values are in reverse order
             (values (append (cdr w) (cdr b))))
        ;; TODO: assert `biases` is of shape 1xC where C is number of columns in `weights`
        ;; TODO: assert count of biases = 1
        (cons count values))
      (read (open-input-file weights))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Initializes and sets a vector with values, in reverse order
(define (create-f32vector values len)
  (let loop ((v (make-f32vector len))
             (i (- len 1))
             (vals values))
    (if (< i 0)
        v
        (begin
          (f32vector-set! v i (car vals))
          (loop v (- i 1) (cdr vals))))))

(define (create-f64vector values len)
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

(define (f64v-join v separator)
  (f64v-fold (lambda (res val)
               (if (> (string-length res) 0)
                   (string-append res separator (number->string val))
                   (string-append res (number->string val))))
             ""
             v))

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

(define (relu v)
  (f64v-map! v (lambda (z) (max 0 z))))

(define (sigmoid v)
  (f64v-map! v (lambda (z) (/ 1 (+ 1 (exp (- z)))))))

(define (softmax v)
  (let* ((numer (f64v-map! (dcopy v) exp))
         (denom (f64v-fold (lambda (sum x) (+ sum x)) 0 numer)))
    (f64v-map! numer (lambda (x) (/ x denom)))))

(define (activations a)
  (cond ((equal? a "relu") relu)
        ((equal? a "sigmoid") sigmoid)
        ((equal? a "softmax") softmax)
        (else identity)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define order RowMajor)
(define transa Trans)
(define alpha 1)
(define beta 0)

(define (forward m n w x y activation)
  (activation (dgemv order transa m n alpha w x beta y)))
