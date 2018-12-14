(include "core.scm")

(use getopt-long)

(define options-grammar
  '(;; Required parameters
    (input-shape "Input shape"
                 (required #t)
                 (value #t))
    (filter-shape "Filter shape"
                  (required #t)
                  (value #t))
    (pooling "Pooling function"
             (single-char #\p)
             (required #t)
             (value #t))
    ;; Optional parameters
    (stride "Stride"
            (single-char #\s)
            (required #f)
            (value #t))))

(define (poolings p)
  (cond ((equal? p "max") max)
        ;; TODO: raise error
        (else #f)))

;; TODO: implement padding
(define (pool v input-shape filter-shape stride)
  (let* ((input-height (car input-shape))
         (input-width (cadr input-shape))
         ;; Input shape must be of three dimensions
         (input-depth (caddr input-shape))
         (filter-height (car filter-shape))
         (filter-width (cadr filter-shape))
         ;; Output height and width indicate how many fields "fit" into input
         (output-height (+ (/ (- input-height filter-height) stride) 1))
         (output-width (+ (/ (- input-width filter-width) stride) 1))
         (output-size (* output-height output-width input-depth))
         (output (make-f64vector output-size 0))) ;; TODO: don't need to initialize to zeros
    ;; To start, pool for first field at 0,0
    (let outer ((i 0)
                (j 0)
                (k 0))
      (cond ((>= k input-depth)   output)
            ((>= i output-height) (outer 0 0 (+ k 1)))
            ((>= j output-width)  (outer (+ i 1) 0 k))
            (else
             ;; Offset into output vector based on index of field
             (let ((offset (+ (* i output-width input-depth)
                              (* j input-depth)
                              k)))
               (let inner ((row 0)
                           (col 0)
                           (vals '()))
                 (cond ((>= row filter-height)
                        ;; TODO: use selected pooling function
                        (begin
                          (f64vector-set! output offset (apply max vals))
                          (outer i (+ j 1) k)))
                       ((>= col filter-width) (inner (+ row 1) 0 vals))
                       ;; TODO: stride needs to be applied internally to field, here!
                       (else (let ((index (+ ;; row offset
                                           (* (+ (* i stride) row) input-width input-depth)
                                           ;; col offset
                                           (* (+ (* j stride) col) input-depth)
                                           ;; depth offset
                                           k)))
                               (inner row (+ col 1) (cons (f64vector-ref v index) vals))))))))))))

(let* ((options (getopt-long (command-line-arguments) options-grammar))
       ;; Options
       (input-shape (read-shape (option-value 'input-shape options)))
       (filter-shape (read-shape (option-value 'filter-shape options)))
       (pooling (poolings (option-value 'pooling options)))
       (stride (if (option-exists? 'stride options)
                   (string->number (option-value 'stride options))
                   1)))
  (read-input
   (lambda (x)
     (let* ((x (create-f64vector x (length x)))
            (output (pool x input-shape filter-shape stride)))
       (print-output output ",")))))
