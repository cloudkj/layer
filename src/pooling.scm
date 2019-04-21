(declare (unit pooling)
         (uses core functions options vectors))

(define (pool v input-shape filter-shape stride f)
  ;; TODO: implement padding
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
         (output (make-f64vector output-size)))
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
                        (begin
                          (f64vector-set! output offset (f vals))
                          (outer i (+ j 1) k)))
                       ((>= col filter-width) (inner (+ row 1) 0 vals))
                       (else (let ((index (+ ;; row offset
                                           (* (+ (* i stride) row) input-width input-depth)
                                           ;; col offset
                                           (* (+ (* j stride) col) input-depth)
                                           ;; depth offset
                                           k)))
                               (inner row (+ col 1) (cons (f64vector-ref v index) vals))))))))))))

(define (pooling options-lookup)
  (let* ((input-shape (read-shape (options-lookup input-shape-option)))
         (filter-shape (read-shape (options-lookup filter-shape-option)))
         (f (poolings (options-lookup function-option)))
         (stride (options-lookup stride-option 1 string->number)))
    (read-input
     (lambda (x)
       (let* ((x (create-f64vector x (length x)))
              (output (pool x input-shape filter-shape stride f)))
         (print-output output ","))))))
