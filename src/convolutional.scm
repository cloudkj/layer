(declare (unit convolutional)
         (uses core functions options vectors))

(use blas)

;; For input shape H x W, field shape FH x FW, padding P, stride S, output shape is:
;;
;;   output height = (H - FH + 2P) / S + 1
;;   output width  = (W - FW + 2P) / S + 1
;;
;; Total number of fields = output height * output width
(define (im2col v input-shape field-height field-width bias?)
  (let* ((bias (if bias? 1 0))
         (input-height (car input-shape))
         (input-width (cadr input-shape))
         ;; Size of each "pixel" is product of remaining input dimensions
         (pixel-size (fold * 1 (cddr input-shape)))
         ;; Output height and width indicate how many fields "fit" into input
         (output-height (+ (- input-height field-height) 1)) ;; TODO: add padding/stride
         (output-width (+ (- input-width field-width) 1))
         ;; Number of values in each field, plus bias
         (field-size (+ bias (* field-height field-width pixel-size)))
         (output-size (* output-height output-width field-size))
         (output (make-f64vector output-size 0))) ;; TODO: don't need to initialize to zeros
    ;; i, j are offsets into the input matrix for each field
    (let outer ((i 0)
                (j 0))
      (cond ((>= i output-height) output)
            ((>= j output-width) (outer (+ i 1) 0))
            (else
             ;; Offset into output vector based on index of field in output
             (let ((offset (+ (* i output-width field-size)
                              (* j field-size))))
               ;; Extract values for each field row by row
               (let inner ((row i))
                 (if (>= row (+ i field-height))
                     (begin
                       ;; Set bias once per field, as first value for the field in output
                       (when bias? (f64vector-set! output offset 1.0))
                       (outer i (+ j 1)))
                     ;; TODO: perf improvement: directly get columns with 1 subv op rather than 2
                     (let* (;; First, get subvector corresponding to entire row
                            (rfrom (* input-width pixel-size row))
                            (rto   (* input-width pixel-size (+ row 1)))
                            (r     (subf64vector v rfrom rto))
                            ;; Second, index into row and get subvector corresponding to columns
                            (cfrom (* j pixel-size))
                            (cto   (+ cfrom (* field-width pixel-size)))
                            (c (subf64vector r cfrom cto))
                            ;; Offset for copying values into output vector: offset of field in _output_ plus
                            ;; offset into output based on the index _within_ the field
                            (offset (+ offset (+ bias (* (- row i) field-height pixel-size)))))
                       (begin
                         ;; Copy columns into output vector
                         (copy-vector! c output offset)
                         (inner (+ row 1))))))))))))

(define (convolve xcols wrows input-shape filter-height filter-width num-filters bias?)
  ;; TODO: add padding/stride
  (let* ((input-height (car input-shape))
         (input-width (cadr input-shape))
         ;; Size of each "pixel" is product of remaining dimensions
         (pixel-size (fold * 1 (cddr input-shape)))
         (output-height (+ (- input-height filter-height) 1))
         (output-width (+ (- input-width filter-width) 1))
         ;; GEMM inputs
         (m (* output-height output-width))
         (n num-filters)
         (k (* filter-height filter-width pixel-size))
         (k (if bias? (+ k 1) k))
         (c (make-f64vector (* m n))))
    (dgemm RowMajor NoTrans NoTrans m n k
           1           ;; alpha
           xcols wrows ;; A, B (input matrices)
           0           ;; beta
           c)))        ;; C (output matrix)

(define (convolutional options-lookup)
  (let* ((input-shape (read-shape (options-lookup input-shape-option)))
         (filter-shape (read-shape (options-lookup filter-shape-option)))
         (num-filters (string->number (options-lookup num-filters-option)))
         (weights (options-lookup weights-option))
         (biases (options-lookup biases-option))
         ;; Weights
         (w (read-weights weights biases))
         ;; Hyperparameters
         (filter-height (car filter-shape))
         (filter-width (cadr filter-shape))
         (activate (activations (options-lookup function-option))))
    (read-input
     (lambda (x)
       (let* ((x (create-f64vector x (length x)))
              (xcols (im2col x input-shape filter-height filter-width biases))
              (output (convolve xcols w input-shape filter-height filter-width num-filters biases))
              (a (activate output num-filters)))
         (print-output a ","))))))
