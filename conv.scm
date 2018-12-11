(include "core.scm")

(use getopt-long)

(define options-grammar
  '(;; Required parameters
    (weights "Weights file"
             (single-char #\w)
             (required #t)
             (value #t))
    (input-shape "Input shape"
                 (required #t)
                 (value #t))
    (filter-shape "Filter shape"
                  (required #t)
                  (value #t))
    (num-filters "Number of filters"
                 (required #t)
                 (value #t))
    ;; Optional parameters
    (biases "Biases file"
            (single-char #\b)
            (required #f)
            (value #t))
    (activation "Activation function"
                (single-char #\a)
                (required #f)
                (value #t))))

;; Returns the ith HxW field patch of a flattened vector.
;; 
;; For input shape H x W, field shape FH x FW, padding P, stride S, output shape is:
;;
;;   output height = (H - FH + 2P) / S + 1
;;   output width  = (W - FW + 2P) / S + 1
;;
;; Total number of patches = output height * output width
(define (im2col v input-height input-width field-height field-width)
  ;; Offsets go from [0, 0] to [output height, output width]
  ;; To start, must get values from `field-height` rows
  ;;   For each row, get values from `field-width` cols

  ;; E.g. offset 0, 0, get a 3x3 patch
  ;; For column = 0, get subvectors corresponding to row 0, row 1, row 2
;  (subf64vector v 0 input-width*1)
;  (subf64vector v input-width*1 input-width*2)
;  (subf64vector v input-width*2 input-width*3)

  ;; (i, j) are offsets into input matrix for the patch in question
  (let* ((output-height (+ (- input-height field-height) 1)) ;; TODO: add padding/stride
         (output-width (+ (- input-width field-width) 1))
         (field-size (* field-height field-width))
         (output (make-f64vector (* output-height output-width field-size) 0)))
    (let outer ((i 0)
                (j 0))
      (cond ((>= i output-height) output)
            ((>= j output-width) (outer (+ i 1) 0))
            (else
             (let inner ((row i))
               (if (>= row (+ i field-height))
                   (outer i (+ j 1))
                   (let* (;; First, get subvector corresponding to row
                          (r (subf64vector v (* input-width row) (* input-width (+ row 1))))
                          ;; Index into row and get subvector corresponding to columns
                          (f (subf64vector r j (+ j field-width))))
                     (begin
                       (copy-vector! f output
                                     (+
                                      ;; Offset into output based on index of field in output
                                      (+ (* i output-width field-size) (* j field-size))
                                      ;; Offset into output based on index within field
                                      (* (- row i) field-height)))
                       (inner (+ row 1)))))))))))

;; Copies all values in `from` and sets starting at offset index in `to`
(define (copy-vector! from to offset)
  (let loop ((i 0))
    (if (>= i (f64vector-length from))
        to
        (begin
          (f64vector-set! to (+ offset i) (f64vector-ref from i))
          (loop (+ i 1))))))

(define (convolve xcols wrows input-height input-width filter-height filter-width num-filters)
  ;; TODO: add padding/stride
  (let* ((output-height (+ (- input-height filter-height) 1))
         (output-width (+ (- input-width filter-width) 1))
         (c (make-f64vector (* output-height output-width num-filters))))
    (dgemm RowMajor NoTrans NoTrans
           (* output-height output-width) ;; m
           num-filters                    ;; n
           (* filter-height filter-width) ;; k
           1                              ;; alpha
           xcols wrows                    ;; a, b (input matrices)
           0                              ;; beta
           c)))                           ;; c (output matrix)

(let* ((options (getopt-long (command-line-arguments) options-grammar))
       ;; Options
       (input-shape (read-shape (option-value 'input-shape options)))
       (filter-shape (read-shape (option-value 'filter-shape options)))
       (num-filters (string->number (option-value 'num-filters options)))
       ;; Weights
       (_ (read-weights (option-value 'weights options) #f))
       (len (length (cdr _)))
       (w (create-f64vector (cdr _) len))
       ;; Hyperparameters
       (input-height (car input-shape))
       (input-width (cadr input-shape))
       (filter-height (car filter-shape))
       (filter-width (cadr filter-shape)))
  (read-input
   (lambda (x)
     (let* ((x (apply f64vector x))
            (xcols (im2col x input-height input-width filter-height filter-width))
            (output (convolve xcols w input-height input-width filter-height filter-width num-filters)))
       (print (f64v-join output  ","))))))
