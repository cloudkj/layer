(include "core.scm")

(use getopt-long)

(define options-grammar
  '(;; Required parameters
    (weights "Weights file"
             (single-char #\w)
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

;; Extract the weights for the `i`th filter from a flattened weights vector
(define (filter-weights v i num-filters)
  (let* ((len (/ (f64vector-length v) num-filters))
         (output (make-f64vector len)))
    (let loop ((j 0))
      (if (>= j len)
          output
          (begin
            (f64vector-set! output j (f64vector-ref v (+ (* j num-filters) i)))
            (loop (+ j 1)))))))

(let* ((options (getopt-long (command-line-arguments) options-grammar))
       (_ (read-weights (option-value 'weights options) #f))
       (len (length (cdr _)))
       (w (create-f64vector (cdr _) len)))

  ;; TODO: from the weights w read in as a flattened vector, extract the specific
  ;; weights for each filter separately as a vector
  (let* ((fw (filter-weights w 0 2))
  ;; TODO: read an input x as flattened vector, and implement `im2col` to extract
  ;; each patch of FxF size as a vector

  ;; TODO: parameterize hardcoded shape. Shape: 28x28x1
         (cols (let* ((line (read-line))
                      (xl (map string->number (string-split line ",")))
                      (x (apply f64vector xl)))
                 (print-2d xl 28 28 )
                 (im2col x 28 28 8 8 ))))
    (print "field weights: " (f64vector-length fw))

    (print fw)
    
    (print "cols: " (f64vector-length cols))

    (print-2d (f64vector->list cols) 21 64)
    
    (print-2d
     (f64vector->list
      (sgemv RowMajor NoTrans 441 64 1 cols fw 0 (make-f64vector 441)))
     21 21)
    
    (print (sgemv RowMajor NoTrans 441 64 1 cols fw 0 (make-f64vector 441)))

    )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Synthetic test case
  ;; (let* ((x (f64vector 0 0 0 0 0
  ;;                      0 0 1 0 0
  ;;                      0 1 1 0 0
  ;;                      0 1 1 1 0
  ;;                      0 0 1 0 0))
  ;;        (cols (im2col x 5 5 3 3)))
  ;;   (print-2d (f64vector->list x) 5 5)
  ;;   (print cols)
  ;;   (print (f64vector-length cols))
  ;;   (print-2d (f64vector->list cols) 9 9))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;; TODO: matrix multiply on matrix of input patches with matrix of weights
  )
