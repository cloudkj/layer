(declare (unit full)
         (uses core functions options vectors))

(use blas)

(define (create-input x input-shape bias?)
  (let ((len (length x)))
    (if (not bias?)
        (create-f64vector x len)
        (let* ((dim (last input-shape))
               (output-size (* (/ len dim) (+ dim 1)))
               (output (make-f64vector output-size)))
          (let loop ((vals x)
                     (output-index 0))
            (if (= (length vals) 0)
                output
                (begin
                  ;; Set bias
                  (f64vector-set! output output-index 1.0)
                  ;; Set remaining `dim` values
                  (loop (let inner ((d 0)
                                    (v vals))
                          (if (>= d dim)
                              v
                              (begin
                                (f64vector-set! output (+ output-index d 1) (car v))
                                (inner (+ d 1) (cdr v)))))
                        (+ output-index (+ dim 1))))))))))

(define (forward x w input-shape num-neurons bias?)
  (let* (;; GEMM inputs
        (m (fold * 1 (drop-right input-shape 1)))
        (k (if bias? (+ 1 (last input-shape)) (last input-shape)))
        (n num-neurons)
        (c (make-f64vector (* m n))))
    (dgemm RowMajor NoTrans NoTrans m n k
           1    ;; alpha
           x w  ;; A, B (input matrices)
           0    ;; beta
           c))) ;; C (output matrix

(define (full options-lookup)
  (let* ((input-shape (read-shape (options-lookup input-shape-option)))
         (weights (options-lookup weights-option))
         (biases (options-lookup biases-option))
         ;; Weights
         (w (read-weights weights biases))
         ;; Number of neurons, inferred from input shape and weights
         (num-neurons (/ (f64vector-length w)
                         (if biases (+ 1 (last input-shape)) (last input-shape))))
         (activate (activations (options-lookup function-option))))
    (begin
      ;; TODO: add assertions on input size versus dimensions of weights
      (read-input
       (lambda (x)
         (let* ((x (create-input x input-shape biases))
                (output (forward x w input-shape num-neurons biases))
                (a (activate output num-neurons)))
           (print-output a ",")))))))
