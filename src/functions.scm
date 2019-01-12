(declare (unit functions)
         (uses vectors))

(use srfi-4)

;;; Activation functions

(define (relu z)
  (max 0 z))

(define (sigmoid z)
  (/ 1 (+ 1 (exp (- z)))))

(define (tanh z)
  (let ((e^2z (exp (* 2 z))))
    (/ (- e^2z 1) (+ e^2z 1))))

(define element-wise-functions
  (list (cons "relu" relu)
        (cons "sigmoid" sigmoid)
        (cons "tanh" tanh)))

;;; Normalization functions

(define softmax!
  (let ((f (lambda (v from to)
             (let ((sum (f64vector-fold-map! exp + 0 v from to)))
               (f64vector-map! (lambda (x) (/ x sum)) v from to)))))
    (case-lambda
     ((v)   (f v 0 (f64vector-length v)))
     ((v k) (let ((n (f64vector-length v)))
              (let loop ((i 0))
                (if (>= i n)
                    v
                    (begin
                      (f v i (+ i k))
                      (loop (+ i k))))))))))

;; Returns an activation function for the given name. An activation function
;; takes a vector as input, along with the number of elements to normalize over
;; for normalization functions (e.g. softmax). Element-wise activation functions
;; simply ignore the second parameter.
(define (activations name)
  (let ((efn (assoc name element-wise-functions)))
    (cond (efn (lambda (v k) (f64vector-map! (cdr efn) v)))
          ((equal? name "softmax") softmax!)
          ;; Identity function
          (else (lambda (v k) v)))))
