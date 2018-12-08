(declare (uses core))

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

(let* ((options (getopt-long (command-line-arguments) options-grammar))
       (bias? (option-exists? 'biases options))
       (_ (read-weights (option-value 'weights options)
                      (if bias? (option-value 'biases options) #f)))
       (m (car _))
       (n (/ (length (cdr _)) m))
       (w (create-vector (cdr _)))
       (activation (activations (option-value 'activation options))))
  (begin
    ;; TODO: remove debugging
    (format (current-error-port) "activation: ~A\n" activation)
    (format (current-error-port) "w: (shape: ~Ax~A)\n" m n)
    ;; TODO: add assertions on input size versus dimensions of weights
    (read-input (lambda (x)
                  (let* ((x (apply f64vector (if bias? (cons 1.0 x) x)))
                         (y (make-f64vector n 0.0)))
                    (print (f64v-join (forward m n w x y activation) ",")))))))
