(require-extension srfi-4)

(define (approx=? a b)
  (< (abs (- a b)) 1E-6))

(define (f64vector-approx=? a b)
  (and (= (f64vector-length a) (f64vector-length b))
       (let loop ((i 0)
                  (result #t))
         (cond ((>= i (f64vector-length a))
                result)
               ((approx=? (f64vector-ref a i) (f64vector-ref b i))
                (loop (+ i 1) #t))
               (else
                #f)))))
