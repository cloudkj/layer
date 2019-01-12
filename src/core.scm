(declare (unit core))

(use input-parse)

;;; Options handling

(define (option-exists? name options)
  (any (lambda (o) (equal? (car o) name)) options))

(define (option-value name options)
  (let ((option (find (lambda (o) (equal? (car o) name)) options)))
    (when option (cdr option))))

;;; I/O

(define (print-output v separator)
  (let ((len (f64vector-length v)))
    (let loop ((i 0))
      (if (>= i len)
          (display #\newline)
          (begin
            (when (> i 0) (display separator))
            (display (f64vector-ref v i))
            (loop (+ i 1)))))))

(define (read-input f)
  (let ((line (read-line)))
    (when (not (eof-object? line))
          (begin
            (f (map string->number (string-split line ",")))
            (read-input f)))))

(define (read-shape str)
  (map string->number (string-split str ",")))

;; Returns a numeric vector of the weights, and optionally biases, in the given
;; input files.
(define (read-weights weights biases)
  (define (read port)
    (let loop ((values '()))
      (let ((token (string->number (next-token '() '(#\, #\newline *eof*) "" port)))
            (c (read-char port)))
        (if (eof-object? c)
            values
            (loop (cons token values))))))
  (let* ((w (read (open-input-file weights)))
         ;; Append biases to end of since values are in reverse order
         ;; TODO: assert `biases` is of shape 1xC where C is number of columns in `weights`
         ;; TODO: assert count of biases = 1
         (w (if biases (append w (read (open-input-file biases))) w))
         (len (length w)))
    ;; Initialize and set a vector with weights in reverse order
    (let loop ((v (make-f64vector len))
               (i (- len 1))
               (vals w))
      (if (< i 0)
          v
          (begin
            (f64vector-set! v i (car vals))
            (loop v (- i 1) (cdr vals)))))))
