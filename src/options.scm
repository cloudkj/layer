(declare (unit options))

;;; Definitions of required parameters

(define filter-shape-option
  '(filter-shape "Filter shape"      (required #t) (value (required "n,m,..."))))

(define input-shape-option
  '(input-shape  "Input shape"       (required #t) (value (required "n,m,..."))))

(define num-filters-option
  '(num-filters  "Number of filters" (required #t) (value (required num))))

(define weights-option
  '(weights      "Weights file"      (required #t) (value (required filename)) (single-char #\w)))

;;; Definitions of optional parameters
(define stride-option
  '(stride       "Stride"            (required #f) (value (required num))))

(define biases-option
  '(biases       "Biases file"       (required #f) (value (required filename)) (single-char #\b)))

(define function-option
  '(function     "Function"          (required #f) (value (required name)) (single-char #\f)))

;;; Layer options

(define (sort-options options)
  (sort options
        (lambda (a b) (string< (symbol->string (car a))
                               (symbol->string (car b))))))

(define layer-options
  (list input-shape-option))

(define weighted-layer-options
  (sort-options (append layer-options
                        (list weights-option
                              biases-option
                              function-option))))

(define conv-layer-options
  (sort-options (append weighted-layer-options
                        (list filter-shape-option
                              num-filters-option))))

(define pool-layer-options
  (sort-options (append layer-options
                        (list filter-shape-option
                              function-option
                              stride-option))))

;;; Helpers

;; Returns a lookup function for looking up option values by the option
;; definition. The lookup function optionally takes a function to apply to the
;; value of the option, if it exists. If the option does not exist, the lookup
;; will return a default value (if supplied), or false.
(define (make-options-lookup options)
  (lambda (option-definition #!optional default f)
    (let* ((key (car option-definition))
           (val (assoc key options)))
      (cond ((not val) default)
            ((not f) (cdr val))
            (else (f (cdr val)))))))
