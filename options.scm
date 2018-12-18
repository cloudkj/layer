(declare (unit options))

;;; Definitions of required parameters

(define filter-shape-option
  '(filter-shape "Filter shape"      (required #t) (value #t)))

(define input-shape-option
  '(input-shape "Input shape"        (required #t) (value #t)))

(define num-filters-option
  '(num-filters "Number of filters"  (required #t) (value #t)))

(define weights-option
  '(weights     "Weights file"       (required #t) (value #t) (single-char #\w)))

;;; Definitions of optional parameters

(define stride-option
  '(stride      "Stride"             (required #f) (value #t) (single-char #\s)))

(define biases-option
  '(biases      "Biases file"        (required #f) (value #t) (single-char #\b)))

(define function-option
  '(function    "Function"           (required #f) (value #t) (single-char #\f)))

;;; Layer options

(define layer-options
  (list input-shape-option))

(define weighted-layer-options
  (append layer-options
          (list weights-option
                biases-option
                function-option)))

(define conv-layer-options
  (append weighted-layer-options
          (list filter-shape-option
                num-filters-option)))

(define pool-layer-options
  (append layer-options
          (list filter-shape-option
                function-option
                stride-option)))

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
