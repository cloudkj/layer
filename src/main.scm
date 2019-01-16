(declare (uses functions options util convolutional full pooling))

(use getopt-long srfi-13)

(define prog-name "layer")

(define prog-desc "Neural network inference")

(define commands
  (sort (list (list "full"          "Fully connected layer"   full-layer-options          full)
              (list "convolutional" "2-D convolutional layer" convolutional-layer-options convolutional)
              (list "pooling"       "2-D pooling layer"       pooling-layer-options       pooling))
        (lambda (a b) (string< (car a) (car b)))))

(define (print-command-usage command #!optional error)
  (when error (print error))
  (let* ((def (assoc command commands))
         (desc (cadr def))
         (grammar (caddr def)))
    (format #t "Usage: ~A ~A [OPTIONS]\n\n~A.\n\n" prog-name command desc)
    (print "Required parameters:\n")
    (print (usage (filter required? grammar)))
    (print "Optional:\n")
    (print (usage (filter (lambda (o) (not (required? o))) grammar)))))

(define (print-program-usage #!optional error)
  (when error (begin (print error) (newline)))
  (format #t "Usage: ~A COMMAND [OPTIONS]\n\n~A.\n\n" prog-name prog-desc)
  (print "Commands:\n")
  (let ((max-command-width (apply max (map (compose string-length car) commands))))
    (for-each (lambda (c)
                (let* ((spaces (+ 4 (- max-command-width (string-length (car c)))))
                       (sep (make-string spaces #\ )))
                  (print "  " (car c) sep (cadr c))))
              commands))
  (format #t "\nSee '~A COMMAND' to read more about a specific command.\n" prog-name))

(define (parse-options command args grammar)
  (handle-exceptions
   e
   (begin
     ;; TODO: figure out how to print original error message
     (print-command-usage command
                          (format #f
                                  "Error: ~A - ~A\n"
                                  (get-condition-property e 'exn 'message)
                                  (get-condition-property e 'exn 'arguments)))
     #f)
   (getopt-long args grammar)))

(define (help? arg)
  (member arg (list "-h" "--help")))

(let* ((args (command-line-arguments))
       (command (if (= (length args) 0) #f (car args)))
       (def (assoc command commands)))
  (cond ((not command)                                 ;; No given command
         (print-program-usage))
        ((help? command)                               ;; General usage
         (print-program-usage))
        ((not def)                                     ;; No matching command
         (print-program-usage (format #f "'~A' is not a valid command" command)))
        ((and (> (length args) 1) (help? (cadr args))) ;; Command usage
         (print-command-usage command))
        (else
         (let* ((grammar (caddr def))
                (f (cadddr def))
                (options (parse-options command args grammar)))
           (when options
                 (f (make-options-lookup options)))))))
