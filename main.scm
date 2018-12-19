(declare (uses core options util conv dense pool))

(use getopt-long)

(define program "main")

(define commands
  (list (list "dense" weighted-layer-options dense)
        (list "conv"  conv-layer-options     conv)
        (list "pool"  pool-layer-options     pool)))

(define (print-command-usage command #!optional error)
  (when error (print error))
  (format #t "Usage: ~A ~A [options]\n\n" program command)
  (print "Options:")
  (print (usage (cadr (assoc command commands)))))

(define (print-usage #!optional error)
  (when error (begin (print error) (newline)))
  (format #t "Usage: ~A command [options]\n\n" program)
  (print "Commands:"))

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
         (print-usage))
        ((help? command)                               ;; General usage
         (print-usage))
        ((not def)                                     ;; No matching command
         (print-usage (format #f "'~A' is not a valid command" command)))
        ((and (> (length args) 1) (help? (cadr args))) ;; Command usage
         (print-command-usage command))
        (else
         (let* ((grammar (cadr def))
                (f (caddr def))
                (options (parse-options command args grammar)))
           (when options
                 (f (make-options-lookup options)))))))
