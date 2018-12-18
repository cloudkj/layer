(declare (uses core options util conv dense pool))

(use getopt-long)

(define command-options
  (list (cons "dense" weighted-layer-options)
        (cons "conv" conv-layer-options)
        (cons "pool" pool-layer-options)))

(define command-functions
  (list (cons "dense" dense)
        (cons "conv" conv)
        (cons "pool" pool)))

(define (usage #!optional error)
  (print "TODO: usage")
  (when error (print error)))

(let* ((args (command-line-arguments))
       (command (if (= (length args) 0) #f (car args)))
       (grammar (assoc command command-options)))
  (cond ((not command) (usage))
        ((not grammar) (usage "Invalid command"))
        (else
         (let* ((options (getopt-long args (cdr grammar)))
                (lookup (make-options-lookup options))
                (f (cdr (assoc command command-functions))))
           (f lookup)))))
