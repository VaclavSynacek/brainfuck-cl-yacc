
(ql:quickload :yacc)

(use-package '#:yacc)

;; Graham's alambda
(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

(defun lexer (list)
  (alambda ()
    (let ((value (pop list)))
      (case value
        (#\+ (values 'add 1))
        (#\- (values 'add -1))
        (#\> (values 'pointer 1))
        (#\< (values 'pointer -1))
        (#\[ (values 'loop-start 'loop-start))
        (#\] (values 'loop-end 'loop-end))
        (#\. (values 'print 'print))
        (#\, (values 'read 'read))
        (t (if (null value)
             (values nil nil)           ; for end of bf program
             (self)))))))               ; in case of all other chars, recurse
            ;(values 'noop 'noop))))))) ; alternatively parse also comments


(define-parser *expression-parser*
  (:start-symbol program)
  (:terminals (add pointer print read loop-start loop-end noop))
  (program
    (statement #'list)
    (statement program #'cons))
  (statement
    (noop     (lambda (a) (list a))) 
    (adds     (lambda (a) `(add  ,(apply #'+ a))))
    (pointers (lambda (a) `(move ,(apply #'+ a))))
    (print    (lambda (a) (list a)))
    (read     (lambda (a) (list a)))
    (loop     (lambda (a) `(while ,@(second a)))))
  (adds
    (add #'list)
    (add adds #'cons))
  (pointers
    (pointer #'list)
    (pointer pointers #'cons))
  (loop
    (loop-start program loop-end)))
    
(defun parse-bf (source)
  (parse-with-lexer
    (lexer (coerce source 'list))
    *expression-parser*))

#|
 
(parse-bf
  "
  +++++++++[>++++++++>+++++++++++>+++++<<<-]>.>++.+++++++..+++.>-.------------.<
  ++++++++.--------.+++.------.--------.>+.
  ")

|#

(defun bf->lisp (bf)
  (when bf
    (let* ((bf-form   (pop bf))
           (lisp-form
             (case (car bf-form)
               ((move) `(incf *BF*pointer ,(cadr bf-form)))
               ((add) `(incf (aref *BF*memory *BF*pointer) ,(cadr bf-form)))
               ((print) `(write-char (code-char (aref *BF*memory *BF*pointer))))
               ((read) `(setf (aref *BF*memory *BF*pointer) (char-code (read-char))))
               ((while) `(loop
                           (progn
                             (when (zerop (aref *BF*memory *BF*pointer))
                               (return))
                             ,@(bf->lisp (cdr bf-form))))))))
      (if lisp-form
        (cons lisp-form (bf->lisp bf))
        (bf->lisp bf)))))


(defun bf-environment (forms)
  `(let ((*BF*pointer 0)
         (*BF*memory   (make-array 30000
                                   :element-type '(integer 0 255)
                                   :initial-element 0)))
        (declare (fixnum *BF*pointer))
        ,@forms))

#|
 
(eval
  (bf-environment
    (bf->lisp
      (parse-bf
       "
  +++++++++[>++++++++>+++++++++++>+++++<<<-]>.>++.+++++++..+++.>-.------------.<
  ++++++++.--------.+++.------.--------.>+.
  "))))

|#

(defun eval-bf (bf-string)
  (eval (bf-environment (bf->lisp (parse-bf bf-string)))))

#|

(time (eval-bf "++++++++++++++++++++++++++++++++++++++++++++++++++++++..."))

(time (eval-bf "+++++++++++[>+++++<-]>-..."))

|#

(defmacro defuck (name bf-string)
  `(defun ,name ()
      (declare (optimize (speed 3) (safety 0) (space 3) (debug 0)))
     ,(bf-environment (bf->lisp (parse-bf bf-string)))))

#|

(defuck devil "++++++++++++++++++++++++++++++++++++++++++++++++++++++...")

(defuck devils-loop "+++++++++++[>+++++<-]>-...")

(disassemble 'devil)

(disassemble 'devils-loop)

;; example from https://gist.github.com/m2ym/774212

(defuck quine "
  -->+++>+>+>+>+++++>++>++>->+++>++>+>>>>>>>>>>>>>>>>->++++>>>>->+++>+++>+++>+++>+
  ++>+++>+>+>>>->->>++++>+>>>>->>++++>+>+>>->->++>++>++>++++>+>++>->++>++++>+>+>++
  >++>->->++>++>++++>+>+>>>>>->>->>++++>++>++>++++>>>>>->>>>>+++>->++++>->->->+++>
  >>+>+>+++>+>++++>>+++>->>>>>->>>++++>++>++>+>+++>->++++>>->->+++>+>+++>+>++++>>>
  +++>->++++>>->->++>++++>++>++++>>++[-[->>+[>]++[<]<]>>+[>]<--[++>++++>]+[<]<<++]
  >>>[>]++++>++++[--[+>+>++++<<[-->>--<<[->-<[--->>+<<[+>+++<[+>>++<<]]]]]]>+++[>+
  ++++++++++++++<-]>--.<<<]")

(quine)

|#
