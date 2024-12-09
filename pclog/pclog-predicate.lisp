;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; Code from Paradigms of AI Programming
;;; Copyright (c) 1991 Peter Norvig

(in-package "PCLOG")


(defpred fail/0 (trail cont)
  "7.8.1"
  (declare (ignore trail cont))
  nil)


(defpred true/0 (trail cont)
  "7.8.2"
  (declare (ignore trail))
  (funcall cont))


(defpred call/1 (trail ?g cont)
  "7.8.3: Try to prove goal by calling it."
  (deref ?g)
  (cond
    ((or (goal-and-p ?g) (goal-or-p ?g) (goal-if-p ?g))
     ;; FIXME: this use of a temporary predicate name is ugly,
     ;; non-threadsafe and basically evil bad and wrong.
     (funcall (compile-predicate 'call/1-tmp-fun 0 (list ?g)) trail cont))
    (t (apply (make-predicate* (predicate ?g) (length (args ?g)))
              trail
              (append (args ?g) (list cont))))))


(defpred !/0 (trail cont)
  "7.8.4"
  (declare (ignore trail))
  (funcall cont))


;;; 8.2 term unification

(defpred =/2 (trail ?arg1 ?arg2 cont)
  "8.2.1"
  (when (unify! trail ?arg1 ?arg2)
    (funcall cont)))


;;; 8.3 type testing


(macrolet ((define-type-testing-predicate (name docstring fun)
             `(defpred ,name (trail x cont)
                ,docstring
                (declare (ignore trail))
                (when (,fun (deref x))
                  (funcall cont)))))
  (define-type-testing-predicate var/1 "8.3.1" unbound-var-p)
  (define-type-testing-predicate atom/1 "8.3.2" symbolp)
  (define-type-testing-predicate atomic/1 "8.3.5" atomicp)
  #-cons-var (assert (not (consp (?))))
  (define-type-testing-predicate nonvar/1 "8.3.7" nonvarp)
  ;; strictly, this should be (OR INTEGERP FLOATP).
  (define-type-testing-predicate number/1 "8.3.8" numberp))


(defun @</2 (trail ?x ?y cont)
  "8.4.3"
  (declare (ignore trail))
  (when (term-precedes ?x ?y)
    (funcall cont)))


(defun @=</2 (trail ?x ?y cont)
  "8.4.4"
  (declare (ignore trail))
  (when (or (deref-equal ?x ?y)
	    (term-precedes ?x ?y))
    (funcall cont)))


(defun @>/2 (trail ?x ?y cont)
  (declare (ignore trail))
  (when (term-precedes ?y ?x)
    (funcall cont)))


(defun @>=/2 (trail ?x ?y cont)
  (declare (ignore trail))
  (when (or (deref-equal ?y ?x)
	    (term-precedes ?y ?x))
    (funcall cont)))


(defpred ==/2 (trail ?x ?y cont)
  "8.4.1: Are the two arguments EQUAL with no unification,
  but with dereferencing?  If so, succeed."
  (declare (ignore trail))
  (when (deref-equal ?x ?y)
    (funcall cont)))


(defpred \\==/2 (trail ?x ?y cont)
  "8.4.2"
  (declare (ignore trail))
  (unless (deref-equal ?x ?y)
    (funcall cont)))


(defun /==/2 (trail ?x ?y cont)
  "8.4.2"
  (declare (ignore trail))
  (unless (deref-equal ?x ?y)
    (funcall cont)))


;;; 8.5 term creation and decomposition

(defpred functor/3 (trail ?term ?name ?arity cont)
  "8.5.1"
  (cond ((unbound-var-p ?term)
         (assert (not (unbound-var-p ?name)))
         (assert (not (unbound-var-p ?arity)))
         (when (unify! trail ?term (list* (deref ?name)
                                          (loop repeat (deref ?arity) collect (?))))
           (funcall cont)))
        (t (if (atomicp (deref ?term))
               (when (and (unify! trail ?arity 0)
                          (unify! trail ?name ?term))
                 (funcall cont))
               (when (and (unify! trail ?arity (length (cdr ?term)))
                          (unify! trail ?name (or (and(car ?term)))))
                 (funcall cont))))))


(defpred arg/3 (trail ?n ?term ?arg cont)
  "8.5.2"
  (when (unify! trail (nth (deref ?n) (deref ?term)) ?arg)
    (funcall cont)))


(defpred copy-term/2 (trail ?term1 ?term2 cont)
  "8.5.4"
  (when (unify! trail (make-renamed-copy (deref ?term1)) (deref ?term2))
    (funcall cont)))


;;; 8.6 arithmetic evaluation

#|(defpred is/2 (trail var exp cont)
  "8.6.1"
  ;; Example: (is ?x (+ 3 (* ?y (+ ?z 4))))
  ;; Or even: (is (?x ?y ?x) (cons (first ?z) ?l))
  (when ;; (and (not (find-if-anywhere #'unbound-var-p exp))
      ;;      (unify! var (eval (deref-exp exp))))
      (or (and (not (find-if-anywhere #'unbound-var-p exp))
               (unify! trail var (eval (deref-exp exp))))
          (let ((var exp)
                (exp var))
            (and (not (find-if-anywhere #'unbound-var-p exp))
                 (unify! trail var (eval (deref-exp exp))))))
    (funcall cont)))|#

(defpred is/2 (trail var exp cont)
  (when (unify! trail var (deref exp))
    (funcall cont)))


(defpred is/1 (trail exp cont)
  (declare (ignore exp trail))
  (funcall cont))


(define-prolog-compiler-macro are (trail goal body cont bindings)
  (let* ((args (args goal))
         (vars (first args))
         (vals (mapcar (lambda (v) (gensym (string v)))
                       (first args)))
         (lisp-exp (second args)))
    (compile-if `(multiple-value-bind (,@vals)
                                      ,lisp-exp
                   (and ,@(mapcar (lambda (var val)
                                    `(unify! ,trail ,var ,val))
                                  vars
                                  vals)))
                (compile-body* trail body cont (bind-new-variables bindings goal)))))


;;; 8.7 arithmetic comparison

(macrolet ((define-arithmetic-comparison-predicate (name op)
	     `(defun ,name (trail ?e1 ?e2 cont)
		"8.7.3"
                (declare (ignore trail))
		(when (and (not (find-if-anywhere #'unbound-var-p ?e1))
			   (not (find-if-anywhere #'unbound-var-p ?e1)))
		  ;; FIXME: CL specifies comparison on (float,integer)
		  ;; by coercing the float to a rational, and
		  ;; comparing.  Prolog doesn't have rationals, and
		  ;; the coercion goes the other way.
		  (when (,op (eval (deref-exp ?e1)) (eval (deref-exp ?e2)))
		    (funcall cont))))))
  (define-arithmetic-comparison-predicate |=:=|/2 =)
  (define-arithmetic-comparison-predicate =\\=/2 /=)
  (define-arithmetic-comparison-predicate </2 <)
  (define-arithmetic-comparison-predicate =</2 <=)
  (define-arithmetic-comparison-predicate >/2 >)
  (define-arithmetic-comparison-predicate >=/2 >=))


;;; FIXME: 8.8 clause retrieval and information

(defpred clause/2 (trail ?head ?body cont)
  "8.8.1"
  (let ((clauses (get-clauses (predicate (deref ?head)))))
    (let ((old-trail (trail-ndx trail)))
      (dolist (clause clauses)
	(when (unify! trail `(,?head . ,?body) clause)
	  (funcall cont))
        (undo-bindings! trail old-trail)))))


;;; 8.9 clause creation and destruction

(defpred asserta/1 (trail ?clause cont)
  "8.9.1"
  (let* ((old-trail (trail-ndx trail))
	 (?head (?))
	 (?body (?)))
    (unless (unify! trail (deref ?clause) `(<- ,?head . ,?body))
      (undo-bindings! trail old-trail)
      (unify! trail `(,?head . ,?body) `(,?clause (true))))
    (add-clause (cons (deref ?head) (deref ?body)) :asserta t)
    (funcall cont)))


(defpred assertz/1 (trail ?clause cont)
  "8.9.2"
  (let* ((old-trail (trail-ndx trail))
	 (?head (?))
	 (?body (?)))
    (unless (unify! trail (deref ?clause) `(<- ,?head . ,?body))
      (undo-bindings! trail old-trail)
      (unify! trail `(,?head . ,?body) `(,?clause (true))))
    (add-clause (cons (deref ?head) (deref ?body)))
    (funcall cont)))


(defpred retract/1 (trail ?clause cont)
  "8.9.3"
  (let* ((old-trail (trail-ndx trail))
	 (?head (?))
	 (?body (?)))
    (unless (unify! trail (deref ?clause) `(<- ,?head . ,?body))
      (undo-bindings! trail old-trail)
      (unify! trail `(,?head . ,?body) `(,?clause (true))))
    (retract-clause (cons (deref ?head) (deref ?body)))
    (funcall cont)))


(defpred abolish/1 (trail ?pi cont)
  "8.9.4"
  ;; FIXME: implement this
  (declare (ignore trail cont ?pi))
  )


;;; 8.10 all solutions

;;; FIXME: I think this is right for FINDALL/3.  BAGOF/3 and SETOF/3
;;; have extra complicated stuff to do with witnesses.

(defpred bagof/3 (trail exp goal result cont)
  "8.10.2: Find all solutions to GOAL, and for each solution,
  collect the value of EXP into the list RESULT."
  ;; Ex: Assume (p 1) (p 2) (p 3).  Then:
  ;;     (bagof ?x (p ?x) ?l) == ?l = (1 2 3)
  (let ((answers nil))
    (call/1 trail
            goal (lambda ()
		   ;; Bug fix by mdf0%shemesh@gte.com (Mark Feblowitz)
		   ;; on 25 Jan 1996; was deref-COPY
                   (push (deref-EXP exp) answers))) 
    (if (and (not (null answers))
             (unify! trail result (nreverse answers)))
        (funcall cont))))


(defpred setof/3 (trail exp goal result cont)
  "8.10.3: Find all unique solutions to GOAL, and for each solution,
  collect the value of EXP into the list RESULT."
  ;; Ex: Assume (p 1) (p 2) (p 3).  Then:
  ;;     (setof ?x (p ?x) ?l) ==> ?l = (1 2 3)
  (let ((answers nil))
    (call/1 trail
            goal
            (lambda ()
              (push (deref-exp exp) answers)))
    (if (and (not (null answers))
             (unify! trail result (delete-duplicates answers
                                                     :test #'deref-equal)))
        (funcall cont))))


;;; FIXME: findall/3

;;; 8.11 stream selection and control

(defmacro with-stream ((s-var s) &body body)
  (let ((n (gensym)))
    `(let ((,n ,s))
       (let ((,s-var (etypecase ,n
		       ;; Lisp SYMBOL -> Prolog atom -> alias
		       (cl:symbol (get ,n 'stream-alias))
		       (stream ,n))))
	 ,@body))))


;;; FIXME: 8.11.11 stream-property/2.  Need a registry of all open
;;; streams.

(defpred nl/0 (trail cont)
  "8.12.5"
  (declare (ignore trail))
  (terpri *standard-output*)
  (funcall cont))


(defpred nl/1 (trail ?stream-or-alias cont)
  "8.12.6"
  (declare (ignore trail))
  (with-stream (s (deref ?stream-or-alias))
    (terpri s)
    (funcall cont)))


;;; FIXME: 8.14 Term input/output

;;; these would probably be OK for lispy prolog; they're probably not
;;; for ISO prolog.
(defpred read/1 (trail exp cont)
  (and (unify! trail exp (read))
       (funcall cont)))


(defpred write/1 (trail exp cont)
  (declare (ignore trail))         
  (write (deref-exp exp) :pretty t)
  (funcall cont))


;;; 8.17 implementation-defined hooks

(define-prolog-compiler-macro lisp (trail goal body cont bindings)
  "lisp/1 and lisp/2"
  (let ((args (args goal)))
    (case (length args)
      (1                                ; lisp/1
         (let* ((lisp-exp (first args))
                (lisp-args (variables-in lisp-exp)))
           `(progn
              (apply (lambda ,lisp-args ,(insert-deref lisp-exp))
                     ,(compile-arg lisp-args bindings))
              ,(compile-body* trail body cont bindings))))
      (2                                ; lisp/2
         (let* ((var (first args))
                (lisp-exp (second args))
                (lisp-args (variables-in lisp-exp)))
           (compile-if `(unify! ,trail ,(compile-arg var bindings)
                                (apply (lambda ,lisp-args ,(insert-deref lisp-exp))
                                       ,(compile-arg lisp-args bindings)))
                       (compile-body* trail body cont (bind-new-variables bindings goal)))))
      (t :pass))))


(define-prolog-compiler-macro lisp* (trail goal body cont bindings)
  "lisp*/1 and lisp*/2"
  (let ((args (args goal)))
    (case (length args)
      (1                                ; lisp*/1
       (let* ((lisp-exp (first args))
              (lisp-args (variables-in lisp-exp)))
         `(progn
            (catch 'deref-fail
              (apply (lambda ,lisp-args ,(insert-deref-fail lisp-exp))
                     ,(compile-arg lisp-args bindings)))
            ,(compile-body* trail body cont bindings))))
      (2                                ; lisp*/2
       (let* ((var (first args))
              (lisp-exp (second args))
              (lisp-args (variables-in lisp-exp)))
         (compile-if `(catch 'deref-fail
                        (unify! ,trail ,(compile-arg var bindings)
                                (apply (lambda ,lisp-args ,(insert-deref-fail lisp-exp))
                                       ,(compile-arg lisp-args bindings))))
                     (compile-body* trail body cont (bind-new-variables bindings goal)))))
      (t :pass))))


(progn
  #||
  (<-- (member ?item (?item . ?rest)))
  (<-  (member ?item (?x . ?rest)) (member ?item ?rest))
  ||#

  ;;#++
  (tail-recursive-defun member/2 (trail ?arg1 ?arg2 cont)
    (let ((old-trail (trail-ndx trail)))
      (let ((?rest (?)))
        (if (unify! trail ?arg2 (cons ?arg1 ?rest))
            (funcall cont)))
      (undo-bindings! trail old-trail)
      (let ((?rest (?)))
        (if (unify! trail ?arg2 (cons (?) ?rest))
            (member/2 trail ?arg1 ?rest cont))))))


(progn
  #||
  (<-- (length () 0))
  (<-  (length (?x . ?y) ?n)
       (length ?y ?n1)
       (is ?n (1+ ?n1)))
  ||#
  (defun length/2 (trail ?arg1 ?arg2 cont)
    (let ((old-trail (trail-ndx trail)))
      (typecase ?arg2
        ((integer 0 *)
         (unify! trail ?arg1 (make-list ?arg2 :initial-element (?)))
         (return-from length/2 (funcall cont)))
        (var (if (unify! trail ?arg1 'nil)
                 (if (unify! trail ?arg2 '0)
                     (funcall cont)))))
      (undo-bindings! trail old-trail)
      (let ((?n1 (?)) (?y (?)))
        (if (unify! trail ?arg1 (cons (?) ?y))
            (length/2 trail ?y ?n1 (lambda () (is/2 trail ?arg2 (list '1+ ?n1) cont))))))))


(define-prolog-compiler-macro lispp* (trail goal body cont bindings)
  (let ((args (args goal)))
    (let* ((lisp-exp (first args))
           (lisp-args (variables-in lisp-exp)))
      `(and (apply (lambda ,lisp-args ,(insert-deref lisp-exp))
                   ,(compile-arg lisp-args bindings))
            ,(compile-body* trail body cont bindings)))))


(define-prolog-compiler-macro lispp (trail goal body cont bindings)
  (let ((args (args goal)))
    (let* ((lisp-exp (first args))
           (lisp-args (variables-in lisp-exp)))
      `(and (catch 'deref-fail
              (apply (lambda ,lisp-args ,(insert-deref lisp-exp))
                     ,(compile-arg lisp-args bindings)))
            ,(compile-body* trail body cont bindings)))))


(defun generator/2 (trail ?item ?generator cont)
  (declare (ignore))
  (deref ?generator)
  (loop (multiple-value-bind (item next?)
                             (funcall ?generator)
          (declare (ignore next?))
          (cond ((and item (unify! trail ?item item))
                 (funcall cont)
                 (setf (var-binding ?item) unbound))
                (T (return nil))))))


(defun generator*/2 (trail ?item ?generator cont)
  (declare (ignore))
  (deref ?generator)
  (loop (multiple-value-bind (item next?)
                             (funcall ?generator)
          (cond ((and next? (unify! trail ?item item))
                 (funcall cont)
                 (setf (var-binding ?item) unbound))
                (T (return nil))))))


(define-prolog-compiler-macro generating (trail goal body cont bindings)
  (let ((generator (gensym "?generator")))
    `(let ((,generator (?)))
       ,(funcall (prolog-compiler-macro 'lisp)
                 trail
                 `(lisp ,generator ,(second (args goal)))
                 `((generator ,(first (args goal))
                              ,generator)
                   ,@body)
                 cont
                 bindings))))


(define-prolog-compiler-macro generating* (trail goal body cont bindings)
  (let ((generator (gensym "?generator")))
    `(let ((,generator (?)))
       ,(funcall (prolog-compiler-macro 'lisp)
                 trail
                 `(lisp ,generator ,(second (args goal)))
                 `((generator* ,(first (args goal))
                               ,generator)
                   ,@body)
                 cont
                 bindings))))


(progn
  #|
   (<-- (slot= ?instance ?slot-name ?slot-value)
        (lisp ?slot-value (slot-value ?instance ?slot-name)))
   |#
  (defun slot=/3 (trail ?arg1 ?arg2 ?arg3 cont)
    (if (unify! trail
                ?arg3
                (apply (lambda (?instance ?slot-name)
                         (slot-value (deref-exp ?instance) (deref-exp ?slot-name)))
                       (list ?arg1 ?arg2)))
        (funcall cont))))


(progn
  #|
  (<-- (slot=* ?instance ?slot-name ?slot-value)
       (lispp* (and (slot-exists-p ?instance ?slot-name)
                    (slot-boundp ?instance ?slot-name)))
       (lisp ?slot-value (slot-value ?instance ?slot-name)))
   |#
  (defun slot=*/3 (trail ?arg1 ?arg2 ?arg3 cont)
    (and (apply (lambda (?instance ?slot-name)
                  (and (slot-exists-p (deref-exp ?instance) (deref-exp ?slot-name))
                       (slot-boundp (deref-exp ?instance) (deref-exp ?slot-name))))
                (list ?arg1 ?arg2))
         (if (unify! trail
                     ?arg3
                     (apply (lambda (?instance ?slot-name)
                              (slot-value (deref-exp ?instance) (deref-exp ?slot-name)))
                            (list ?arg1 ?arg2)))
             (funcall cont)))))


(defun slot-value/3 (trail ?instance ?slot-name ?slot-value cont)
  (catch 'deref-fail
    (let ((instance (deref-exp ?instance))
          (slot-name (deref-exp ?slot-name)))
      (if (slot-boundp instance slot-name)
          (and (unify! trail
                       ?slot-value
                       (slot-value instance slot-name))
               (funcall cont))
          (progn
            (setf (slot-value instance slot-name) (deref-exp-fail ?slot-value))
            (and (unify! trail
                         ?slot-value
                         (slot-value instance slot-name))
                 (funcall (lambda ()
                            (funcall cont)
                            (slot-makunbound instance slot-name)))))))))


(defun slot-value!/3 (trail ?instance ?slot-name ?slot-value cont)
  (catch 'deref-fail
    (let ((instance (deref-exp ?instance))
          (slot-name (deref-exp ?slot-name)))
      (if (slot-boundp instance slot-name)
          (and (unify! trail
                       ?slot-value
                       (slot-value instance slot-name))
               (funcall cont))
          (progn
            (setf (slot-value instance slot-name) (deref-exp-fail ?slot-value))
            (and (unify! trail
                         ?slot-value
                         (slot-value instance slot-name))
                 (funcall cont)))))))


(define-prolog-compiler-macro let (trail goal body cont bindings)
  (destructuring-bind (op var val &optional decl)
                      goal
    (declare (ignore op))
    `(let ((,var ,val))
       ,decl
       ,(compile-body* trail body cont bindings))))


(define-prolog-compiler-macro let* (trail goal body cont bindings)
  (destructuring-bind (op var val &optional decl)
                      goal
    (declare (ignore op))
    `(let* ((,var ,val))
       (declare (special ,var))
       ,decl
       ,(compile-body* trail body cont bindings))))


(define-prolog-compiler-macro unwind-protect (trail goal body cont bindings)
  (destructuring-bind (op &body cleanup-forms)
                      goal
    (declare (ignore op))
    `(unwind-protect (catch 'deref-fail
                       ,(compile-body* trail body cont bindings))
       ,@cleanup-forms)))


(defun leash-1 (functor arity)
  (eval `(trace ',functor ',arity)))


(defun unleash-1 (functor arity)
  (eval `(untrace ',functor ',arity)))


(defmacro leash (&rest functor/arity)
  `(trace ,@(loop :for (functor arity) :on functor/arity :by #'cddr
                  :collect (make-predicate* functor arity))))


(defmacro unleash (&rest functor/arity)
  `(untrace ,@(loop :for (functor arity) :on functor/arity :by #'cddr
                    :collect (make-predicate* functor arity))))


(prolog-compile-symbols)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#||
http://www.lispworks.com/documentation/lw71/KW-U/html/kwprolog-u-104.htm
||#


(defun cut/0 (trail cont)
  "7.8.4"
  (declare (ignore trail))
  (funcall cont))


(defun make-return-exp (xpr)
  (typecase xpr
    (atom xpr)
    (cons `(list
            ,@(mapcar (lambda (x)
                        (if (variable-p x)
                            `(deref-exp ,x)
                            `(quote ,x)))
                      xpr)))))


;; (any '(?x is in (1 2 3)) '(member ?x (1 2 3)))
;; → (1 is in (1 2 3))
(defun any (pattern-to-instantiate goal-to-prove)
  (when (null pattern-to-instantiate)
    (error "must specify patterns."))
  ;; FIXME
  (eval
   `(prolog ,goal-to-prove
            (lisp (return-from prolog 
                    ,(make-return-exp pattern-to-instantiate))))))


(define-compiler-macro any (&whole whole pattern-to-instantiate goal-to-prove)
  (if (or ;;(constantp pattern-to-instantiate)
          (and (consp pattern-to-instantiate)
               (eq 'quote (car pattern-to-instantiate))
               (consp goal-to-prove)
               (eq 'quote (car goal-to-prove))))
      `(prolog
        ,(eval goal-to-prove)
        (lisp (return-from prolog 
                ,(make-return-exp (eval pattern-to-instantiate)))))
      whole))


#|(defmacro %prolog (cont-fn &rest goals)
  "Run Prolog in the surrounding Lisp environment
which is accessed from lisp functor.
"
  (let ((*predicate* 'prolog-clauses))
    `(block prolog
       (flet ((,*predicate* (trail cont)
                (declare (ignorable trail cont))
                ,(let* ((vars (variables-in goals)))
                   `(let-de (,@(mapcar (lambda (v) `(,v (?))) vars))
                      ,(compile-body* 'trail goals 'cont nil)))))
         (declare (dynamic-extent #',*predicate*))
         (let ((trail (fast *trail*)))
           (if trail
               (,*predicate* trail ,cont-fn)
               (let ((trail (allocate-trail *default-trail-size*)))
                 (let ((*trail* trail))
                   (,*predicate* trail ,cont-fn)))))))))|#


;; (findall '(?x is in (1 2 3)) '(member ?x (1 2 3)))
;; → ((3 is in (1 2 3)) (2 is in (1 2 3)) (1 is in (1 2 3)))
(defun findall (pattern-to-instantiate goal-to-prove)
  (when (null pattern-to-instantiate)
    (error "must specify patterns."))
  ;; FIXME
  (eval
   (let ((result (gensym "result")))
     `(let (,result)
        (prolog
         ,goal-to-prove
         (lisp (push ,(make-return-exp pattern-to-instantiate)
                     ,result)))
        (nreverse ,result)))))


;; (findallset '?y '(or (= ?y 5) (= ?y 5)))
;; → (5)
(defun findallset (pattern-to-instantiate goal-to-prove)
  (when (null pattern-to-instantiate)
    (error "must specify patterns."))
  ;; FIXME
  (eval
   (let ((result (gensym "result"))
         (ans (gensym "ans")))
     `(let ((,result (make-hash-table :test #'equal)))
        (prolog
         ,goal-to-prove
         (lisp (setf (gethash ,(make-return-exp pattern-to-instantiate) ,result) 
                     T)))
        (let ((,ans (list)))
          (maphash (lambda (k v)
                     (declare (ignore v))
                     (push k ,ans))
                   ,result)
          ,ans)))))


(defun logic (goal &key return-type all bag-exp)
  (ecase return-type
    ((:display nil)
     (let ((vars (variables-in goal)))
       (format *debug-io*
               "~{~{~S = ~S~}~%~}"
               (mapcan (lambda (v)
                         (let ((res (funcall (if all #'findall #'any) v goal)))
                           (if all
                               (mapcar (lambda (x) (list v x)) res)
                               (list (list v res)))))
                       vars))))
    (:fill
     (if all
         (funcall (ecase all
                    (:values #'values-list)
                    (:list #'values))
                  (findall goal goal))
         (any goal goal)))
    (:bag 
     (if all
         (funcall (ecase all
                    (:values #'values-list)
                    (:list #'values))
                  (findall bag-exp goal))
         (any bag-exp goal)))
    (:alist
     (let ((vars (variables-in goal)))
       (if all
           (funcall (ecase all
                      (:values #'values-list)
                      (:list #'values))
                    (mapcan (lambda (v)
                              (mapcar (lambda (x)
                                        (cons v x))
                                      (findall v goal)))
                            vars))
           (mapcan (lambda (v)
                     (mapcar (lambda (x)
                               (cons v x))
                             (any (list v) goal)))
                   vars))))))


#||
(defrel color
  ((color red))
  ((color blue))
  ((color green)))

(logic '(color ?x)
       :return-type :display
       :all :values)

(logic '(color ?x)
       :return-type :bag
       :bag-exp '(?x is a color)
       :all :list)

(logic '(color ?x)
       :return-type :alist
       :all nil)

((?x green)) 
 
(logic '(color ?x)
       :return-type :fill
       :all :values)
(findall '(color ?x) '(color ?x))

((color green) (color blue) (color red)) 



||#


(defun make-return-exp* (xpr)
  (typecase xpr
    (atom xpr)
    (cons `(list
            ,@(mapcar (lambda (x)
                        (if (variable-p x)
                            `(deref-exp ',x)
                            x))
                      xpr)))))


(defmacro deflogfun (name (&rest args) sample-expr return-expr)
  `(defun ,name (,@args &key all)
     (logic (cons ',(car sample-expr)
                  ,(make-return-exp* (cdr sample-expr)))
            :return-type :bag
            :bag-exp ',return-expr
            :all all)))


#||
(deflogfun break-up (y) (append ?a ?b y) (?a ?b))

(break-up '(foo bar baz) :all :list)
→ ((nil (foo bar baz)) ((foo) (bar baz)) ((foo bar) (baz)) ((foo bar baz) nil))
||#

(defun ?.p (x)
  (typecase x
    (cl:symbol (eql 0 (search "?." (string x))))
    (T nil)))


(defun escape-?.var (exp)
  (cond ((null exp) nil)
        ((atom exp)
         (if (?.p exp)
             (intern (subseq (string exp) 2))
             `',exp))
        (T (list 'cons
                 (escape-?.var (car exp))
                 (escape-?.var (cdr exp))))))


(defmacro with-prolog (&body body)
  `(prolog
    ,(eval (escape-?.var `(and ,@body)))
    (lisp (return-from prolog T))))


(defmacro with-prolog/free-vars ((&rest free-vars) &body body)
  `(prolog/free-vars (,@free-vars)
    ,(eval (escape-?.var `(and ,@body)))
    (lisp (return-from prolog T))))

#||

(defrel reverse
  ((reverse () ()))
  ((reverse (?x . ?xs) ?ans)
   (reverse ?xs ?ans1)
   (append ?ans1 (?x) ?ans)))
||#

(defmacro defrel (name &body clauses)
  (destructuring-bind (first . rest)
                      clauses
    (let ((first (replace-?-vars first)))
      `(progn
         (retract-same-arity-clause ',first)
         (add-clause ',first)
         ,@(mapcar (lambda (c)
                     `(add-clause ',(make-anonymous c)))
                   rest)
         (prolog-compile ',name)))))

(defrel append
  ((append () ?x ?x))
  ((append (?x . ?xs) ?y (?x . ?zs))
   (append ?xs ?y ?zs)))


(defrel =..
  ((=.. (?op . ?args) ?list)
   (= ?list (\. ?op ?args)))
  ((=.. ?term ?list)
   ((and (vectorp ?term) (not (stringp ?term))) T)
   ((coerce ?term 'list) ?term/list)
   (= ?term/list ?list)))


(defun /\\ (x y)
  (min x y))


(defun \\/ (x y)
  (max x y))


(defun >> (x y)
  (ash x (- y)))


(defun << (x y)
  (ash x y))


(defrel not
  ((not ?goal)
   (call ?goal)
   !
   (fail))
  ((not ?goal)))

(defrel once
  ((once ?x)
   (call ?x)
   !))


;;; *EOF*
