(cl:in-package cl-user)


(declaim (optimize (speed 3) (safety 0) (compilation-speed 0)
                   (debug 0)))


;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; Code from Paradigms of AI Programming
;;; Copyright (c) 1991 Peter Norvig

;;; File auxfns.lisp: Auxiliary functions used by all other programs
;;; Load this file before running any other programs.

(in-package "PCLOG")


(defmacro defun-inline (name (&rest args) &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name (,@args) ,@body)))


(defmacro nlet (name bindspec &body body)
  (let ((gs (loop :for nil :in bindspec :collect (gensym)))
        (gname (gensym "name"))
        (gblock (gensym "block")))
    `(macrolet ((,name ,gs
                  `(progn
                     (psetq
                      ,@(apply #'nconc
                               (mapcar #'list ',(mapcar #'car bindspec)
                                       (list ,@gs))))
                     (go ,',gname))))
       (block ,gblock
         (let ,bindspec
           (tagbody
            ,gname (return-from
                       ,gblock (progn ,@body))))))))


(defmacro tail-recursive-defun (name (&rest args) &body body)
  `(defun ,name (,@args)
     (nlet ,name (,@(mapcar (lambda (v) `(,v ,v)) args))
       ,@body)))


(defmacro fast (&body body)
  `(locally
       #+(or sbcl allegro lispworks) (declare (optimize (speed 3) (safety 0)))
       ,@body))


(defun ignorer (&rest args)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ignore args))
  nil)


;;;; Macros (formerly in auxmacs.lisp: that file no longer needed)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun find-anywhere (item tree)
  "Does item occur anywhere in tree?"
  (if (atom tree)
      (if (eql item tree) tree)
      (or (find-anywhere item (first tree))
          (find-anywhere item (rest tree)))))

(defun starts-with (list x)
  "Is x a list whose first element is x?"
  (and (consp list) (eql (first list) x)))
)


(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence 
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))


;;; NOTE: In ANSI Common Lisp, the effects of adding a definition (or most
;;; anything else) to a symbol in the common-lisp package is undefined.
;;; Therefore, it would be best to rename the function SYMBOL to something 
;;; else.  This has not been done (for compatibility with the book).  

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun symbolify (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (with-standard-io-syntax
    (intern (format nil "~:@(~{~A~}~)" args) "PCLOG")))

(defun new-symbol (&rest args)
  "Concatenate symbols or strings to form an uninterned symbol"
  (with-standard-io-syntax
    (make-symbol (format nil "~{~A~}" args)))))


(unless (boundp 'null-var)
  (defconstant null-var (?)))


;;;; PATTERN MATCHING FACILITY

(defconstant fail nil)


(defconstant no-bindings (if (boundp 'no-bindings)
			     (symbol-value 'no-bindings)
			     '((t . t))))


(defun match-variable (var input bindings)
  "Does VAR match input?  Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))


(defun make-binding (var val) (cons var val))


(defun binding-var (binding)
  "Get the variable part of a single binding."
  (car binding))


(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))


(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))


(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))


(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy no-bindings
        (if (eq bindings no-bindings)
            nil
            bindings)))


(eval-when (:compile-toplevel :load-toplevel :execute)
(defun variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

(defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))
  
(defun length=1 (x) 
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x)))))


(defun find-if-anywhere (predicate tree)
  "Does predicate apply to any atom in the tree?"
  (if (atom tree)
      (funcall predicate tree)
      (or (find-if-anywhere predicate (first tree))
          (find-if-anywhere predicate (rest tree)))))


;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File unify.lisp: Unification functions


(defun unify (x y &optional (bindings no-bindings))
  "See if x and y match with given bindings."
  (cond ((eq bindings fail) fail)
        ((equal x y) bindings)
        ((variable-p x) (unify-variable x y bindings))
        ((variable-p y) (unify-variable y x bindings))
        ((and (consp x) (consp y))
         (unify (rest x) (rest y) 
                (unify (first x) (first y) bindings)))
        (t fail)))


(defun unify-variable (var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  (cond ((get-binding var bindings)
         (unify (lookup var bindings) x bindings))
        ((and (variable-p x) (get-binding x bindings))
         (unify var (lookup x bindings) bindings))
        (t (extend-bindings var x bindings))))


;;; ==============================

(defun subst-bindings (bindings x)
  "Substitute the value of variables in bindings into x,
  taking recursively bound variables into account."
  (cond ((eq bindings fail) fail)
        ((eq bindings no-bindings) x)
        ((and (variable-p x) (get-binding x bindings))
         (subst-bindings bindings (lookup x bindings)))
        ((atom x) x)
        (t (reuse-cons (subst-bindings bindings (car x))
                       (subst-bindings bindings (cdr x))
                       x))))


;;; ==============================

(defun unifier (x y)
 "Return something that unifies with both x and y (or fail)."
 (subst-bindings (unify x y) x))


;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File prolog.lisp: prolog from (11.3), with interactive backtracking.

;;;; does not include destructive unification (11.6); see prologc.lisp

;; clauses are represented as (head . body) cons cells
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun clause-head (clause) (first clause))
(defun clause-body (clause) (rest clause))
;; clauses are stored on the predicate's plist
(defun get-clauses (pred) (get pred 'clauses))
(defun predicate (relation)
  (if (atom relation)
      relation
      (first relation)))
(defun args (x)
  "The arguments of a relation"
  (if (atom x)
      nil
      (rest x))))


(defvar *db-predicates* nil
  "a list of all predicates stored in the database.")


(defun clear-db ()
  "remove all clauses (for all predicates) from the data base."
  (mapc #'clear-predicate *db-predicates*))


(defun clear-predicate (predicate)
  "remove the clauses for a single predicate."
  (setf (get predicate 'clauses) nil))


(defun rename-variables (x)
  "replace all variables in x with new ones."
  (sublis (mapcar (lambda (var) (cons var (gensym (string var))))
                  (variables-in x))
          x))


(defun unique-find-anywhere-if (predicate tree
                                &optional found-so-far)
  "return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (adjoin tree found-so-far)
          found-so-far)
      (unique-find-anywhere-if
        predicate
        (first tree)
        (unique-find-anywhere-if predicate (rest tree)
                                 found-so-far))))


(defun find-anywhere-if (predicate tree)
  "does predicate apply to any atom in the tree?"  
  (if (atom tree)
      (funcall predicate tree)
      (or (find-anywhere-if predicate (first tree))
          (find-anywhere-if predicate (rest tree)))))


(defmacro ?- (&rest goals) `(top-level-prove ',(replace-?-vars goals)))


(defun prove-all (goals bindings)
  "Find a solution to the conjunction of goals."
  (cond ((eq bindings fail) fail)
        ((null goals) bindings)
        (t (prove (first goals) bindings (rest goals)))))


(defun prove (goal bindings other-goals)
  "Return a list of possible solutions to goal."
  (let ((clauses (get-clauses (predicate goal))))
    (if (listp clauses)
        (some
         (lambda (clause)
           (let ((new-clause (rename-variables clause)))
             (prove-all
              (append (clause-body new-clause) other-goals)
              (unify goal (clause-head new-clause) bindings))))
         clauses)
        ;; The predicate's "clauses" can be an atom:
        ;; a primitive function to call
        (funcall clauses (rest goal) bindings
                 other-goals))))


(defun show-prolog-vars (vars bindings other-goals)
  "Print each variable with its binding.
  Then ask the user if more solutions are desired."
  (if (null vars)
      (format t "~&Yes")
      (dolist (var vars)
        (format t "~&~a = ~a" var
                (subst-bindings bindings var))))
  (force-output)
  (if (continue-p)
      fail
      (prove-all other-goals bindings)))


(setf (get 'show-prolog-vars 'clauses) 'show-prolog-vars)


(defun continue-p ()
  "Ask user if we should continue looking for solutions."
  (case (read-char)
    (#\; t)
    (#\. nil)
    (#\newline (continue-p))
    (otherwise 
      (format t " Type ; to see more or . to stop")
      (continue-p))))


(defun variables-in (exp)
  "Return a list of all the variables in EXP."
  (unique-find-anywhere-if #'non-anon-variable-p exp))


(defun non-anon-variable-p (x)
  (and (variable-p x) (not (eq x '?))))


(eval-when (:compile-toplevel :load-toplevel :execute)
(defun replace-?-vars (exp)
  "Replace any ? within exp with a var of the form ?123."
  (cond ((eq exp '?) (gensym "?"))
        ((atom exp) exp)
        (t (reuse-cons (replace-?-vars (first exp))
                       (replace-?-vars (rest exp))
                       exp)))))


;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File prologc.lisp: Final version of the compiler,
;;;; including all improvements from the chapter.




(defmacro set-binding! (trail var value)
  (let ((gvar (gensym))
        (gval (gensym)))
    `(let ((,gvar ,var)
           (,gval ,value))
       (fast
         (unless (eq ,gvar ,gval)
           (vec-push ,trail ,gvar)
           (setf (var-binding ,gvar) ,gval))
         t))))


(defun unify! (trail x y)
  "Destructively unify two expressions"
  (declare (list trail))
  (cond ((equal (deref x) (deref y)) t)
        ((var-p x) (set-binding! trail x y))
        ((var-p y) (set-binding! trail y x))
        ((and (consp x) (consp y))
         (and (unify! trail (first x) (first y))
              (unify! trail (rest x) (rest y))))
        (t nil)))


(defmacro undo-bindings! (trail old-trail)
  (let ((gvar (gensym)))
    `(fast
       (let* ((trail ,trail)
              (,gvar ,old-trail)
              (ndx (trail-ndx trail))
              (vec (trail-vec trail)))
         (declare (simple-vector vec)
                  (type adim ndx ,gvar))
         (loop :until (eql ndx ,gvar)
               :do (setf (var-binding (svref vec (decf ndx)))
                         (load-time-value unbound)))
         (setf (trail-ndx trail) ndx)))))


(defun prolog-compile (symbol &optional
                       (clauses (get-clauses symbol)))
  "Compile a symbol; make a separate function for each arity."
  (unless (null clauses)
    (let ((arity (relation-arity (clause-head (first clauses)))))
      ;; Compile the clauses with this arity
      (compile-predicate
        symbol arity (clauses-with-arity clauses #'= arity))
      ;; Compile all the clauses with any other arity
      (prolog-compile
        symbol (clauses-with-arity clauses #'/= arity)))))


(defun clauses-with-arity (clauses test arity)
  "Return all clauses whose head has given arity."
  (find-all arity clauses
            :key (lambda (clause)
                   (relation-arity (clause-head clause)))
            :test test))


(eval-when (:compile-toplevel :load-toplevel :execute)
(defun relation-arity (relation)
  "The number of arguments to a relation.
  Example: (relation-arity '(p a b c)) => 3"
  (length (args relation)))

(defun make-parameters (arity)
  "Return the list (?arg1 ?arg2 ... ?arg-arity)"
  (loop for i from 1 to arity
        collect (new-symbol '?arg i)))

(defun make-predicate (symbol arity)
  "Return the symbol: symbol/arity"
  (symbolify symbol '/ arity)))


(eval-when (:compile-toplevel :load-toplevel :execute)
(defun make-new-predicate (symbol arity)
  ;;(new-symbol symbol '/ arity)
  (symbolify symbol '/ arity)            ;;;XXX
  )

(defun ensure-functor-locf* (name arity)
  (let ((functor (assoc arity (get name 'functor))))
    (if functor
        functor
        (let ((new-functor (cons arity (make-new-predicate name arity))))
          (push new-functor (get name 'functor))
          new-functor))))


(defmacro make-predicate* (symbol arity)
  `(the cl:symbol (cdr (ensure-functor-locf* ,symbol ,arity)))))


(defun make-= (x y) `(= ,x ,y))


(defun compile-call (predicate args cont)
  "Compile a call to a prolog predicate."
  `(,predicate ,@args ,cont))


(defun prolog-compiler-macro (name)
  "Fetch the compiler macro for a Prolog predicate."
  ;; Note NAME is the raw name, not the name/arity
  (get name 'prolog-compiler-macro))


(defmacro define-prolog-compiler-macro (name arglist &body body)
  "Define a compiler macro for Prolog."
  `(setf (get ',name 'prolog-compiler-macro)
         (lambda ,arglist .,body)))


(defun has-variable-p (x)
  "Is there a variable anywhere in the expression x?"
  (find-if-anywhere #'variable-p x))


(defun proper-listp (x)
  "Is x a proper (non-dotted) list?"
  (or (null x)
      (and (consp x) (proper-listp (rest x)))))


(defun maybe-add-undo-bindings (trail compiled-exps)
  "Undo any bindings that need undoing.
  If there are any, bind the trail before we start."
  (if (length=1 compiled-exps)
      compiled-exps
      `((let ((old-trail (trail-ndx ,trail)))
          ,(first compiled-exps)
          ,@(loop for exp in (rest compiled-exps)
                  collect `(undo-bindings! ,trail old-trail)
                  collect exp)))))


(defun bind-unbound-vars (parameters exp)
  "If there are any variables in exp (besides the parameters)
  then bind them to new vars."
  (let ((exp-vars (set-difference (variables-in exp)
                                  parameters)))
    (if exp-vars
        `(let ,(mapcar (lambda (var) `(,var (?)))
                exp-vars)
           ,exp)
        exp)))


(eval-when (:compile-toplevel :load-toplevel :execute)
(defun make-anonymous (exp &optional
                           (anon-vars (anonymous-variables-in exp)))
  "Replace variables that are only used once with ?."
  (cond ((consp exp)
         (reuse-cons (make-anonymous (first exp) anon-vars)
                     (make-anonymous (rest exp) anon-vars)
                     exp))
        ((member exp anon-vars) '?)
        (t exp)))

(defun anonymous-variables-in (tree)
  "Return a list of all variables that occur only once in tree."
  (values (anon-vars-in tree nil nil)))

(defun anon-vars-in (tree seen-once seen-more)
  "Walk the data structure TREE, returning a list of variabless
   seen once, and a list of variables seen more than once."
  (cond
   ((consp tree)
    (multiple-value-bind (new-seen-once new-seen-more)
                         (anon-vars-in (first tree) seen-once seen-more)
      (anon-vars-in (rest tree) new-seen-once new-seen-more)))
   ((not (variable-p tree)) (values seen-once seen-more))
   ((member tree seen-once)
    (values (delete tree seen-once) (cons tree seen-more)))
   ((member tree seen-more)
    (values seen-once seen-more))
   (t (values (cons tree seen-once) seen-more)))))


(defun compile-unify (trail x y bindings)
  "Return 2 values: code to test if x and y unify,
  and a new binding list."
  (cond
   ;; Unify constants and conses:                       ; Case
   ((not (or (has-variable-p x) (has-variable-p y)))    ; 1,2
    (values (equal x y) bindings))
   ((and (consp x) (consp y))                           ; 3
    (multiple-value-bind (code1 bindings1)
                         (compile-unify trail (first x) (first y) bindings)
      (multiple-value-bind (code2 bindings2)
                           (compile-unify trail (rest x) (rest y) bindings1)
        (values (compile-if code1 code2) bindings2))))
   ;; Here x or y is a variable.  Pick the right one:
   ((variable-p x) (compile-unify-variable trail x y bindings))
   (t              (compile-unify-variable trail y x bindings))))


(defun compile-if (pred then-part)
  "Compile a Lisp IF form. No else-part allowed."
  (case pred
    ((t) then-part)
    ((nil) nil)
    (otherwise `(if ,pred ,then-part))))


(defun compile-unify-variable (trail x y bindings)
  "X is a variable, and Y may be."
  (let* ((xb (follow-binding x bindings))
         (x1 (if xb (cdr xb) x))
         (yb (if (variable-p y) (follow-binding y bindings)))
         (y1 (if yb (cdr yb) y)))
    (cond                                                 ; Case:
      ((or (eq x '?) (eq y '?)) (values t bindings))      ; 12
      ((not (and (equal x x1) (equal y y1)))              ; deref
       (compile-unify trail x1 y1 bindings))
      ((find-anywhere x1 y1) (values nil bindings))       ; 11
      ((consp y1)                                         ; 7,10
       (values `(unify! ,trail ,x1 ,(compile-arg y1 bindings))
               (bind-variables-in y1 bindings)))
      ((not (null xb))
       ;; i.e. x is an ?arg variable
       (if (and (variable-p y1) (null yb))
           (values 't (extend-bindings y1 x1 bindings))   ; 4
           (values `(unify! ,trail ,x1 ,(compile-arg y1 bindings))
                   (extend-bindings x1 y1 bindings))))    ; 5,6
      ((not (null yb))
       (compile-unify-variable trail y1 x1 bindings))
      (t (values 't (extend-bindings x1 y1 bindings))))))


; 8,9
(defun bind-variables-in (exp bindings)
  "Bind all variables in exp to themselves, and add that to
  bindings (except for variables already bound)."
  (dolist (var (variables-in exp))
    (unless (get-binding var bindings)
      (setf bindings (extend-bindings var var bindings))))
  bindings)


(defun follow-binding (var bindings)
  "Get the ultimate binding of var according to bindings."
  (let ((b (get-binding var bindings)))
    (if (eq (car b) (cdr b))
        b
        (or (follow-binding (cdr b) bindings)
            b))))


(defun compile-arg (arg bindings)
  "Generate code for an argument to a goal in the body."
  (cond ((eq arg '?) '(?))
        ((variable-p arg)
         (let ((binding (get-binding arg bindings)))
           (if (and (not (null binding))
                    (not (eq arg (binding-val binding))))
               (compile-arg (binding-val binding) bindings)
               arg)))
        ((not (find-if-anywhere #'variable-p arg)) `',arg)
        ((proper-listp arg)
         `(list .,(mapcar (lambda (a) (compile-arg a bindings))
                          arg)))
        (t `(cons ,(compile-arg (first arg) bindings)
                  ,(compile-arg (rest arg) bindings)))))


(defun bind-new-variables (bindings goal)
  "Extend bindings to include any unbound variables in goal."
  (let ((variables (remove-if (lambda (v) (assoc v bindings))
                              (variables-in goal))))
    (nconc (mapcar #'self-cons variables) bindings)))


(defun self-cons (x) (cons x x))


(define-prolog-compiler-macro = (trail goal body cont bindings)
  "Compile a goal which is a call to =."
  (let ((args (args goal)))
    (if (/= (length args) 2)
        :pass ;; decline to handle this goal
        (multiple-value-bind (code1 bindings1)
                             (compile-unify trail (first args) (second args) bindings)
          (compile-if code1
                      (compile-body* trail body cont bindings1))))))


(defun compile-clause (trail parms clause cont)
  "Transform away the head, and compile the resulting body."
  (bind-unbound-vars       
   parms                  
   ;; fix broken compilation of (setof ?x (or clause clause ..) ?answer)
   (if (member (car clause) '(if or and))
       (compile-body* trail (list clause) cont (mapcar #'self-cons parms))
       (compile-body* trail
                      (nconc (mapcar #'make-= parms (args (clause-head clause)))
                             (clause-body clause))
                      cont
                      (mapcar #'self-cons parms)))))


;***
(defvar *uncompiled* nil 
  "Prolog symbols that have not been compiled.")


(defun add-clause (clause &key asserta)
  "Add a clause to the data base, indexed by head's predicate."
  ;; The predicate must be a non-variable symbol.
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *db-predicates*)
    (pushnew pred *uncompiled*)         ;***
    (setf (get pred 'clauses)
	  (if asserta
	      (nconc (list clause) (get-clauses pred))
	      (nconc (get-clauses pred) (list clause))))
    pred))


(defun retract-clause (clause)
  "Retract a clause from the data base"
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *uncompiled*)
    (setf (get pred 'clauses)
	  (delete clause (get-clauses pred) :test #'equal))
    pred))


(defun top-level-prove (goals)
  "Prove the list of goals by compiling and calling it."
  ;; First redefine top-level-query
  (clear-predicate 'top-level-query)
  (let ((vars (delete '? (variables-in goals))))
    (add-clause `((top-level-query)
                  ,@goals
                  (show-prolog-vars ,(mapcar #'symbol-name vars)
                                    ,vars))))
  ;; Now run it
  (run-prolog 'top-level-query/0 *trail* #'ignorer)
  (format t "~&No.")
  (values))


(defun run-prolog (procedure trail cont)
  "Run a 0-ary prolog procedure with a given continuation."
  ;; First compile anything else that needs it
  (prolog-compile-symbols)
  ;; Reset the trail and the new variable counter
  (setf (trail-ndx trail) 0)
  (setf *var-counter* 0)
  ;; Finally, call the query
  (catch 'top-level-prove
    (funcall procedure trail cont)))


(defun prolog-compile-symbols (&optional (symbols *uncompiled*))
  "Compile a list of Prolog symbols.
  By default, the list is all symbols that need it."
  (with-compilation-unit ()
    (mapc #'prolog-compile symbols)
    (setf *uncompiled* (set-difference *uncompiled* symbols))))


(defun show-prolog-vars/2 (var-names vars cont)
  "Display the variables, and prompt the user to see
  if we should continue.  If not, return to the top level."
  (if (null vars)
      (format t "~&Yes")
      (loop for name in var-names
            for var in vars do
              (format t "~&~a = ~a" name (deref-exp var))))
  (finish-output)
  (if (and vars (continue-p))
      (funcall cont)
      (throw 'top-level-prove nil)))


(defun deref-exp (exp)
  "Build something equivalent to EXP with variables dereferenced."
  (if (atom (deref exp))
      exp
      (reuse-cons
        (deref-exp (first exp))
        (deref-exp (rest exp))
        exp)))


(defun deref-exp-fail (exp)
  (if (atom (deref-fail exp))
      exp
      (reuse-cons (deref-exp-fail (first exp))
                  (deref-exp-fail (rest exp))
                  exp)))


(defvar *predicate* nil
  "The Prolog predicate currently being compiled")


(defvar *defun* 'defun #+tail-recursive-defun 'tail-recursive-defun)


(defun compile-predicate (symbol arity clauses)
  "Compile all the clauses for a given symbol/arity
  into a single LISP function."
  (let ((*predicate* (make-predicate* symbol arity))    ;***
        (parameters (make-parameters arity)))
    (setf (get *predicate* 'defun)
          `(,*defun* ,*predicate* (trail ,@parameters cont)
             .,(maybe-add-undo-bindings 'trail
                                        (mapcar (lambda (clause)
                                                  (compile-clause 'trail parameters clause 'cont))
                                                clauses))))
    (compile (eval (get *predicate* 'defun)))))


(defun goal-cut-p (goal)
  (typecase goal
    ((eql !) T)
    ((cons (eql cut) null) T)
    (T nil)))


(defun goal-lisp-form-p (goal)
  (typecase goal
    ((cons cons *) T)
    (T nil)))


(defun goal-conjunction-p (goal)
  (goal-and-p goal))


(defun goal-and-p (goal)
  (and (consp goal)
       (member (car goal) '(and \,))))


(defun goal-disjunction-p (goal)
  (and (goal-or-p goal)
       (not (goal-if-then-p (cadr goal)))))


(defun goal-or-p (goal)
  (and (consp goal)
       (member (car goal) '(or \;))))


(defun goal-if-p (goal)
  (and (consp goal)
       (eq (car goal) 'if)))


(defun goal-if-then-p (goal)
  (and (goal-if-p goal)
       (null (cdddr goal))))


(defun goal-if-then-else-p (goal)
  (or
   ;; (OR (IF A B) C)
   (and (goal-or-p goal)
        (goal-if-then-p (cadr goal)))
   ;; (IF A B C)
   (and (goal-if-p goal)
        (not (null (cdddr goal)))
        (null (cddddr goal)))))


(defun destructure-if-then-else (goal)
  (cond
    ((goal-or-p goal)
     (destructuring-bind (or/2 (if/2 if then) else) goal
       (declare (ignore or/2 if/2))
       (values if then else)))
    ((goal-if-p goal)
     (destructuring-bind (if/3 if then else) goal
       (declare (ignore if/3))
       (values if then else)))
    (t (error "Goal not an IF-THEN-ELSE: ~S" goal))))


(defun compile-body (trail body cont bindings)
  "Compile the body of a clause."
  (if (null body)
      `(funcall ,cont)
      (let ((goal (first body)))
	(cond
         ((goal-cut-p goal)
          `(progn
             ,(compile-body trail (rest body) cont bindings)
             (return-from ,*predicate* nil)))
         ((goal-conjunction-p goal)
          (compile-body trail (append (cdr goal) (rest body)) cont bindings))
         ((goal-disjunction-p goal)
          (let ((bindings (bind-new-variables bindings goal)))
            `(let ((old-trail (trail-ndx trail))
                   (cont (lambda () ,(compile-body trail (rest body) cont bindings))))
               ,(compile-body trail (list (cadr goal)) 'cont bindings)
               (undo-bindings! ,trail old-trail)
               ,(compile-body trail (list (caddr goal)) 'cont bindings))))
         ((goal-if-then-p goal)
          (let ((bindings (bind-new-variables bindings goal)))
            `(let ((cont (lambda ()
                           ,(compile-body trail (cons (caddr goal) (rest body)) cont bindings))))
               (block nil
                 ,(compile-body trail (list (cadr goal)) '(lambda () (funcall cont) (return nil)) bindings)))))
         ((goal-if-then-else-p goal)
          (let ((bindings (bind-new-variables bindings goal)))
            (multiple-value-bind (if then else)
                                 (destructure-if-then-else goal)
              `(let ((old-trail (trail-ndx ,trail))
                     (cont (lambda ()
                             ,(compile-body trail (rest body) cont bindings))))
                 (block nil
                   ,(compile-body trail
                                  (list if)
                                  `(lambda ()
                                     ,(compile-body trail (list then) cont bindings) (return nil)) bindings)
                   (undo-bindings! ,trail old-trail)
                   ,(compile-body trail (list else) 'cont bindings))))))
         (t
          (let* ((macro (prolog-compiler-macro (predicate goal)))
                 (macro-val (if macro 
                                (funcall macro trail goal (rest body) cont bindings))))
            (if (and macro (not (eq macro-val :pass)))
                macro-val
                `(,(make-predicate* (predicate goal)
                                    (relation-arity goal))
                  ,trail
                  ,@(mapcar (lambda (arg)
                              (compile-arg arg bindings))
                            (args goal))
                  ,(if (null (rest body))
                       cont
                       `(lambda ()
                          ,(compile-body trail
                                         (rest body) cont
                                         (bind-new-variables bindings goal))))))))))))


;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File prologcp.lisp:  Primitives for the prolog compiler
;;;; needed to actually run some functions.

;;; Bug fix by Adam Farquhar, farquhar@cs.utexas.edu.
;;; Trivia: Farquhar is Norvig's cousin.


(eval-when (:compile-toplevel :load-toplevel :execute)
(defun name/arity (symbol)
  (let* ((name/arity (string symbol))
         (split-at (position #\/ name/arity)))
    (cons (subseq name/arity 0 split-at)
          (parse-integer (subseq name/arity (1+ split-at)))))))


(defmacro defpred (name (&rest args) &body body)
  (let* ((name/arity (name/arity name))
         (functor (ensure-functor-locf* (find-symbol (car name/arity)
                                                     (symbol-package name))
                                        (cdr name/arity))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
     #|(setf (fdefinition ',(cdr functor))
             (fdefinition
              (defun ,name (,@args)
                ,@body)))|#
     (defun ,name (,@args)
       ,@body)
     ',(cdr functor))))


(define-condition prolog-throw ()
  ((ball :initarg :ball :reader ball)))


;;; 8.3 type testing

(defun atomicp (exp)
  ;; not ATOM, because we might be implementing unbound VARs as
  ;; something returning true to lisp's ATOM.
  (or (symbolp exp)
      (integerp exp)
      (floatp exp)))


;;; 8.4 term comparison

(defun deref-equal (x y)
  "Are the two arguments EQUAL with no unification,
  but with dereferencing?"
  (or (eql (deref x) (deref y))
      (and (consp x) (consp y)
           (deref-equal (car x) (car y))
           (deref-equal (cdr x) (cdr y)))))


(defun term-precedes (x y)
  (and (not (eql (deref x) (deref y)))
       (typecase x
	 (var (or (not (var-p y))
		  ;; FIXME
		  (evenp (random 2))))
	 ((float) (if (var-p y)
		      nil
		      (or (not (floatp y))
			  (< x y))))
	 ((integer) (if (or (var-p y) (floatp y))
			nil
			(or (not (integerp y))
			    (< x y))))
	 (cl:symbol (if (or (var-p y) (floatp y) (integerp y))
			nil
			(or (not (symbolp y))
			    (string< (string x) (string y)))))
	 ;; "compound term": i.e. not lists, really
	 ((cons) (when (consp y)
		   (or (< (length x) (length y))
		       (and (= (length x) (length y))
			    (or (term-precedes (car x) (car y))
				(and (eql (car x) (car y))
				     (do* ((xis (cdr x) (cdr xis))
					   (xi (car xis) (car xis))
					   (yis (cdr y) (cdr yis))
					   (yi (car yis) (car yis)))
					  ((null xis) nil)
				       (when (term-precedes xi yi)
					 (return t))))))))))))


(defun make-renamed-copy (term)
  (cond
    ((unbound-var-p term) (?))
    ((var-p term) term)
    ((atom term) term)
    (t (cons (make-renamed-copy (car term))
             (make-renamed-copy (cdr term))))))


(defun deref-copy (exp)
  "Copy the expression, replacing variables with new ones.
  The part without variables can be returned as is."
  ;; Bug fix by farquhar and norvig, 12/12/92.  Forgot to deref var.
  (sublis (mapcar (lambda (var) (cons (deref var) (?)))
                  (unique-find-anywhere-if #'var-p exp))
          exp))


;;; 8.15 logic and control

(defmacro with-undo-bindings ((trail) &body body)
  "Undo bindings after each expression in body except the last."
  (if (length=1 body)
      (first body)
      `(let ((old-trail (trail-ndx ,trail)))
         ,(first body)
         ,@(loop for exp in (rest body)
                 collect `(undo-bindings! ,trail old-trail)
                 collect exp))))


(defun retract-same-arity-clause (clause)
  "Retract a clause from the data base"
  (let* ((head (clause-head clause))
         (pred (predicate head))
         (arity (1-  (length head))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *uncompiled*)
    (setf (get pred 'clauses)
	  (delete-if (lambda (x)
                       (let* ((h (clause-head x))
                              (p (predicate h))
                              (a (1- (length h))))
                         (and (eq p pred)
                              (= a arity))))
                     (get-clauses pred)))
    pred))


(eval-when (:compile-toplevel :load-toplevel :execute)
(defun make-predicate-ftype (clause-head)
  (let ((name (predicate clause-head))
        (arity (relation-arity clause-head)))
    `(ftype (function (list ,@(loop :repeat arity :collect T) function) T)
            ,(make-predicate* name arity)))))


(defun insert-deref (exp)
  (if (atom exp)
      (if (variable-p exp)
          `(deref-exp ,exp)
          exp)
      (cons (insert-deref (car exp))
            (insert-deref (cdr exp)))))


(defun insert-deref-fail (exp)
  (if (atom exp)
      (if (variable-p exp)
          `(deref-exp-fail ,exp)
          exp)
      (cons (insert-deref-fail (car exp))
            (insert-deref-fail (cdr exp)))))


(defun prolog-translate-goals (goals)
  (mapcar (lambda (goal)
            (if (starts-with goal 'lisp)
                (let ((vars (variables-in (last goal))))
                  ``(,@',(butlast goal)
                         (apply ,(lambda (,@vars)
                                   ,@(insert-deref (last goal)))
                                (list ,@',vars))))
                `',goal))
          goals))


(defun interpreted-de-wrapper (cont &rest conses)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (dynamic-extent cont conses))
  (apply cont conses))


(defmacro de-consify (pred trail cont &rest args)
  `(apply #'interpreted-de-wrapper
          (lambda (&rest conses)
            (declare (ignorable conses))
            (,(eval pred) ,trail ,cont ,@args))
          (make-list 0)))


(defun-inline let-de-1 (setter cont)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (function setter cont))
  (let ((var (?)))
    (declare (var var))
    (funcall setter var)
    (funcall cont)))


(defmacro let-de ((&rest bindspec) &body body)
  (if bindspec
      `(let (,(caar bindspec))
         (flet ((stuffit (new) (setq ,(caar bindspec) new)))
           (declare (dynamic-extent #'stuffit))
           (let-de-1 #'stuffit
                     (lambda ()
                       (let-de ,(cdr bindspec)
                         ,@body)))))
      `(progn
         ,@body)))


(defmacro %let-de ((&rest vars) &body body)
  (if vars
      `(flet ((stuffit (new) (setq ,(car vars) new)))
         (declare (dynamic-extent #'stuffit))
         (let-de-1 #'stuffit
                   (lambda ()
                     (%let-de ,(cdr vars)
                       ,@body))))
      `(progn
         ,@body)))


(defun compile-body* (trail body cont bindings)
  "Compile the body of a clause."
  (if (null body)
      `(funcall ,cont)
      (let ((goal (first body)))
	(cond
         ((goal-lisp-form-p goal)
          (if (< 1 (length (args goal)))
              (compile-body* trail `((are ,(args goal) ,(predicate goal)) ,@(rest body))
                             cont bindings)
              (compile-body* trail `((is ,@(args goal) ,(predicate goal)) ,@(rest body))
                             cont bindings)))
         ((goal-cut-p goal)
          `(progn
             ,(compile-body* trail (rest body) cont bindings)
             (return-from ,*predicate* nil)))
         ((goal-conjunction-p goal)
          (compile-body* trail (append (cdr goal) (rest body)) cont bindings))
         ((goal-disjunction-p goal)
          (let ((bindings (bind-new-variables bindings goal)))
            `(let ((old-trail (trail-ndx ,trail))
                   (cont (lambda () ,(compile-body* trail (rest body) cont bindings))))
               ,(compile-body* trail (list (cadr goal)) 'cont bindings)
               (undo-bindings! ,trail old-trail)
               ,(compile-body* trail (list (caddr goal)) 'cont bindings))))
         ((goal-if-then-p goal)
          (let ((bindings (bind-new-variables bindings goal)))
            `(let ((cont (lambda ()
                           ,(compile-body* trail (cons (caddr goal) (rest body)) cont bindings))))
               (block nil
                 ,(compile-body* trail (list (cadr goal)) '(lambda () (funcall cont) (return nil)) bindings)))))
         ((goal-if-then-else-p goal)
          (let ((bindings (bind-new-variables bindings goal)))
            (multiple-value-bind (if then else)
                                 (destructure-if-then-else goal)
              `(let ((old-trail (trail-ndx ,trail))
                     (cont (lambda ()
                             ,(compile-body* trail (rest body)
                                             cont bindings))))
                 (block nil
                   ,(compile-body* trail (list if)
                                   `(lambda ()
                                      ,(compile-body* trail (list then) cont bindings)
                                      (return nil))
                                   bindings)
                   (undo-bindings! ,trail old-trail)
                   ,(compile-body* trail (list else) 'cont bindings))))))
         (t
          (let* ((macro (prolog-compiler-macro (predicate goal)))
                 (macro-val (and macro (funcall macro trail goal (rest body) cont bindings))))
            (if (and macro (not (eq macro-val :pass)))
                macro-val
                `(let ((de-cont
                        ,(if (null (rest body))
                             cont
                             `(lambda ()
                                ,(compile-body* trail (rest body)
                                                cont
                                                (bind-new-variables bindings goal))))))
                   (declare (dynamic-extent de-cont))
                   ,(case (predicate goal)
                      ((or is)
                       `(symbol-macrolet ((? (?)))
                          ,@(mapcar (lambda (lv)
                                      `(deref ,lv))
                                    (variables-in (args goal)))
                          (de-consify (make-predicate* ',(predicate goal)
                                                       ',(relation-arity goal))
                                      ,trail
                                      ,@(args goal)
                                      de-cont)))
                      (otherwise
                       `(de-consify (make-predicate* ',(predicate goal)
                                                     ',(relation-arity goal))
                                    ,trail
                                    ,@(mapcar (lambda (arg)
                                                (compile-arg arg bindings))
                                              (args goal))
                                    de-cont)))))))))))


(defmacro prolog (&rest goals)
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
               (,*predicate* trail #'ignorer)
               (let ((trail (allocate-trail *default-trail-size*)))
                 (let ((*trail* trail))
                   (,*predicate* trail #'ignorer)))))))))


(defmacro prolog/free-vars ((&rest free-vars) &rest goals)
  "Run Prolog in the surrounding Lisp environment
which is accessed from lisp functor.
"
  (let ((*predicate* 'prolog-clauses))
    `(block prolog
       (flet ((,*predicate* (trail cont)
                (declare (ignorable trail cont))
                ,(let* ((vars (set-difference (variables-in goals) free-vars)))
                   `(let-de (,@(mapcar (lambda (v) `(,v (?))) vars))
                            ,(compile-body* 'trail goals 'cont nil)))))
         (declare (dynamic-extent #',*predicate*))
         (let ((trail (fast *trail*)))
           (if trail
               (,*predicate* trail #'ignorer)
               (let ((trail (allocate-trail *default-trail-size*)))
                 (let ((*trail* trail))
                   (,*predicate* trail #'ignorer)))))))))


;;; *EOF*
