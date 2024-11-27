;;;; PKW.lisp -*- Mode: Lisp;-*- 
(cl:in-package "https://github.com/g000001/pkw#internals")


;;(#|defrule subs|#
;; eval-when (:compile-toplevel :load-toplevel :execute)

(defun lhs (body)
  (subseq body 0 (position '--> body)))


(defun rhs (body)
  (subseq body (1+ (position '--> body))))


(defun getopts (body)
  (do* ((b body (cddr b))
        (ans (list nil))
        (tem ans))
       ((endp b) (cdr ans))
    (when (eq :priority (car b))
      (setf (cdr tem)
            (list :salience (cadr b))))
    (when (eq :context (car b))
      (setf (cdr tem)
            (list :context (cadr b))))))


(defun parse-defrule-body (body &aux (doc nil))
  (when (typep (car body) 'string)
    (setq doc (pop body)))
  (do* ((b body)
        (ans (list nil))
        (tem ans)
        (newbody (list nil))
        (tembody newbody))
       ((endp b) 
        (values (cdr ans)
                (lhs (cdr newbody))
                (rhs (cdr newbody))
                doc))
    (typecase b
      ((CONS (EQL :priority) *)
       (setf (cdr tem)
             (list :salience (cadr b)))
       (setq tem (cddr tem))
       (setq b (cddr b)))
      ((CONS (EQL :context) *)
       (setf (cdr tem)
             (list :context (cadr b)))
       (setq tem (cddr tem))
       (setq b (cddr b)))
      (T (setf (cdr tembody)
               (setq tembody (list (car b))))
         (pop b)))))


#|(defun process-rhs-lisp-form (rhs)
  `(nil:let* (,@(mapcar (lambda (rule)
                      (if (= 1 (length rule))
                          `(,(gensym "rhs-var") ,@rule)
                          (reverse rule))) 
                    (remove-if-not #'consp rhs :key #'car)))
     ,@(mapcar #'assert-to-modyfy
               (remove-if #'consp rhs :key #'car))))|#


(defun process-rhs-lisp-form (vars rhs)
  `(pclog::with-prolog/free-vars (,@vars)
     ,@(mapcar #'process-backward-chaining-rules rhs)))


(defun process-rhs (lhs-vars rhs)
  (process-rhs-lisp-form lhs-vars rhs))


(defun make-lhs-form (form)
  (typecase form
    ((CONS (EQL test) *)
     `(lisa:test (not (null ,@(cdr form)))))
    (T (destructuring-bind (c cvar &rest i-ivar)
                           form
         `(,cvar (,c ,@(loop :for x :on i-ivar :by #'cddr
                             :collect (subseq x 0 2))))))))


(defun process-lhs (rhs)
  (mapcar #'make-lhs-form rhs))


(defun assert-to-modyfy (form)
  (destructuring-bind (assert (c cvar &rest kvs))
                      form
    (declare (ignore assert c))
    `(lisa:modify ,cvar
       ,@(loop :for x :on kvs :by #'cddr
               :collect (subseq x 0 2)))))


(defun process-backward-chaining-rules (form)
  (destructuring-bind (op &rest args)
                      form
    (case op
      (assert (destructuring-bind ((c cvar &rest kvs))
                                  args
                (declare (ignore c))
                `((lisa:modify ,cvar
                    ,@(loop :for x :on kvs :by #'cddr
                            :collect (subseq x 0 2))))))
      (erase (destructuring-bind (var)
                                 args
               `((lisa:retract-instance ,var))))
      (otherwise form))))


;; #|defrule subs|#)


(defmacro defrule (name direction &body body)
  (check-type direction (member :forward :backward))
  (ecase direction
    (:forward
     (multiple-value-bind (opts lhs rhs doc)
                          (parse-defrule-body body)
       `(lisa:defrule ,name (,@opts)
          ,doc
          ,@(process-lhs lhs) 
          lisa:=>
          ,(process-rhs (pclog::variables-in lhs) rhs))))
    (:backward
     `(pclog:defrel ,name
        ,@(mapcar (lambda (clause)
                    (destructuring-bind (head <-- . tail)
                                        clause
                      (declare (ignore <--))
                      `(,head ,@tail)))
                  body)))))


(defmacro def-kb-class (name (&rest supers) &body slots)
  `(defclass ,name (lisa::inference-engine-object ,@supers)
     ,@slots))


(defun clear-all ()
  (lisa:clear))


(defun erase-object (obj)
  #+TODO (lisa:retract-instance obj)
  (values obj))


(defmethod initialize-instance :after
           ((inst lisa::inference-engine-object) &rest args &key)
  (lisa:assert-instance inst))


(defun infer (&key contexts)
  (lisa:run contexts))


(defmacro defcontext (name &key strategy)
  (declare (ignore strategy))
  ;;;`(lisa:defcontext ,name ,@(when strategy `((quote ,strategy))))
  `(lisa:defcontext ,name)
  )


(defun reset ()
  (lisa:reset))


;;; *EOF*
