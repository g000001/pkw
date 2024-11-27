;;;; package.lisp -*- Mode: Lisp;-*- 
(cl:in-package "COMMON-LISP-USER")


(defpackage "https://github.com/g000001/pkw"
  (:documentation "Poorman's KnowledgeWorks")
  (:nicknames pkw)
  (:use :cl :pclog)
  (:export
   defrule 
   def-kb-class
   -->
   clear-all
   infer
   test
   erase
   erase-object
   defcontext
   reset)
  (:export
   "!" ;
   "," ;
   "-->"
   "->"
   "." ;
   "/==" ;
   "/\\" ;
   ":-"
   ";" ;
   "<<" ;
   "=.." ;
   "=:=" ;
   "=<" ;
   "==" ;
   "=\\=" ;
   ">>" ;
   "?" ;
   "?*"
   "?-" ;
   "@<" ;
   "@=<" ;
   "@>" ;
   "@>=" ;
   "ANY" ;
   "ARE"
   "ARG" ;
   "ASSERTA" ;
   "ASSERTZ" ;
   "ATOMIC" ;
   "BAGOF" ;
   "CALL" ;
   "CLAUSE" ;
   "CLEAR-ERROR-OBJECT-LIST"
   "COMMON-PROLOG"
   "COMMON-PROLOG-ASSERTA"
   "COMMON-PROLOG-ASSERTZ"
   "COMPARE"
   "COMPILE-AND-RECONSULT"
   "CONSULT"
   "COUNTER"
   "CURRENT-OP"
   "CUT" ;
   "DEBUGGING"
   "DEFDETPRED"
   "DEFDETPRED-WITH-REST"
   "DEFDETREL"
   "DEFDETUNIPRED"
   "DEFGRAMMAR"
   "DEFLOGFUN" ;
   "DEFREL" ;
   "DEFRELMACRO"
   "DEFSPECIAL"
   "DETERMINISTIC"
   "DETPRED-FAIL"
   "DISPLAY"
   "DYNAMIC"
   "EDINBURGH"
   "ERASE"
   "ERQP"
   "ERROR-OBJECT-LIST"
   "EXISTS"
   "EXIT"
   "FAIL" ;
   "FINDALL" ;
   "FINDALLSET" ;
   "FUNCTOR" ;
   "GET0"
   "HALT"
   "IS" ;
   "KEYSORT"
   "LEASH"
   "LISP-CALL"
   "LISPWORKS"
   "LISTING"
   "LOGIC" ;
   "LOGIC-LISTENER"
   "MAKE-INTERNAL-VAR"
   "MODE"
   "NAME"
   "NEXT-COUNTER"
   "NL" ;
   "NO."
   "NODEBUG"
   "NONVAR" ;
   "NOSPY"
   "NOTRACE"
   "OK."
   "ONCE" ;
   "OP"
   "OUTPUT-DEFRELS"
   "PHRASE"
   "PORTRAY"
   "PUT"
   "READ-TERM"
   "RECONSULT"
   "RECORDA"
   "RECORDED"
   "RECORDZ"
   "REDO"
   "REPEAT"
   "RETRACT"
   "RETRACTALL"
   "RQP"
   "SEE"
   "SEEING"
   "SEEN"
   "SETOF" ;
   "SKIP"
   "SPECIAL-ARG"
   "SPY"
   "TAB"
   "TELL"
   "TELLING"
   "TERM-P"
   "TERM-REF"
   "TEST"
   "TOLD"
   "TRANSLATE-FILE"
   "TRANSLATE-VARS"
   "TTYDISPLAY"
   "TTYNL"
   "TTYPUT"
   "UNDEFINED"
   "UNLEASH"
   "USER"
   "VAR"
   "WITH-DEBUG"
   "WITH-PROLOG" ;
   "WITHOUT-DEBUG"
   "WRITEQ"
   "YES."
   "\\"
   "\\+"
   "\\/" ;
   "\\==" ;
   "^"))


(defpackage "https://github.com/g000001/pkw#internals"
  (:use "https://github.com/g000001/pkw"
        "COMMON-LISP"))


;;; *EOF*
