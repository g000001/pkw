;;; -*- mode: Lisp; coding: utf-8  -*-

(cl:in-package "CL-USER")


(defpackage pclog
  (:use cl)
  (:export "!" ;
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
           "^"
           "append"
           "are"
           "arg"
           "asserta"
           "assertz"
           "atomic"
           "bagof"
           "call"
           "clause"
           "close"
           "common_prolog"
           "compare"
           "compile-and-reconsult"
           "compile_and_reconsult"
           "consult"
           "current_op"
           "debugging"
           "display"
           "dynamic"
           "erase"
           "error"
           "exists"
           "exit"
           "fail"
           "findall"
           "findallset"
           "full"
           "functor"
           "fx"
           "fy"
           "get"
           "get0"
           "half"
           "halt"
           "in_package"
           "integer"
           "is"
           "keysort"
           "leash"
           "listing"
           "loose"
           "member"
           "mod"
           "name"
           "nl"
           "nodebug"
           "nonvar"
           "nospy"
           "not"
           "notrace"
           "off"
           "once"
           "op"
           "portray"
           "print"
           "put"
           "read"
           "reconsult"
           "recorda"
           "recorded"
           "recordz"
           "redo"
           "repeat"
           "retract"
           "retractall"
           "see"
           "seeing"
           "seen"
           "setof"
           "skip"
           "sort"
           "spy"
           "tab"
           "tell"
           "telling"
           "tight"
           "told"
           "trace"
           "true"
           "ttydisplay"
           "ttynl"
           "ttyput"
           "undefined"
           "unleash"
           "user"
           "var"
           "write"
           "writeq"
           "xf"
           "xfx"
           "xfy"
           "yf"
           "yfx"))


;;; *EOF*
