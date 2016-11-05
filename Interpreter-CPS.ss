;:  Single-file version of the interpreter.
;; Easier to submit to server, probably harder to use in the development process
;; Brendan Goldacker and Cameron Metzger

; Everything until interpreter that should be in CPS is. 
; Ive added TODOs to functions left that we need to change
; If you change a proc to CPS then mark it with a "; CPS" tag so it it easier to find
; and we can know that its alread been changed
; Test every proc that you change as you go

;(load "C:/Users/goldacbj/Google Drive/Documents/CSSE/CSSE304/chez-init.ss") 
(load "C:/Users/metzgecj/Desktop/Year3/PLC/chez-init.ss") 

;-------------------+
;                   |
;    DATATYPES      |
;                   |
;-------------------+


; returns true if x is a literal (anything that evaluates to itself)  CHECK 
(define literal? 
    (lambda (x)
        (or (number? x) (string? x) (boolean? x) (vector? x) (null? x) (and (list? x) (equal? 'quote (car x)) (or (list? (cadr x)) (symbol? (cadr x)) (vector? (cadr x)))))))



; datatypes: lambda-exp, variable, lit-exp, if-exp, if-else-exp, let-exp, letrec-exp, let*-exp, set!-exp
(define-datatype expression expression?
    [var-exp (id symbol?)]
    [lambda-exp
        (vars (lambda (x) (or  (and (pair? x) (not (list? x))) (null? x) (symbol? x) ((list-of symbol?) x))))
        (body (lambda (x) (or ((list-of expression?) x) (symbol? x))))]
    [app-exp
        (rator expression?)
        (rand (list-of expression?))]
    [lit-exp (id literal?)]
    [if-exp 
        (condition expression?)
        (if-body expression?)]
    [if-else-exp
        (condition expression?)
        (if-body expression?)
        (else-body expression?)]
    [let-exp
        (var-binds (list-of (lambda (x) (and (pair? x) (symbol? (car x)) (expression? (cadr x))))))
        (body (list-of expression?))]
    [let*-exp
        (var-binds (list-of (lambda (x) (and (pair? x) (symbol? (car x)) (expression? (cadr x))))))
        (body (list-of expression?))]
    [letrec-exp 
        (var-binds (list-of (lambda (x) (and (pair? x) (symbol? (car x)) (expression? (cadr x))))))
        (body (list-of expression?))]
    [set!-exp
        (var symbol?)
        (value expression?)]
    [and-exp 
        (tests (list-of expression?))]
    [or-exp 
        (tests (list-of expression?))]
     [begin-exp
         (body (list-of expression?))]
    [case-exp
        (key expression?)
        (body list?)]
    [while-exp
        (test expression?)
        (body (list-of expression?))]
    [named-let-exp  
        (name symbol?)
        (var-binds (list-of (lambda (x) (and (pair? x) (symbol? (car x)) (expression? (cadr x))))))
        (body (list-of expression?))]
    [define-exp
        (vars symbol?)
        (body expression?)]
    [cond-exp 
        (cases (list-of (lambda (all-cases) (list-of (lambda (single-case) 
                            (and ((list-of expression?) (car x)) ((list-of expression?) (cadr x))))))))])

(define scheme-value?
  (lambda (x) #t))

; environment datatypes
(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
    (syms (list-of symbol?))
    (vals (list-of scheme-value?))
    (env environment?)])

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.
(define-datatype proc-val proc-val?
    [prim-proc
        (name in-prim?)]
   [k-proc 
        (k continuation?)]
   [closure
        (vars (lambda (x) (or  (and (pair? x) (not (list? x))) (null? x) (symbol? x) ((list-of symbol?) x))))
        (bodies (list-of expression?))
        (env environment?)
        (k continuation?)])

(define in-prim? 
    (lambda (x)
        (if (member x *prim-proc-names*)
            #t
            (proc-val? x))))

(define-datatype continuation continuation?
    [init-k]
;   [if-else-test-k  
;      (then-exp expression?)
;        (else-exp expression?)
;        (env environment?)
;        (k continuation?)]
;    [rator-k 
;        (rands (list-of expression?))
;        (env environment?)
;        (k continuation?)]
;    [append-k
;        (car-L1 symbol?)
;        (k continuation)]
;    [rands-k 
;        (proc-val proc-val?)
;;        (k continuation?)]
    [extend-env-record-k
        (syms (list-of symbol?))
        (env environment?)
        (k continuation?)]
    [list-find-pos-k
        (sym symbol?)
        (vals list?)
        (env environment?)
        (succed-cps procedure?)
        (fail-cps procedure?)
        (k continuation?)]
    [deref-k
        (k continuation?)]
    [list-index-r-k
        (k continuation?)]
    [map-cps-1-k
        (proc procedure?)
        (ls list?)
        (k continuation?)]
    [map-cps-2-k
        (value scheme-value?)
        (k continuation?)]
        )


;-------------------+
;                   |
;    apply-k        |
;                   |
;-------------------+


(define apply-k
    (lambda (k v)
        (if (continuation? k)
            (cases continuation k 
                [init-k () v] 
;                [rator-k (rands env k) 
;                   (eval-rands rands env (rands-k val k))]
;                [append-k (car-L1 k)  ; v is the appended list 
;                    (apply-k k (cons car-L1 v))]
;                [rands-k (proc-val k) 
;                    (apply-proc proc-val val k)]
;                [if-else-k (then-exp else-exp env k) ; v is the result of the test
;                    (if v
;                        (eval-exp then-exp env k)
;                        (eval-exp else-exp env k))]
                [extend-env-record-k (syms env k)  ; v is the map ref values
                   (apply-k k (extended-env-record syms v env))]
                [list-find-pos-k (sym vals env succed-cps fail-cps k) ; v is pos in list ;;mess-up here
                    (if (number? v)
                        (succed-cps (list-ref vals v) k)
                        (apply-env-ref env sym succed-cps fail-cps k))]
                [deref-k (k) ; v is the value to deref
                    (deref v k)]
                [list-index-r-k (k) ; v is the index
                    (if (number? v)
                       (apply-k k (+ 1 v))
                        (apply-k k #f))]
                [map-cps-1-k (proc-cps ls k) ; v is proc-cps value of (car ls)
                    (map-cps proc-cps (cdr ls) (map-cps-2-k v k))]
                [map-cps-2-k (proced-car k) ; v is the res of the ls
                    (apply-k k (cons proced-car v))]
            )
            (apply k v))))
	 
	

;-------------------+
;                   |
;    PARSER         |
;                   |
;-------------------+


; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)


; parses expression into seperate expression and variable variants (if expressionis evaluated then it is an expression, else it is a variable)
(define parse-exp         
  (lambda (datum)
    (cond
        [(and (not (null? datum)) (pair? datum) (not (list? datum))) 
            (eopl:error 'parse-exp "expression ~s is not a proper list" datum)]
        [(symbol? datum) (var-exp datum)]
        [(literal? datum) (lit-exp datum)]
        [(null? datum) (lit-exp datum)]
        [(pair? datum)
            (cond
                [(eqv? (car datum) 'lambda)
                    ;error checking 
                   (cond [(< (length datum) 3)
                             (eopl:error 'parse-exp "lambda-expression: incorrect length ~s" datum)]
                        [(and (list? (2nd datum)) (not ((list-of symbol?) (2nd datum))))
                            (eopl:error 'parse-exp "lambda's formal arguments ~s must all be symbols" (2nd datum))]
                        [else 
                          (if (list? (2nd datum))
                             (lambda-exp (2nd datum) ; (lambda (x) body ...) expression (adds paren around body)
                                 (map parse-exp (cddr datum)))
                             (lambda-exp (2nd datum) ; (lambda x body ...) expression (adds paren around body)
                                (map parse-exp (cddr datum))))])]
                [(and (eqv? (car datum) 'let) (not (symbol? (2nd datum))))
                    (cond [(< (length datum) 3) (eopl:error 'parse-exp "~s-expression has incorrect length ~s" (car datum) datum)]
                        [(and (pair? (2nd datum)) (not (list? (2nd datum)))) (eopl:error 'parse-exp "declarations in ~s-expression not a list ~s" (car datum) datum)]
                        [(or (not (list? (2nd datum))) (not (for-all (lambda (pair) (or (null? pair) (list? pair))) (2nd datum)))) 
                            (eopl:error 'parse-exp "declaration in ~s-exp is not a proper list ~s" (car datum) datum)]
                        [(not (for-all (lambda (pair) (equal? 2 (length pair))) (2nd datum))) 
                            (eopl:error 'parse-exp "declaration in ~s-exp must be a list of length 2 ~s" (car datum) datum)]
                        [(not (for-all (lambda (pair) (symbol? (car pair))) (2nd datum)))
                            (eopl:error 'parse-exp "vars in ~s-exp must be symbols ~s" (car datum) datum)]
                        [else 
                            (let-exp (map (lambda (pair) (list (car pair) (parse-exp (cadr pair)))) (2nd datum)) ; (let ((x val) ...) body ...) expression (adds extra paren around body)
                              (map parse-exp (cddr datum)))])]
                [(and (eqv? (car datum) 'let) (symbol? (2nd datum)))
                    (cond [(< (length datum) 4) (eopl:error 'parse-exp "~s-expression has incorrect length ~s" (car datum) datum)]
                        [(and (pair? (3rd datum)) (not (list? (3rd datum)))) (eopl:error 'parse-exp "declarations in ~s-expression not a list ~s" (car datum) datum)]
                        [(or (not (list? (3rd datum))) (not (for-all (lambda (pair) (or (null? pair) (list? pair))) (3rd datum)))) 
                            (eopl:error 'parse-exp "declaration in ~s-exp is not a proper list ~s" (car datum) datum)]
                        [(not (for-all (lambda (pair) (equal? 2 (length pair))) (3rd datum))) 
                            (eopl:error 'parse-exp "declaration in ~s-exp must be a list of length 2 ~s" (car datum) datum)]
                        [(not (for-all (lambda (pair) (symbol? (car pair))) (3rd datum)))
                            (eopl:error 'parse-exp "vars in ~s-exp must be symbols ~s" (car datum) datum)]
                        [else 
                            (named-let-exp (2nd datum) (map (lambda (pair) (list (car pair) (parse-exp (cadr pair)))) (3rd datum)) ;(let name ((x val) ...) body ...) 
                                (map parse-exp (cdddr datum)))])]
                [(eqv? (car datum) 'let*)
                    (cond [(< (length datum) 3) (eopl:error 'parse-exp "~s-expression has incorrect length ~s" (car datum) datum)]
                        [(and (pair? (2nd datum)) (not (list? (2nd datum)))) (eopl:error 'parse-exp "declarations in ~s-expression not a list ~s" (car datum) datum)]
                        [(or (not (list? (2nd datum))) (not (for-all (lambda (pair) (or (null? pair) (list? pair))) (2nd datum)))) 
                            (eopl:error 'parse-exp "declaration in ~s-exp is not a proper list ~s" (car datum) datum)]
                        [(not (for-all (lambda (pair) (equal? 2 (length pair))) (2nd datum))) 
                            (eopl:error 'parse-exp "declaration in ~s-exp must be a list of length 2 ~s" (car datum) datum)]
                        [(not (for-all (lambda (pair) (symbol? (car pair))) (2nd datum)))
                            (eopl:error 'parse-exp "vars in ~s-exp must be symbols ~s" (car datum) datum)]
                        [else 
                             (let*-exp (map (lambda (pair) (list (car pair) (parse-exp (cadr pair)))) (2nd datum)) ; (let ((x val) ...) body ...) expression (adds extra paren around body)
                                (map parse-exp (cddr datum)))])]
                [(eqv? (car datum) 'letrec)
                    (cond [(< (length datum) 3) (eopl:error 'parse-exp "~s-expression has incorrect length ~s" (car datum) datum)]
                        [(and (pair? (2nd datum)) (not (list? (2nd datum)))) (eopl:error 'parse-exp "declarations in ~s-expression not a list ~s" (car datum) datum)]
                        [(or (not (list? (2nd datum))) (not (for-all (lambda (pair) (or (null? pair) (list? pair))) (2nd datum)))) 
                            (eopl:error 'parse-exp "declaration in ~s-exp is not a proper list ~s" (car datum) datum)]
                        [(not (for-all (lambda (pair) (equal? 2 (length pair))) (2nd datum))) 
                            (eopl:error 'parse-exp "declaration in ~s-exp must be a list of length 2 ~s" (car datum) datum)]
                        [(not (for-all (lambda (pair) (symbol? (car pair))) (2nd datum)))
                            (eopl:error 'parse-exp "vars in ~s-exp must be symbols ~s" (car datum) datum)]
                        [else 
                            (letrec-exp (map (lambda (pair) (list (car pair) (parse-exp (cadr pair)))) (2nd datum)) ; (let ((x val) ...) body ...) expression (adds extra paren around body)
                                (map parse-exp (cddr datum)))])]
                [(eqv? (car datum) 'if)
                    (cond [(< (length datum) 3) 
                                (eopl:error 'parse-exp "if-expression ~s does not have (only) test, then, and else" datum)]
                        [else 
                            (if (<= (length datum) 3)
                                (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum))) ; (if condition body) expression
                                (if-else-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) ; (if condition then else) expression
                                   (parse-exp (cadddr datum))))])]
                [(eqv? (car datum) 'set!)
                    (cond [(not (equal? (length datum) 3)) (eopl:error 'parse-exp "set! expression ~s does not have (only) variable and expression" datum)]
                        [else 
                             (set!-exp (2nd datum) (parse-exp (3rd datum)))])] ; (set! var val) expression
                [(eqv? (car datum) 'cond)
                    (cond-exp (map (lambda (x) (list (parse-exp (car x)) (list (map parse-exp (cdr x))))) (cdr datum)))]
                [(eqv? (car datum) 'and)
                    (and-exp (map parse-exp (cdr datum)))]
                [(eqv? (car datum) 'or)
                    (or-exp (map parse-exp (cdr datum)))]
                [(eqv? (car datum) 'begin)
                    (begin-exp (map parse-exp (cdr datum)))]
                [(eqv? (car datum) 'case)
                    (case-exp (parse-exp (2nd datum)) (map (lambda (x) (list (car x) (parse-exp (cadr x)))) (cddr datum)))]
                [(eqv? (car datum) 'while)
                    (while-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum)))]
                [(eqv? (car datum) 'define)
                    (define-exp (2nd datum) (parse-exp (3rd datum)))]
                [else (app-exp (parse-exp (1st datum)) ; (x y z ...) expression (x is a procedure, y z ... are paremeters)
		            (map parse-exp (cdr datum)))])]
        [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))


(define unparse-exp         
  (lambda (datum)
    (cond
     [(symbol? datum) datum]
     [(literal? datum) datum]
     [(null? datum) datum]
     [(pair? datum)
      (cond
        [(eqv? (car datum) 'var-exp) (2nd datum)]
        [(eqv? (car datum) 'lambda-exp)
                (append (list 'lambda (2nd datum)) (unparse-exp (3rd datum)))]
        [(eqv? (car datum) 'app-exp) (append (list (unparse-exp (2nd datum))) (unparse-exp (3rd datum)))]
        [(eqv? (car datum) 'lit-exp) (2nd datum)]
        [(eqv? (car datum) 'let-exp)
            (append (list 'let (unparse-exp (2nd datum))) (unparse-exp (3rd datum)))]
        [(eqv? (car datum) 'named-let-exp)
            (append (list 'let (2nd datum) (unparse-exp (3rd datum))) (unparse-exp (cadddr datum)))]
        [(eqv? (car datum) 'let*-exp)
            (append (list 'let* (unparse-exp (2nd datum))) (unparse-exp (3rd datum)))]
        [(eqv? (car datum) 'letrec-exp)
            (append (list 'letrec (unparse-exp (2nd datum))) (unparse-exp (3rd datum)))]
        [(eqv? (car datum) 'if-else-exp)
            (append (list 'if (unparse-exp (2nd datum))) (list (unparse-exp (3rd datum)) (unparse-exp (cadddr datum))))]
        [(eqv? (car datum) 'set!-exp)
            (list 'set! (unparse-exp (2nd datum)))] ; (set! var val) expression
        [((list-of list?) datum) (append (list (unparse-exp (car datum))) (unparse-exp (cdr datum)))]
        [(eqv? (car datum) 'prim-proc) (2nd datum)]
         [(eqv? (car datum) 'begin-exp)
            (cons 'begin (map unparse-exp (2nd datum)))]
        [(eqv? (car datum) 'cond-exp) (cons 'cond (map (lambda (case) (apply cons (unparse-exp (car case)) 
                                                         (car (map unparse-exp (cdr case))))) (cadr datum)))]
        [(eqv? (car datum) 'and-exp) (apply list 'and (map unparse-exp (2nd datum)))]
        [(eqv? (car datum) 'or-exp) (apply list 'or (map unparse-exp (2nd datum)))]
        [(eqv? (car datum) 'case-exp) (list 'case (unparse-exp (2nd datum)) (3rd datum))]
        [(eqv? (car datum) 'while-exp) (apply list 'while (unparse-exp (2nd datum)) (unparse-exp (3rd datum)))]
        [(eqv? (car datum) 'define-exp) (list 'define (2nd datum) (unparse-exp (3rd datum)))]
        [else #f])])))




;-----------------------+
;                       |
;   SYNTAX EXPANSION    |
;                       |
;-----------------------+


(define syntax-expand 
    (lambda (exp)
        (cases expression exp
            [lit-exp (datum) (lit-exp datum)]
            [let-exp (var-binds bodies) 
                (app-exp (lambda-exp (get-var-binds var-binds) (map syntax-expand bodies)) (map syntax-expand (get-binds var-binds)))]
            [var-exp (var) (var-exp var)]
            [lambda-exp (vars bodies) 
                 (lambda-exp vars (map syntax-expand bodies))]
            [app-exp (rator rands) (app-exp (syntax-expand rator)
                                            (map syntax-expand rands))]
            [if-else-exp (test then-exp else-exp)
                    (if-else-exp (syntax-expand test)
                                  (syntax-expand then-exp)
                                  (syntax-expand else-exp))]
            [if-exp (test then-exp)
                    (if-exp (syntax-expand test)
                                  (syntax-expand then-exp))]
            [letrec-exp (var-binds bodies) (syntax-expand (list 'let-exp (map list-with-false var-binds) (append (map add-set!-exp var-binds)  bodies)))]

            [let*-exp (var-binds body) (letrec ([parse-let* (lambda (var-binds) 
                                                                (if (null? (cdr var-binds)) 
                                                                    (app-exp (lambda-exp (list (caar var-binds)) (map syntax-expand body)) (list (syntax-expand (cadar var-binds))))
                                                                    (app-exp (lambda-exp (list (caar var-binds)) (list (parse-let* (cdr var-binds)))) (list (syntax-expand (cadar var-binds))))))])
                                                  (parse-let* var-binds))]  
            [set!-exp (new-val value)
                (set!-exp new-val (syntax-expand value))]
            [and-exp (tests)
                (if (null? tests)
                    (lit-exp #f)
                (if (null? (cdr tests))
                    (syntax-expand (car tests))
                    (syntax-expand (let-exp (list (list 'test-case (syntax-expand (car tests))))
                        (list (if-else-exp (var-exp 'test-case)
                                (syntax-expand (and-exp (cdr tests)))
                                (var-exp 'test-case)))))))]
            [or-exp (tests) 
                (if (null? tests)
                    (lit-exp #f)
                    (if (null? (cdr tests))
                        (syntax-expand (car tests))
                        (syntax-expand (let-exp (list (list 'test-case (syntax-expand (car tests))))
                                            (list (if-else-exp (var-exp 'test-case)
                                                        (var-exp 'test-case)
                                                        (syntax-expand (or-exp (cdr tests)))))))))]
            [begin-exp (list-of-bodies)
                  (syntax-expand (app-exp (lambda-exp '() list-of-bodies) '()))]
            [cond-exp (cases)
                (letrec ([parse-cond (lambda (cases)
                                        (if (equal? 'else (cadaar cases))
                                            (syntax-expand (begin-exp (caadar cases)))
                                            (if (null? (cdr cases))
                                                (if-exp (syntax-expand (caar cases))
                                                        (syntax-expand (begin-exp (caadar cases))))
                                                (if-else-exp (syntax-expand (caar cases))
                                                    (syntax-expand (begin-exp (caadar cases)))
                                                    (parse-cond (cdr cases))))))])
                    (parse-cond cases))]     
            [case-exp (key body)
                (letrec ([expand-case (lambda (body)
                                            (if (equal? 'else (caar body))
                                                (syntax-expand (cadar body))
                                                (if (null? (cdr body))
                                                    (if-exp (app-exp (var-exp 'member) (list key (list 'lit-exp (caar body))))
                                                        (syntax-expand (cadar body)))
                                                    (if-else-exp (app-exp (var-exp 'member) (list key (list 'lit-exp (caar body))))
                                                        (syntax-expand (cadar body))
                                                        (expand-case (cdr body))))))])
                        (expand-case body))]
            [while-exp (test body)
                (while-exp (syntax-expand test) (map syntax-expand body))]
            [named-let-exp (name var-binds bodies)
                (let ([vars (get-var-binds var-binds)]
                        [binds (get-binds var-binds)])
                    (syntax-expand (quasiquote (app-exp (letrec-exp ([(unquote name) (lambda-exp (unquote vars) (unquote bodies))])
                                                (unquote (list (list 'var-exp name)))) (unquote binds)))))]
            [define-exp (var body) 
                (define-exp var (syntax-expand body))]
            [else (eopl:error 'syntax-expand "not an expression ~s" exp)])))


(define get-var-binds
    (lambda (let-bindings)
            (map car let-bindings)))

(define get-binds 
    (lambda (let-bindings)
            (map cadr let-bindings)))

(define list-with-false 
    (lambda (let-bindings)
        (list (car let-bindings) (lit-exp 0))))
        
(define add-set!-exp
    (lambda (binding)
            (list 'set!-exp (car binding) (syntax-expand (cadr binding)))))



;-------------------+
;                   |
;   ENVIRONMENTS    |
;                   |
;-------------------+



; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))


(define extend-env ;CPS
  (lambda (syms vals env k)
    (map-cps ref vals (extend-env-record-k syms env k)))) 


(define list-find-position ; CPS
  (lambda (sym los k)
      (list-index (lambda (xsym) (eqv? sym xsym)) los k)))


(define list-index ; CPS
  (lambda (pred ls k)
    (cond
        [(null? ls) (apply-k k #f)]
        [(pred (car ls)) (apply-k k 0)]
        [else 
            (list-index pred (cdr ls) (list-index-r-k k))])))

(define apply-env-ref ;CPS
    (lambda (env sym succeed-cps fail-cps k)
         (cases environment env
            [empty-env-record ()
                (fail-cps sym k)]
      [extended-env-record (syms vals env)
	        (list-find-position sym syms (list-find-pos-k sym vals env succeed-cps fail-cps k))])))

(define apply-env ;CPS
  (lambda (env sym succeed-cps fail-cps k)
        (apply-env-ref env sym succeed-cps fail-cps (deref-k k))))




;-------------------+
;                   |
;   INTERPRETER     |
;                   |
;-------------------+


; functions for references

(define set-ref!  ; CPS
    (lambda (r val k)
        (apply-k k (set-box! r val))))

(define deref ; CPS
    (lambda (val k) 
        (apply-k k (unbox val))))
(define ref? box?)
(define ref  ; CPS
    (lambda (val k)
        (apply-k k (box val))))



; helper funcs

(define map-cps  ; CPS
    (lambda (proc-cps ls k)
        (cond 
            [(null? ls) (apply-k k '())]
            [else (proc-cps (car ls) (map-cps-1-k proc-cps ls k))])))

(define apply-env-succeed-cps ;CPS
    (lambda (v k)
        (apply-k k v)))

(define apply-env-fail-cps ;CPS
    (lambda (v k)
        (apply-env-ref global-env v apply-env-succeed-cps apply-env-ref-fail-cps k)))

(define apply-env-ref-fail-cps ; CPS
    (lambda (v k) 
        (eopl:error 'apply-env ; procedure to call if id not in env
		                                        "variable not found in environment: ~s"
			                                     id)))

;helper functions for let-exp 

;TODO
(define get-vars  
    (lambda (ls)
        (map car (unparse-exp ls))))

;TODO
(define get-exps
    (lambda (ls)
        (map cadr ls)))



; top-level-eval evaluates a form in the global environment
(define top-level-eval ; CPS
  (lambda (form)
    (eval-exp form (empty-env) (init-k))))



; eval-exp is the main component of the interpreter
;TODO
(define eval-exp
  (lambda (exp env k)
    (cases expression exp
      [lit-exp (datum)
                (if (and (list? datum) (eqv? (car datum) 'quote))
                    (cadr datum)
                    datum)]
      [var-exp (id)
				(apply-env env id; look up its value.
      	   (lambda (x) x) ; procedure to call if id is in the environment 
           (lambda () (apply-env-ref global-env id (lambda (x) x) 
                                              (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
		                                        "variable not found in environment: ~s"
			                                     id)) k)) k)] 
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env k)]
              [args (eval-rands rands env k)])
          (apply-proc proc-value args))]
     [if-else-exp (test-exp then-exp else-exp)
        (if (eval-exp test-exp env k)    ;;need to add enviornments
            (eval-exp then-exp env k)    
            (eval-exp else-exp env k))]
    [if-exp (test-exp then-exp)
        (if (eval-exp test-exp env k)
            (eval-exp then-exp env k))]
    [lambda-exp (vars bodies)
        (closure vars bodies env k)]
    [let-exp (var-binds bodies)
        (let ([new-env (extend-env (get-vars var-binds) (eval-rands (get-exps var-binds) env) env k)])
            (eval-bodies bodies new-env k))]
    [while-exp (test bodies)
        (let ([while-helper (lambda ()
                                (if (eval-exp test env k)
                                    (begin 
                                        (eval-bodies bodies env k)
                                        (eval-exp exp env k))))])
            (while-helper))]
    
    [set!-exp (var val-exp) 
        (set-ref! (apply-env-ref env var (lambda (x) x) ; procedure to call if id is in the environment 
           (lambda () (apply-env-ref global-env var 
                                                    (lambda (x) x)
                                                    (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
		                                                    "variable found in environment: ~s"
			                                                var)) k)) k) (eval-exp val-exp env k) k)]
    [define-exp (var body)
        (set! global-env (extend-env (list var) (list (eval-exp body (empty-env))) global-env k))]
    [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

;TODO Need to Test
(define eval-rands
  (lambda (rands env k)
      (eval-exp rands 
      (map-cps (lambda (x) (eval-exp x env (init-k))) rands k))))

; apply a proc and its arguments
;TODO
(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [closure (vars bodies env k) 
            (let* ([vars-args (flatten-vars-args vars args)]
                    [vars (car vars-args)]
                    [args (cadr vars-args)])
                (eval-bodies bodies (extend-env vars args env k) k))]
      [prim-proc (op) (apply-prim-proc op args)]
			; You will add other cases
      [else (eopl:error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

; gets a flat list of the vars and the corresponding amount of args 
; if more args than vars then the last set of args is put into a list
;TODO
(define flatten-vars-args 
    (lambda (vars args)
        (cond 
            [(null? vars) (if (not (null? args)) 
                                (eopl:error 'flatten-vars-args "lamnbda expexted no args, but got ~s" args)
                                (list '() '()))]
            [(symbol? vars) (list (list vars) (list args))] 
            [(symbol? (car vars)) (if (and (null? (cdr vars)) (not (null? (cdr args))))
                                    (eopl:error 'flatten-vars-args "incorrect amount of arguments to procedure ~s" args)
                                    (if (symbol? (cdr vars))
                                        (if (null? (cdr args))
                                            (list (list (car vars) (cdr vars)) (list (car args) '()))
                                            (list (list (car vars) (cdr vars)) (cons (car args) (list (cdr args)))))
                                        (let ([final-vars-args (flatten-vars-args (cdr vars) (cdr args))])
                                          (list (cons (car vars) (car final-vars-args)) (cons (car args) (cadr final-vars-args))))))])))
;TODO
(define eval-bodies 
    (lambda (bodies env k)
         (if (null? (cdr bodies))
            (eval-exp (car bodies) env k)
            (begin (eval-exp (car bodies) env k)
                (eval-bodies (cdr bodies) env k))))) 


(define *prim-proc-names* '(+ - * / add1 sub1 zero? not = < > <= >= cons list null? assq eq? eqv? append list-tail
                            equal? atom? length list->vector list? pair? procedure? vector->list vector 
                            make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set! member
                            display newline car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr apply map quotient))

(define *prim-proc-zero '(newline))

(define *prim-proc-one '(add1 sub1 zero? not car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr null? 
                                atom? length list->vector vector? list? pair? procedure? vector->list number? symbol?))

(define *prim-proc-two '(= < > <= >= cons eq? equal? eqv? make-vector vector-ref set-car! set-cdr! assq member quotient list-tail))

(define *prim-proc-three '(vector-set!))

(define *prim-proc-multiple '(+ - * / list vector apply map append))

(define init-env       
  (extend-env  *prim-proc-names*   
     (map prim-proc      
          *prim-proc-names*)
     (empty-env) (init-k)))

(define global-env  
    init-env)

(define reset-global-env (lambda () (set! global-env init-env)))

; TODO
(define apply-prim-proc
  (lambda (prim-proc args)
     ;error checking 
     (cond 
        [(member prim-proc *prim-proc-zero) (if (not (null? args))
                                                (eopl:error 'apply-prim-proc "incorrect amount of arguments to ~s" prim-proc)
                                                (newline))]
         [(member prim-proc *prim-proc-one) (if (not (equal? 1 (length args)))
                                                (eopl:error 'apply-prim-proc "incorrect amount of arguments to ~s" prim-proc)
                                                (case prim-proc 
                                                    [(add1) (+ (1st args) 1)]
                                                    [(sub1) (- (1st args) 1)]
                                                    [(zero?) (zero? (1st args))]
                                                    [(not) (not (1st args))]
                                                    [(car) (car (1st args))]
                                                    [(cdr) (cdr (1st args))]
                                                    [(caar) (caar (1st args))]
                                                    [(cadr) (cadr (1st args))]
                                                    [(cdar) (cdar (1st args))]
                                                    [(cddr) (cddr (1st args))]
                                                    [(caaar) (caaar (1st args))]
                                                    [(caadr) (caadr (1st args))]
                                                    [(cadar) (cadar (1st args))]
                                                    [(caddr) (caddr (1st args))]
                                                    [(cdaar) (cdaar (1st args))]
                                                    [(cdadr) (cdadr (1st args))]
                                                    [(cddar) (cddar (1st args))]
                                                    [(cdddr) (cdddr (1st args))]
                                                    [(null?) (null? (1st args))]
                                                    [(atom?) (atom? (1st args))]
                                                    [(length) (length (1st args))]
                                                    [(list->vector) (list->vector (1st args))]
                                                    [(vector?) (vector? (1st args))]
                                                    [(list?) (list? (1st args))]
                                                    [(pair?) (pair? (1st args))]
                                                    [(procedure?) (proc-val? (1st args))]
                                                    [(vector->list) (vector->list (1st args))]
                                                    [(number?) (number? (1st args))]
                                                    [(symbol?) (symbol? (1st args))]
                                                    [else (eopl:error 'apply-prim-proc "programming error one")]))]
        [(member prim-proc *prim-proc-two) (if (not (equal? 2 (length args)))
                                                (eopl:error 'apply-prim-proc "incorrect amount of arguments to ~s" prim-proc)
                                                (case prim-proc
                                                    [(=) (= (1st args) (2nd args))]
                                                    [(<) (< (1st args) (2nd args))]
                                                    [(>) (> (1st args) (2nd args))]
                                                    [(<=) (<= (1st args) (2nd args))]
                                                    [(>=) (>= (1st args) (2nd args))]
                                                    [(cons) (cons (1st args) (2nd args))]
                                                    [(eq?) (eq? (1st args) (2nd args))]
                                                    [(equal?) (equal? (1st args) (2nd args))]
                                                    [(eqv?) (eqv? (1st args) (2nd args))]
                                                    [(make-vector) (make-vector (1st args) (2nd args))]
                                                    [(vector-ref) (vector-ref (1st args) (2nd args))]
                                                    [(set-car!) (set-car! (1st args) (2nd args))]
                                                    [(set-cdr!) (set-cdr! (1st args) (2nd args))]
                                                    [(member) (member (1st args) (2nd args))]
                                                    [(list-tail) (list-tail (1st args) (2nd args))]
                                                    [(quotient) (quotient (1st args) (2nd args))]
                                                    [(assq) (assq (1st args) (2nd args))]
                                                    [else (eopl:error 'apply-prim-proc "programming error two")]))]
        [(member prim-proc *prim-proc-three) (if (not (equal? 3 (length args)))
                                                (eopl:error 'apply-prim-proc "incorrect amount of arguments to ~s" prim-proc)
                                                (vector-set! (1st args) (2nd args) (3rd args)))]
        [(member prim-proc *prim-proc-multiple) (case prim-proc
                                                    [(+) (apply + args)]
                                                    [(-) (apply - args)]
                                                    [(*) (apply * args)]
                                                    [(/) (apply / args)]
                                                    [(list) (apply list args)]
                                                    [(vector) (apply vector args)]
                                                    [(append) (apply append args)]
                                                   ;; [(apply) (apply (lambda (ls) (apply-prim-proc (unparse-exp (car args)) ls)) (cdr args))]
                                                    [(apply) (if (equal? 'closure (caar args)) 
                                                                        (apply-proc (car args) (cadr args))
                                                                        (apply (lambda (ls) (apply-prim-proc (unparse-exp (car args)) ls)) (cdr args)))]
                                                    [(map) (if (equal? 'closure (caar args))
                                                            (map (lambda (x) (apply-proc (car args) (list x))) (cadr args))
                                                            (map (lambda (x) (apply-prim-proc (unparse-exp (car args)) (list x))) (cadr args)))]
                                                    [else (eopl:error 'apply-prim-proc "programming error multiple")])])))
        

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (syntax-expand (parse-exp (read))))])
      ;; TODO: are there answers that should display differently
        (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.


(define eval-one-exp 
  (lambda (x) (un-closure (top-level-eval (syntax-expand (parse-exp x))))))


(define un-closure 
    (lambda (x)
        (cond 
            [(not (pair? x)) x]
            [(null? x) x]
            [(list? x) (if (equal? (car x) 'closure)
                            '<procedure>
                            (cons (un-closure (car x)) (un-closure (cdr x))))])))
    




