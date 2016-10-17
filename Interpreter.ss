;:  Single-file version of the interpreter.
;; Easier to submit to server, probably harder to use in the development process
;; Brendan Goldacker and Cameron Metzger

(load "C:/Users/goldacbj/Google Drive/Documents/CSSE/CSSE304/chez-init.ss") 
;;(load "C:/Users/metzgecj/Desktop/Year3/PLC/chez-init.ss") 

;-------------------+
;                   |
;    DATATYPES      |
;                   |
;-------------------+

; parsed expression

; returns true if x is a literal (anything that evaluates to itself)
(define literal? 
    (lambda (x)
        (or (number? x) (string? x) (boolean? x) (vector? x) (null? x) (and (list? x) (equal? 'quote (car x)) (or (list? (cadr x)) (symbol? (cadr x)) (vector? (cadr x)))))))


; datatypes: lambda-exp, variable, lit-exp, if-exp, if-else-exp, let-exp, letrec-exp, let*-exp, set!-exp
(define-datatype expression expression?
    [var-exp (id symbol?)]
    [lambda-exp
        (vars (lambda (x) (or  (and (pair? x) (not (list? x))) (null? x) (symbol? x) ((list-of symbol?) x))))
        (body (list-of expression?))]
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
    [cond-exp 
        (cases (list-of (lambda (all-cases) (list-of (lambda (single-case) 
                            (and ((list-of expression?) (car x)) ((list-of expression?) (cadr x))))))))])

	
	

;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name in-prim?)]
   [closure
   (vars (lambda (x) (or  (and (pair? x) (not (list? x))) (null? x) (symbol? x) ((list-of symbol?) x))))
   (bodies (list-of expression?))
   (env environment?)])

(define in-prim? 
    (lambda (x)
        (if (member x *prim-proc-names*)
            #t
            (proc-val? x))))
	 
	

;-------------------+
;                   |
;    PARSER         |
;                   |
;-------------------+


; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.
        

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
                [(eqv? (car datum) 'let)
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
                    (cond-exp (map (lambda (x) (list (parse-exp (car x)) (parse-exp (cadr x)))) (cdr datum)))]
                [(eqv? (car datum) 'and)
                    (and-exp (map parse-exp (cdr datum)))]
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
        [(eqv? (car datum) 'cond-exp) (apply list 'cond (map (lambda (case) (cons (unparse-exp (car case)) 
                                                         (map unparse-exp (cdr case)))) (cadr datum)))]
        [(eqv? (car datum) 'and-exp) (apply list 'and (map unparse-exp (2nd datum)))]
        [else #f])])))








;-------------------+
;                   |
;   ENVIRONMENTS    |
;                   |
;-------------------+





; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

(define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      (empty-env-record ()
        (fail))
      (extended-env-record (syms vals env)
	(let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
	      (succeed (list-ref vals pos))
	      (apply-env env sym succeed fail)))))))








;-----------------------+
;                       |
;   SYNTAX EXPANSION    |
;                       |
;-----------------------+



; To be added later

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
            [letrec-exp (var-binds body) (letrec-exp (map (lambda (var-bind) (list (car var-bind) (syntax-expand (cadar var-binds)))) var-binds) (map syntax-expand body))]
            [let*-exp (var-binds body) (letrec ([parse-let* (lambda (var-binds) 
                                                                (if (null? (cdr var-binds)) 
                                                                    (app-exp (lambda-exp (list (caar var-binds)) (map syntax-expand body)) (list (syntax-expand (cadar var-binds))))
                                                                    (app-exp (lambda-exp (list (caar var-binds)) (list (parse-let* (cdr var-binds)))) (list (syntax-expand (cadar var-binds))))))])
                                                  (parse-let* var-binds))]  
            [set!-exp (new-val value)
                (set!-exp new-val (syntax-expand value))]
            [and-exp (tests)
                (letrec ([parse-and (lambda (tests) 
                                        (if (null? (cdr tests))
                                            (syntax-expand (car tests))
                                            (if-else-exp (syntax-expand (car tests))
                                                (parse-and (cdr tests))
                                                (parse-exp '#f))))])
                        (parse-and tests))]
            [cond-exp (cases)
                (letrec ([parse-cond (lambda (cases)
                                        (if (equal? 'else (cadaar cases))
                                            (syntax-expand (cadar cases))
                                            (if (null? (cdr cases))
                                                (if-exp (syntax-expand (caar cases))
                                                        (syntax-expand (cadar cases)))
                                                (if-else-exp (syntax-expand (caar cases))
                                                    (syntax-expand (cadar cases))
                                                    (parse-cond (cdr cases))))))])
                    (parse-cond cases))]                                        
            [else (eopl:error 'syntax-expand "not an expression ~s" exp)])))


(define get-var-binds
    (lambda (let-bindings)
            (map car let-bindings)))

(define get-binds 
    (lambda (let-bindings)
            (map cadr let-bindings)))



;-------------------+
;                   |
;   INTERPRETER     |
;                   |
;-------------------+



; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form init-env)))

;helper functions for let-exp
(define get-vars
    (lambda (ls)
        (map car (unparse-exp ls))))

(define get-exps
    (lambda (ls)
        (map cadr ls)))

; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum)
                (if (and (list? datum) (eqv? (car datum) 'quote))
                    (cadr datum)
                    datum)]
      [var-exp (id)
				(apply-env env id; look up its value.
      	   (lambda (x) x) ; procedure to call if id is in the environment 
           (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
		          "variable not found in environment: ~s"
			   id)))] 
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands rands env)])
          (apply-proc proc-value args))]
     [if-else-exp (test-exp then-exp else-exp)
        (if (eval-exp test-exp env)    ;;need to add enviornments
            (eval-exp then-exp env)    
            (eval-exp else-exp env))]
    [if-exp (test-exp then-exp)
        (if (eval-exp test-exp env)
            (eval-exp then-exp env))]
    [lambda-exp (vars bodies)
        (closure vars bodies env)]
    [let-exp (var-binds bodies)
        (let ([new-env (extend-env (get-vars var-binds) (eval-rands (get-exps var-binds) env) env)])
            (eval-bodies bodies new-env))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env)) rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [closure (vars bodies env) 
            (let* ([vars-args (flatten-vars-args vars args)]
                    [vars (car vars-args)]
                    [args (cadr vars-args)])
                (eval-bodies bodies (extend-env vars args env)))]
      [prim-proc (op) (apply-prim-proc op args)]
			; You will add other cases
      [else (eopl:error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define flatten-vars-args 
    (lambda (vars args)
        (cond 
            [(null? vars) (if (not (null? args)) 
                                (eopl:error 'flatten-vars-args "lamnbda expexted no args, but got ~s" args)
                                (list '() '()))]
            [(symbol? vars) (list (list vars) (list args))]
            [(symbol? (car vars)) (if (and (null? (cdr vars)) (not (null? (cdr args))))
                                    (eopl:error 'flatten-vars-args "incorrect amount of arguments to procedure ~s" args)
                                    (if (not (pair? (cdr vars)))
                                        (if (null? (cdr args))
                                            (list (list (car vars)) args)
                                            (list (list (car vars) (cdr vars)) (cons (car args) (list (cdr args)))))
                                        (let ([final-vars-args (flatten-vars-args (cdr vars) (cdr args))])
                                          (list (cons (car vars) (car final-vars-args)) (cons (car args) (cadr final-vars-args))))))])))

(define eval-bodies 
    (lambda (bodies env)
        (if (null? (cdr bodies))
            (eval-exp (car bodies) env)
            (begin (eval-exp (car bodies) env)
                (eval-bodies (cdr bodies) env)))))

(define *prim-proc-names* '(+ - * / add1 sub1 zero? not = < > <= >= cons list null? assq eq? 
                            equal? atom? length list->vector list? pair? procedure? vector->list vector 
                            make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set! 
                            display newline car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr apply map))

(define *prim-proc-zero '(newline))

(define *prim-proc-one '(add1 sub1 zero? not car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr null? 
                                atom? length list->vector vector? list? pair? procedure? vector->list number? symbol?))

(define *prim-proc-two '(= < > <= >= cons eq? equal? make-vector vector-ref set-car! set-cdr! assq))

(define *prim-proc-three '(vector-set!))

(define *prim-proc-multiple '(+ - * / list vector apply map))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

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
                                                    [(make-vector) (make-vector (1st args) (2nd args))]
                                                    [(vector-ref) (vector-ref (1st args) (2nd args))]
                                                    [(set-car!) (set-car! (1st args) (2nd args))]
                                                    [(set-cdr!) (set-cdr! (1st args) (2nd args))]
                                                    [else (eopl:error 'apply-prim-proc "programming error two")]))]
        [(member prim-proc *prim-proc-three) (if (not (equal? 3 (length args)))
                                                (eopl:error 'apply-prim-proc "incorrect amount of arguments to ~s" prim-proc)
                                                (vector-set! (1st args) (2nd args) (3rd args)))]
        [(member prim-proc *prim-proc-multiple) (if (null? args)
                                                (eopl:error 'apply-prim-proc "incorrect amount of arguments to ~s" prim-proc)
                                                (case prim-proc
                                                    [(+) (apply + args)]
                                                    [(-) (apply - args)]
                                                    [(*) (apply * args)]
                                                    [(/) (apply / args)]
                                                    [(list) (apply list args)]
                                                    [(vector) (apply vector args)]
                                                    [(apply) (apply (lambda (ls) (apply-prim-proc (unparse-exp (car args)) ls)) (cdr args))]
                                                    [(map) (map (lambda (x) (apply-prim-proc (unparse-exp (car args)) (list x))) (cadr args))]
                                                    [else (eopl:error 'apply-prim-proc "programming error multiple")]))])))
        

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (un-closure (top-level-eval (syntax-expand (parse-exp (read)))))])
      ;; TODO: are there answers that should display differently
        (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))



(define un-closure 
    (lambda (x)
        (cond 
            [(not (pair? x)) x]
            [(null? x) x]
            [(list? x) (if (equal? (car x) 'closure)
                            '<procedure>
                            (cons (un-closure (car x)) (un-closure (cdr x))))])))
    





