; elif kizilkaya
; 2018400108
; compiling: yes
; complete: yes
#lang racket

(provide (all-defined-out))

; read and parse the input file
(define parse (lambda (input-file)
        (letrec (
            [input-port (open-input-file input-file)]
            [read-and-combine (lambda ()
                (let ([line (read input-port)])
                    (if (eof-object? line)
                        '()
                        (append `(,line) (read-and-combine))
                    )
                )
            )]
            )
            (read-and-combine)
        )
    )
)
(define create-hash (lambda (vars values)
        (letrec (
            [create-hash-iter (lambda (vars values hash)
                (if (null? vars)
                    hash
                    (create-hash-iter (cdr vars) (cdr values) (hash-set hash (car vars) (car values)))
                )
            )]
            )
            (create-hash-iter vars values (hash))
        )
    )
)

(define add-to-hash (lambda (old-hash new-hash)
        (foldl (lambda (key hash) (hash-set hash key (hash-ref new-hash key)))
            old-hash
            (hash-keys new-hash)
        )
    )
)

(define eval-program (lambda (program-str)
        (get (eval-exprs (parse program-str) empty-state) '-r)
    )
)

; solution starts here
; 1. empty-state (5 points)
(define empty-state (hash))

; 2. get (5 points)
(define get (lambda (state var)
    (if (hash-has-key? state var) ;if variable in the hash table, evaluate it
        (hash-ref state var) ; (eval (hash-ref state var))
        (eval var) ; if not, just evaluate the variable
    )))


; 3. put (5 points)
; if it is a list put the val; otherwise get the value and put it to state
(define put (lambda (state var val) 
(if (list? val) (hash-set state var val) (hash-set state var (get state val)))
))


; 4. := (15 points)
(define := (lambda (var val-expr state) 
 (let* ([value (hash-ref (eval-expr val-expr state) '-r)] ;eval-expr val-expr
        [new-state (put state var value)]) ;put it value in state
   (put new-state '-r value))  ;update '-r value 
))


; 5. if: (15 points)
(define if:
  (lambda (test-expr then-exprs else-exprs state)
    (let* ([result (eval-expr test-expr state)] ; eval-expr test-expr
           [condition (hash-ref result '-r)]) ; get the condition from the result above
           (if condition 
              (eval-exprs then-exprs state)
              (eval-exprs else-exprs state)))))


; 6. while: (15 points)
(define while:
  (lambda (test-expr body-exprs state)
    (let* ([state (eval-expr test-expr state)] ;eval-expr test-expr
           [condition (hash-ref state '-r)]) ;get the condition from above
      (if condition
          (let ([new-state (evaluate-body body-exprs state)])
            (while: test-expr body-exprs new-state)) ; if condition true evaluate body and make recursive with new state
          state)))) ; if condition false, return the current state

(define evaluate-body
  (lambda (body-exprs state)
    (cond
      [(null? body-exprs) state] ; if at the end of the body-exprs return state
      [else
       (let ([new-state (eval-expr (car body-exprs) state)]) ;evaluate body with new states recursively
         (evaluate-body (cdr body-exprs) new-state))])))

; 7. func (15 points)
(define func  (lambda (params body-exprs state)
    (let ([my-func (lambda args ; create a function
                     (let ([new-state (bind-together params args state)]) ; bind params and args
                       (get (eval-exprs body-exprs new-state ) '-r)) ; evaluate the body of if
        )])
      (put state '-r my-func) ;put the function in '-r
      )))

 ; helper function for func --> it binds the param and arg and puts them in the state
(define bind-together (lambda (param arg state)
  (if (null? param)
      state
      (put (bind-together (cdr param) (cdr arg) state) (car param) (car arg)))  
   ))

; helper function for map-eval --> it takes list of hashes as parameter
; and takes the '-r values of the hashes and makes them a list
(define hash-to-list
  (lambda (list)
    (map (lambda (hash) (hash-ref hash '-r)) list)))

 ; helper function for eval-expr, same functionality in description
 ; except state in the end
 ; after eval-expr in each expr in list, it creates a list of hashes,
 ; so I need hash-to-list
(define map-eval
  (lambda (list state) 
   (hash-to-list (map (lambda (expr) (eval-expr expr state)) list))))

; 8. eval-expr (20 points)
; eval the expression according to conditions given in the description
(define eval-expr (lambda (expr state) 
    (if (list? expr) ; expression is a list
       (cond  ((eq? (car expr) ':=) (:= (cadr expr) (caddr expr) state))
               ((eq? (car expr) 'if:) (if: (cadr expr) (caddr expr) (cadddr expr) state))
               ((eq? (car expr) 'while:) (while: (cadr expr) (caddr expr) state))
               ((eq? (car expr) 'func) (func (cadr expr) (caddr expr) state ))
               ((eq? (car expr) 'lambda)(hash-set state '-r (eval expr)))
               ((procedure? (get (eval-expr (car expr) state) '-r))  (hash-set state '-r (apply (get (eval-expr (car expr) state) '-r) (map-eval (cdr expr) state))))
               (else hash-set state '-r expr )) ; just a list
       (put state '-r (get state expr))   
)))



; 9 eval-exprs (5 points)
(define eval-exprs (lambda (exprs state) 
    (foldl eval-expr state exprs)))

