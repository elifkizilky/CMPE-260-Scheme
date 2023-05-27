#lang racket

; example if - else if - else statement, delete later
(define my-function
  (lambda (x)
    (if (< x 0)
        (display "x is negative")
        (if (> x 0)
            (display "x is positive")
            (display "x is zero")))))


;(if (>= x 0)
 ;   (if (< x 10)
  ;      (display "x is between 0 and 9")
   ;     (display "x is greater than or equal to 10"))
    ;(display "x is less than 0"))


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
(define put (lambda (state var val) 
(if (list? val) (hash-set state var val) (hash-set state var (get state val)))
))


; 4. := (15 points)
(define := (lambda (var val-expr state) 
 (let* ([value (hash-ref (eval-expr val-expr state) '-r)]
        [new-state (put state var value)])
   (put new-state '-r value))  
))
; var a da eval-expr uygulamak?

; 5. if: (15 points)
(define if:
  (lambda (test-expr then-exprs else-exprs state)
    (let* ([result (eval-expr test-expr state)]
           [condition (hash-ref result '-r)])
           (if condition
              (eval-exprs then-exprs state)
              (eval-exprs else-exprs state)))))


; 6. while: (15 points)
;(define while: 0)
(define while:
  (lambda (test-expr body-exprs state)
    (let* ([state (eval-expr test-expr state)]
           [condition (hash-ref state '-r)])
      (if condition
          (let ([new-state (evaluate-body body-exprs state)])
            (while: test-expr body-exprs new-state))
          state))))

(define evaluate-body
  (lambda (body-exprs state)
    (cond
      [(null? body-exprs) state]
      [else
       (let ([new-state (eval-expr (car body-exprs) state)])
         (evaluate-body (cdr body-exprs) new-state))])))

; 7. func (15 points)

(define func  (lambda (params body-exprs state)
    (let ([my-func (lambda args
                     (let ([new-state (bind-together params args state)])
                       (get (eval-exprs body-exprs new-state ) '-r))
        )])
      (put state '-r my-func)
      )))

;(define func  (lambda (params body-exprs state)
 ;   (put state '-r (lambda args
  ;      (eval-exprs body-exprs (bind-together params args state))))))


;first, map list list1 list2 --> '((a 1) (b 2) (c 3) (d 4))
;second, (apply append (map list list1 list2)) --> '(a 1 b 2 c 3 d 4)
;third, (apply hash (apply append (map list list1 list2))) --> '#hash((a . 1) (b . 2) (c . 3) (d . 4))

;(define bind-together (lambda (param arg)
 ;    (apply hash (apply append (map list list1 list2))
;)))


(define bind-together (lambda (param arg state)
  (if (null? param)
      state
      (put (bind-together (cdr param) (cdr arg) state) (car param) (car arg)))  
   ))


(define hash-to-list
  (lambda (list)
    (map (lambda (hash) (hash-ref hash '-r)) list)))


(define map-eval
  (lambda (list state)
   (hash-to-list (map (lambda (expr) (eval-expr expr state)) list))))

; 8. eval-expr (20 points)
(define eval-expr (lambda (expr state) 
    (if (list? expr) ; expression is a list
       (cond  ((eq? (car expr) ':=) (:= (cadr expr) (caddr expr) state))
               ((eq? (car expr) 'if:) (if: (cadr expr) (caddr expr) (cadddr expr) state))
               ((eq? (car expr) 'while:) (while: (cadr expr) (caddr expr) state))
               ((eq? (car expr) 'func) (func (cadr expr) (caddr expr) state ))
               ((eq? (car expr) 'lambda)(hash-set state '-r (eval expr)))
               ((procedure? (get (eval-expr (car expr) state) '-r))  (hash-set state '-r (apply (get (eval-expr (car expr) state) '-r) (map-eval (cdr expr) state)))); map-eval olacak
               (else hash-set state '-r expr )) ; just a list
       (put state '-r (get state expr))   
)))



; 9 eval-exprs (5 points)
(define eval-exprs (lambda (exprs state) 
    (foldl eval-expr state exprs)))

