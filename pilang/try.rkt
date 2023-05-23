#lang racket

(define list-to-hash
  (lambda (lst proc)
    (define iter
      (lambda (remaining result)
        (cond
          [(null? remaining) result]
          [else
           (let* ([current (car remaining)]
                  [value (proc current)])
             (iter (cdr remaining) (hash-set result current value)))])))

    (iter lst (hash))))

(define square (lambda(n)
  (* n n)))

(define input-list '(1 2 3 4 5))
(define result-hash (list-to-hash input-list square))

(define source-hash (hash 'a 1 'b 2 'c 3))
(define target-hash (hash 'd 4 'e 5))

(hash-map source-hash
          (lambda (key value)
            (hash-set target-hash key value)))

(define empty-state (hash))

(define get (lambda (state var) 
    (if (hash-has-key? state var) ;if variable in the hash table, evaluate it
        (eval (hash-ref state var))
        (eval var) ; if not, just evaluate the variable
    )))


(define put (lambda (state var val) 
(hash-set state var (get state val))
))

(define put2 (lambda (state var val) 
(let* ([value (get state val)])
      (hash-set state var value))
))


(define if:
  (lambda (test-expr then-exprs else-exprs state)
    (let* ([result (eval-expr test-expr state)]
           [condition (hash-ref result '-r)])
           (if condition
              (map (lambda (expr) (eval-expr expr state)) then-exprs)
              (map (lambda (expr) (eval-expr expr state)) else-exprs)))))


#|
(define if: (lambda( test-expr then-exprs else-exprs state)
  (let* ([result (get state test-expr)])
    (if result
        (map eval then-exprs)
        (map eval else-exprs)))))


(define if2
  (lambda (test-expr then-exprs else-exprs state)
    (letrec ([result (get state test-expr)]
             [lastResult 2])
      (put state '-r' lastResult))))

; eval-expr yaptıktan sonra resultu -r variable ından çekilebilir ama bu listteki her satır birbiriyle ilgiliyse nasıl olacak?, eval-exprde her variable eklenmeli
(define if3
  (lambda (test-expr then-exprs else-exprs state)
    (let* ([result (get state test-expr)]
           [last-result (if result
                         (map eval then-exprs)
                         (map eval else-exprs))])
     (put2 state '-r (car last-result))
      
     )))
|#
;(displayln target-hash)


(define create-function
  (lambda (state)
    (let ([inner-function
           (lambda (x)
             (+ x 1))])
      (put state '-r inner-function))))


;(define func
 ; (lambda (params body-exprs state)
  ;  (let ([inner-function
   ;        (lambda ()
    ;         (eval body-exprs ))]);evaluate-body: evaluates body-exprs with parameters args list and state
     ; (put state '-r inner-function))))


;(define func
 ; (lambda (params body-exprs state)
  ;  (let ([inner-function
   ;        (lambda (args)
    ;         (eval (car body-exprs)))])
     ; (put state '-r inner-function))))


;(define result-state (func '() '((+ 2 3)) empty-state))
;(define inner-func (get result-state '-r))

;(displayln (inner-func '())) ; Call the inner function with empty arguments

;(define func
 ; (lambda (params body-exprs state)
  ;  (let ([inner-function
   ;        (lambda (args state)
    ;         (evaluate-body body-exprs args state)
     ;        )])
      ;(put state '-r inner-function))))


#|
(define my-lambda
  (lambda (params body-exprs)
    (lambda (args)
      (let loop ([param-list params]
                 [arg-list args]
                 [env (make-base-environment)]) ; Base environment for variables
        (cond
          [(null? param-list) ; All parameters processed, evaluate body expressions
           (eval-body body-exprs env)]
          [else ; Bind current parameter to argument and continue with remaining parameters
           (let ([param (car param-list)]
                 [arg (car arg-list)])
             (loop (cdr param-list) (cdr arg-list)
                   (extend-environment param arg env)))])))))

(define eval-body
  (lambda (body-exprs env)
    (cond
      [(null? body-exprs) '()] ; Base case: no expressions to evaluate, return empty list
      [else
       (let ([current-expr (car body-exprs)])
         (let ([result (eval current-expr env)])
           (cons result (eval-body (cdr body-exprs) env))))])))

(define make-base-environment
  (lambda ()
    (make-hash)))

(define extend-environment
  (lambda (var val env)
    (hash-set env var val)))

;; Example usage
;(define my-add (my-lambda '(x y) '((+ x y))))
;((my-add 5) 3)
|#

(define := (lambda (var val-expr state) 
 (let* ([value (hash-ref (eval-expr val-expr state) '-r)]
        [new-state (put state var value)])
   (put new-state '-r value))  
))


;(define := (lambda (var val-expr state) 
 ;   (put (put state var (hash-ref (eval-expr val-expr state) '-r)) '-r (get state val-expr))
;))


(define map-eval
  (lambda (list state)
   (hash-to-list (map (lambda (expr) (eval-expr expr state)) list))))




(define eval-expr (lambda (expr state) 
    (if (list? expr) ; expression is a list
       (cond  ((eq? (car expr) ':=) (eval (append expr (list state))))
               ((eq? (car expr) 'if:) (eval (append expr (list state))))
               ((eq? (car expr) 'while:) (eval (append expr (list state))))
               ((eq? (car expr) 'func) (eval (append expr (list state))))
               ((eq? (car expr) 'lambda)(hash-set state '-r (eval expr)))
               ((symbol? (car expr))  (hash-set state '-r (eval (map-eval expr state)))); map-eval olacak
               (else (display "invalid operation")))
       (put state '-r (get state expr))   
)))

(define hash-to-list
  (lambda (list)
    (map (lambda (hash) (hash-ref hash '-r)) list)))




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


(define eval-exprs
  (lambda (exprs state)
    (foldl eval-expr state exprs)))


(define func
  (lambda (params body-exprs state)
    (put state '-r (lambda args
      (eval-exprs body-exprs args state)))))

(define my-lambda
  (lambda (params body-exprs state)
    (lambda args
      (let ([state (foldl (lambda (param arg state)
                            (put state param arg))
                          empty-state
                          params
                          args)])
        (eval-exprs body-exprs state)))))