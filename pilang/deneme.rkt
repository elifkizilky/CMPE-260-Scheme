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


(define if_needed: (lambda( test-expr then-exprs else-exprs state)
  (let* ([result (eval-expr test-expr state)]
         [r (get-value 'r result)])
    (if r
        (eval-expr-list then-exprs state)
        (eval-expr-list else-exprs state)))))

(define if: (lambda( test-expr then-exprs else-exprs state)
  (let* ([result (get state test-expr)])
    (if result
        (map eval then-exprs)
        (map eval else-exprs)))))


(define if:
  (lambda (test-expr then-exprs else-exprs state)
    (let* ([result (get state test-expr)]
           [last-result (if result
                         (map eval then-exprs)
                         (map eval else-exprs))]
                          (put state '-r' last-result))
     )))


(define if2:
  (lambda (test-expr then-exprs else-exprs state)
    (let* ([result (eval-expr test-expr state)]
           [last-result (if result
                          (eval-expr-list then-exprs state)
                          (eval-expr-list else-exprs state))])
      (put state '-r' last-result))))




(define while:
  (lambda (test-expr body-exprs state)
    (let ([condition (get state test-expr)])
      (if condition
          (let ([new-state (eval-body body-exprs state)])
            (while: test-expr body-exprs new-state))
          state))))

(define eval-body
  (lambda (body-exprs state)
    (cond
      [(null? body-exprs) state]
      [else
       (let ([new-state (eval (car body-exprs) state)])
         (eval-body (cdr body-exprs) new-state))])))


        ; ((get (func '(x y) '((:= x (* x x)) (:= y (- y x))) empty-state) '-r) 5 10)



(define eval-expr
  (lambda (expr state)
    (if (list? expr) ; if expression is a list
        (let ((operation (car expr)))
          (cond
            ((eq? operation ':=)
             (eval ((append lst (list state)))))
            ((eq? operation 'if:)
             (let* ((test-expr (car args))
                    (then-exprs (cadr args))
                    (else-exprs (caddr args))
                    (result (get state test-expr)))
               (if result
                   (eval-expr then-exprs state)
                   (eval-expr else-exprs state))))
            ((eq? operation 'while:)
             (let* ((test-expr (car args))
                    (body-exprs (cdr args))
                    (result (get state test-expr)))
               (if result
                   (begin
                     (eval-expr body-exprs state)
                     (eval-expr expr state))
                   state)))
            ((eq? operation 'func)
             (let* ((params (car args))
                    (body-exprs (cdr args))
                    (func (create-function params body-exprs state)))
               (put state '-r func)
               state))
            ((symbol? operation)
             (let ((arg-values (map-eval args state)))
               (apply operation arg-values)))
            (else (display "Invalid operation"))))
      (put state '-r expr)
      state)))



      ;(define map-eval
 ; (lambda (lst state)
  ;  (let* ((expr-results (map (lambda (expr) (eval-expr expr state)) lst))
   ;        (last-result (last lst))
    ;       (value (eval-expr last-result state)))
     ; (list expr-results (put state '-r 1)))))
;  (list expr-results (put state '-r value)))))



; deneme kodu
(define (my-cond x)
    (if (< x  10)
    (cond ((= x 1) (display "Condition 1 is true"))
        ((= x 2) (display "Condition 2 is true"))
        ((= x 3) (display "Condition 3 is true"))
        ((= x 4) (display "Condition 4 is true"))
        (else (display "No condition is true")))
        (display "x is bigger than 10")))


(define last (lambda (lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst)))))


;  (cons    (cons (eval (car lst))))


(define := (lambda (var val-expr state) 
    (put (put state var (get state val-expr)) '-r (get state val-expr))
))



(define if:
  (lambda (test-expr then-exprs else-exprs state)
    (let* ([result (get state test-expr)]
           [last-result (if result
                         (map eval then-exprs)
                         (map eval else-exprs))])
     (put state '-r (car last-result)))))




(define while:
  (lambda (test-expr body-exprs state)
    (let* ([result (eval-expr test-expr state)]
           [condition (hash-ref result '-r)])
      (if condition
          (let ([new-state (eval-body body-exprs state)])
            (while: test-expr body-exprs new-state))
          state))))

(define eval-body
  (lambda (body-exprs state)
    (cond
      [(null? body-exprs) state]
      [else
       (let ([new-state (eval-expr (car body-exprs) state)])
         (eval-body (cdr body-exprs) new-state))])))



(define while:
  (lambda (test-expr body-exprs state)
    (let* ([state (eval-expr test-expr state)]
           [condition (hash-ref state '-r)])
      (if condition
          (let ([new-state (eval-body body-exprs state)])
            (while: test-expr body-exprs new-state))
          state))))

(define eval-body
  (lambda (body-exprs state)
    (cond
      [(null? body-exprs) state]
      [else
       (let ([new-state (eval-expr (car body-exprs) state)])
         (eval-body (cdr body-exprs) new-state))])))
