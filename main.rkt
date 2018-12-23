#lang racket

(provide pred-p single-digit-p single-alphabet-p seq alt
         epsilon-p zero-or-more one-or-more whitespace-p
         number-p identifier-p variable-p term-p expression-p assignment-p)

(define-struct gnode (sym list) #:transparent)

(define-struct ident (str) #:transparent)

(define-struct num (val) #:transparent)

(define (combine-cc char1 char2)
  (list->string (list char1 char2)))

(define (combine-sc str char)
  (list->string (append (string->list str)
                        (list char))))

(define (combine-cs char str)
  (list->string (cons char (string->list str))))

(define (combine-ss str1 str2)
  (list->string (append (string->list str1)
                        (string->list str2))))

(define (pred-p p)
  (lambda(str)
    (cond ((= (string-length str) 0) `fail)
          (else
           (define x (string-ref str 0))
           (cond ((p x) (cons x (substring str 1)))
                 (else `fail))))))

(define single-digit-p
  (lambda (str)
    ((pred-p char-numeric?) str)))

(define single-alphabet-p
  (lambda (str)
    ((pred-p char-alphabetic?) str)))

(define (seq p1 p2 f)
  (lambda (str)
    (define temp (p1 str))
    (cond ((eq? temp `fail) `fail)
          (else
           (begin
             (define m (car temp))
             (define str2 (cdr temp))
             (define temp2 (p2 str2))
             (cond ((eq? temp2 `fail) `fail)
                   (else
                    (begin
                      (define str3 (cdr temp2))
                      (define n (car temp2))
                      (cons (f m n) str3)))))))))

(define (alt p1 p2)
  (lambda (str)
    (define temp (p1 str))
    (cond ((eq? temp `fail) (p2 str))
          (else temp))))

(define epsilon-p (lambda (str) (cons "" str)))

(define (zero-or-more p f)
  (lambda (str)
    (define temp (p str))
    (cond ((eq? temp `fail) (epsilon-p str))
          (else
           (define m (car temp))
           (define str2 (cdr temp))
           (define temp2 ((zero-or-more p f) str2))
           (define n (car temp2))
           (define str3 (cdr temp2))
           (cons (f m n) str3)))))

(define (one-or-more p f)
  (lambda (str)
    (define temp (p str))
    (cond ((eq? temp `fail) `fail)
          (else
           (define m (car temp))
           (define str2 (cdr temp))
           (define temp2 ((zero-or-more p f) str2))
           (define n (car temp2))
           (define str3 (cdr temp2))
           (cons (f m n) str3)))))

(define whitespace-p
  (lambda (str)
    (cond ((= (string-length str) 0) `fail)
          (else
           (define x (string-ref str 0))
           (cond ((char-whitespace? x) (whitespace-p (substring str 1)))
                 ((string-suffix? str " ")
                  (whitespace-p (string-trim str)))
                 (else (epsilon-p str)))))))

(define number-p
  (lambda (str)
    (define sp-rem (whitespace-p str))
    (cond ((eq? sp-rem `fail) `fail)
          (else
           (define finstr (cdr sp-rem))
           (define res ((one-or-more single-digit-p combine-cs) finstr))
           (cond ((eq? res `fail) `fail)
                 (else
                  (define x (string->number (car res) 10))
                  (define numstr (num x))
                  (cons numstr (cdr res))))))))

(define identifier-p
  (lambda (str)
    (define sp-rem (whitespace-p str))
    (cond ((eq? sp-rem `fail) `fail)
          (else
           (define penstr (cdr sp-rem))
           (define lchk (single-alphabet-p penstr))    ;letter checker for 1st position
           (cond ((eq? lchk `fail) `fail)
                 (else
                  (define finstr (cdr lchk))
                  (define x (car lchk))
                  (define res
                    ((zero-or-more (alt single-digit-p single-alphabet-p) combine-cs) finstr))
                  (define op (ident (combine-cs x (car res))))
                  (cons op (cdr res))))))))

(define variable-p
  (lambda (str)
    (define temp (identifier-p str))
    (cond
      ((eq? temp `fail) `fail)
      ((not (non-empty-string? (cdr temp))) temp)
      ((eqv? (string-ref (cdr temp) 0) #\[)
       (define str2 (substring (cdr temp) 1))
       (define temp2 (expression-p str2))
       (cond
         ((eq? temp2 `fail) `fail)
         ((not (non-empty-string? (cdr temp2))) `fail)
         ((eqv? (string-ref (cdr temp2) 0) #\])
          (cons (gnode `ARRAY (list (car temp) (car temp2))) (substring (cdr temp2) 1)))
         (else `fail)))
      (else temp))))

(define term-p
  (lambda (str)
    (define temp (number-p str))
    (cond ((not (eq? temp `fail)) temp)
          (else
           (define temp2 (variable-p str))
           (cond ((not (eq? temp2 `fail)) temp2)
                 ((not (non-empty-string? str)) `fail)
                 (else
                  (cond
                    ((eqv? (string-ref str 0) #\()
                     (define temp3 (expression-p (substring str 1)))
                     (cond
                       ((eq? temp3 `fail) `fail)
                       ((not (non-empty-string? (cdr temp3))) `fail)
                       ((eqv? (string-ref (cdr temp3) 0) #\))
                        (cons (car temp3) (substring (cdr temp3) 1)))
                       (else `fail)))
                    (else `fail))))))))

(define expression-p
  (lambda (str)
    (define temp (term-p str))
    (cond ((eq? temp `fail) `fail)
          (else
           (define trimmed (string-trim (cdr temp)))
           (cond
             ((not (non-empty-string? trimmed)) temp)
             ((eqv? (string-ref trimmed 0) #\+)
              (define temp2 (expression-p (substring trimmed 1)))
              (cond ((eq? temp2 `fail) `fail)
                    (else
                     (cons
                      (gnode `PLUS (list (car temp) (car temp2)))
                      (cdr temp2)))))
             (else temp))))))

(define assignment-p
  (lambda (str)
    (define temp (variable-p str))
    (cond ((eq? temp `fail) `fail)
          (else
           (define str1 (string-trim (cdr temp)))
           (cond
             ((not (non-empty-string? str1)) `fail)
             ((eqv? (string-ref str1 0) #\=)
              (define temp2 (expression-p (substring str1 1)))
              (cond ((eq? temp2 `fail) `fail)
                    (else
                     (cons (gnode `ASSIGN (list (car temp) (car temp2))) (cdr temp2)))))
             (else `fail))))))

;; Example Usage
#|
((pred-p (lambda (c) (char=? c #\=))) "=abc") 
((pred-p (lambda (c) (char=? c #\=))) "+abc") 
(single-alphabet-p "a1234") 
(single-digit-p "a1234") 
(single-digit-p "1234") 
((seq single-digit-p single-alphabet-p combine-cc) "1a234") 
((seq single-digit-p single-alphabet-p combine-cc) "a1234") 
((alt single-digit-p single-alphabet-p) "a1234") 
((alt single-digit-p single-alphabet-p) "1a234") 
(epsilon-p "1234") 
((zero-or-more single-digit-p combine-cs) "1234a") 
((one-or-more single-digit-p combine-cs) "1234a") 
(whitespace-p " a12345") 
(number-p "a12345") 
(number-p " a12345") 
(identifier-p "a12345") 
(identifier-p " a12345") 
(variable-p "a12345") 
(variable-p "x[a12345+1]") 
(term-p "(a+(b+c))") 
(expression-p "(a+(b+c))+d[e]") 
(assignment-p "a = 3") 
(assignment-p "a = 3 + i") 
(assignment-p "a[a[l]] = a[3+i]") 
(assignment-p "a[a[l]+j] = a[3+i]")
|#

;; Main execution starts here
;; Note that this parser represents a given expression in the form of a Parse Tree

(assignment-p "a[a[l]] = a[3+i] + b[2] + xyz")
