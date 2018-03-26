#lang racket
(provide pred-p)
(provide single-digit-p)
(provide single-alphabet-p)
(provide seq)
(provide alt)
(provide epsilon-p)
(provide zero-or-more)
(provide one-or-more)
(provide whitespace-p)
(provide number-p)
(provide identifier-p)
(provide variable-p)
(provide term-p)
(provide expression-p)
(provide assignment-p)

(define-struct gnode (sym list) #:transparent)
(define-struct ident (str) #:transparent)
(define-struct num (val) #:transparent)


(define (pred-p p)
  (lambda(string)
    (cond[(zero? (string-length string)) 'fail]
         [(p (string-ref string 0)) (cons (string-ref string 0) (substring string 1))]
         [else 'fail])))

(define single-alphabet-p
  (lambda (str)(cond[(zero? (string-length str)) 'fail]
                    [(or (and
                       (>= (char->integer (string-ref str 0)) 65)
                       (<= (char->integer (string-ref str 0)) 90))
                        (and
                       (>= (char->integer (string-ref str 0)) 97)
                       (<= (char->integer (string-ref str 0)) 122)))
                       (cons (string-ref str 0) (substring str 1))]
                    [else 'fail])))

(define single-digit-p
  (lambda (str)(cond[(zero? (string-length str)) 'fail]
                    [(and
                       (>= (char->integer (string-ref str 0)) 48)
                       (<= (char->integer (string-ref str 0)) 57))
                       (cons (string-ref str 0) (substring str 1))]
                    [else 'fail])))

(define (seq p1 p2 f) (lambda (str)
    (let*[(s1 (p1 str))]
      (if (equal? 'fail s1) 'fail
          (let*[(s2 (p2 (cdr s1)))]
            (if (equal? 'fail s2) 'fail
                (cons (f (car s1) (car s2))
                      (cdr s2))))))))

(define (alt p1 p2)(lambda(str) (cond[(not (equal? 'fail (p1 str))) (p1 str)]
                                     [(not (equal? 'fail (p2 str))) (p2 str)]
                                     [else 'fail])))

(define epsilon-p (lambda (str)(cons "" str)))

(define (zero-or-more p f)
  (lambda (str) ((alt (seq p (zero-or-more p f) f) epsilon-p) str)))

(define (one-or-more p f)
  (lambda (str) (cond[(zero? (string-length (car ((zero-or-more p f) str)))) 'fail]
                     [else ((zero-or-more p f)str)])))

(define (whitespace-p str)
  (cons "" (cdr((zero-or-more (pred-p (lambda (x)(equal? #\space x))) combine-cs) str))))

(define number-p
  (lambda(str)
    (let* [(output
            ((seq whitespace-p (one-or-more single-digit-p combine-cs) combine-ss) str))]
     (cond [(equal? output 'fail) 'fail]
           [else (cons (num (string->number (car output))) (cdr output))]))))
          

(define identifier-p
  (lambda (str)
    (let* [(value
            ((seq single-alphabet-p
             (zero-or-more (alt single-alphabet-p single-digit-p)combine-cs) combine-cs)
                (cdr (whitespace-p str))))]
      (cond [(not (equal? 'fail value)) (cons (ident (car value)) (cdr value))]
            [else 'fail]))))

(define variable-p
   (lambda (str)
     ((seq identifier-p (alt (seq open (seq expression-p close
      (lambda (p q) (gnode 'ARRAY (list (car (identifier-p str)) p))))
                    (lambda (a b) b))
               epsilon-p)
          (lambda (x y) (if (gnode? y) y x)))
     str)))

(define open  (lambda (str)
    ((pred-p (lambda (t) (if (char=? t #\[) #t #f)))
     (cdr (whitespace-p str)))))

(define close
  (lambda (str)
    ((pred-p (lambda (t) (if (char=? t #\]) #t #f)))
     (cdr (whitespace-p str)))))

(define term-p
  (lambda (str)
    ((alt (seq (seq c-open expression-p (lambda (x y) y)) c-close(lambda (a b) a))
          (alt number-p variable-p))
     str)))

(define c-open
  (lambda (str) ((pred-p (lambda (t) (if (char=? t #\() #t #f)))
     (cdr (whitespace-p str)))))

(define c-close
  (lambda (str) ((pred-p (lambda (t) (if (char=? t #\)) #t #f)))
    (cdr (whitespace-p str)))))

(define expression-p
  (lambda (str)
    ((seq term-p (alt
                  (seq add expression-p (lambda (x y) (gnode 'PLUS (list (car (term-p str)) y))))
                      epsilon-p)
   (lambda (a b) (if (gnode? b) b a)))
    str)))

(define add
  (lambda (str)
    ((pred-p (lambda (t) (if (char=? t #\+) #t #f)))
    (cdr (whitespace-p str)))))

(define assignment-p
  (lambda (str)
    ((seq variable-p (seq equal expression-p (lambda (x y) y))
          (lambda (a b) (gnode 'ASSIGN (list a b))))
    str)))

(define equal
  (lambda (str)
    ((pred-p (lambda (t) (if (char=? t #\=) #t #f)))
     (cdr (whitespace-p str)))))

(define (combine-cc char1 char2) (list->string (list char1 char2)))
(define (combine-sc str char) (list->string (append (string->list str) (list char))))
(define (combine-cs char str) (list->string (cons char (string->list str))))
(define (combine-ss str1 str2) (list->string (append (string->list str1) (string->list str2))))