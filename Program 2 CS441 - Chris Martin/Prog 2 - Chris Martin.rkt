#lang racket

(define (parse filename)
  (define input (string-trim (file->string filename)))
  (define-values (result remaining) (parse-program (string->list input)))
  (if (and result (null? remaining))
      (begin (displayln "Accept") (displayln "Parse Tree:") (display result) result)
      (string-append "Syntax error: " (if (null? remaining) "end of input" (list->string remaining)))))

(define (trim-whitespace chars)
  (if (and (not (null? chars)) (char-whitespace? (car chars)))
      (trim-whitespace (cdr chars))
      chars))

(define (parse-program chars)
  (define-values (stmt-list-result remaining) (parse-list (trim-whitespace chars)))
  (if (and stmt-list-result (not (null? remaining)) (char=? (car remaining) #\$) (char=? (cadr remaining) #\$))
      (values stmt-list-result (cddr remaining))
      (values #f chars)))

(define (parse-list chars)
  (if (null? chars)
      (values '() chars)
      (let-values ([(stmt-result remaining) (parse-stmt (trim-whitespace chars))])
        (if stmt-result
            (let-values ([(stmt-list-result remaining2) (parse-list remaining)])
              (values (cons stmt-result stmt-list-result) remaining2))
            (values '() chars)))))

(define (parse-stmt chars)
  (define trimmed (trim-whitespace chars))
  (cond
    [(and (not (null? trimmed)) (char-alphabetic? (car trimmed)))
     (let-values ([(id remaining) (parse-id trimmed)])
       (if (and (not (null? remaining)) (char=? (car remaining) #\=))
           (let-values ([(expr-result remaining2) (parse-expr (cdr remaining))])
             (if (and expr-result (not (null? remaining2)) (char=? (car remaining2) #\;))
                 (values (list 'assign id expr-result) (cdr remaining2))
                 (values #f chars)))
           (values #f chars)))]
    [(and (>= (length trimmed) 2) (equal? (list->string (take trimmed 2)) "if"))
     (let-values ([(expr-result remaining) (parse-expr (drop trimmed 2))])
       (if expr-result
           (let-values ([(stmt-list-result remaining2) (parse-list remaining)])
             (if (and stmt-list-result (>= (length remaining2) 5) (equal? (list->string (take remaining2 5)) "endif"))
                 (values (list 'if expr-result stmt-list-result) (drop remaining2 5))
                 (values #f chars)))
           (values #f chars)))]
    [(and (>= (length trimmed) 4) (equal? (list->string (take trimmed 4)) "read"))
     (let-values ([(id remaining) (parse-id (drop trimmed 4))])
       (if (and id (not (null? remaining)) (char=? (car remaining) #\;))
           (values (list 'read id) (cdr remaining))
           (values #f chars)))]
    [(and (>= (length trimmed) 5) (equal? (list->string (take trimmed 5)) "write"))
     (let-values ([(expr-result remaining) (parse-expr (drop trimmed 5))])
       (if (and expr-result (not (null? remaining)) (char=? (car remaining) #\;))
           (values (list 'write expr-result) (cdr remaining))
           (values #f chars)))]
    [else (values #f chars)]))

(define (parse-num chars)
  (let loop ([remaining chars] [num '()])
    (if (and (not (null? remaining)) (char-numeric? (car remaining)))
        (loop (cdr remaining) (append num (list (car remaining))))
        (values (list->string num) remaining))))


(define (parse-id chars)
  (let loop ([remaining chars] [id '()])
    (if (and (not (null? remaining)) (char-alphabetic? (car remaining)))
        (loop (cdr remaining) (append id (list (car remaining))))
        (values (list->string id) remaining))))

(define (parse-expr chars)
  (define trimmed (trim-whitespace chars))
  (let-values ([(first remaining) (if (char-alphabetic? (car trimmed)) 
                                      (parse-id trimmed) 
                                      (parse-num trimmed))])
    (if first
        (let-values ([(etail-result remaining2) (parse-etail remaining)])
          (values (if etail-result (list 'expr first etail-result) first) remaining2))
        (values #f chars))))

(define (parse-etail chars)
  (if (null? chars)
      (values '() chars)
      (let ((first (car chars)))
        (if (or (char=? first #\+) (char=? first #\-))
            (let-values ([(expr-result remaining) (parse-expr (cdr chars))])
              (if expr-result
                  (values (list first expr-result) remaining)
                  (values #f chars)))
            (values '() chars)))))
