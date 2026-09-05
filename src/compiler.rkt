#lang typed/racket/base

(require racket/list
         racket/match
         racket/port)

(provide
 S-Tree s-atom s-atom? s-atom-value s-list s-list? s-list-items
 normalize-delimiters parse-program s-tree->datum)

(struct s-atom ([value : Any]) #:transparent)
(struct s-list ([items : (Listof S-Tree)]) #:transparent)
(define-type S-Tree (U s-atom s-list))

(: normalize-delimiters (-> String String))
(define (normalize-delimiters source)
  (define characters : (Listof Char) (string->list source))
  (define translated : (Listof Char)
    (let loop : (Listof Char)
      ([remaining : (Listof Char) characters]
       [in-string? : Boolean #f]
       [escaped? : Boolean #f])
      (cond
        [(null? remaining) '()]
        [else
         (define character (car remaining))
         (define rest (cdr remaining))
         (cond
           [in-string?
            (cons character
                  (loop rest
                        (if (and (char=? character #\")
                                 (not escaped?))
                            #f
                            #t)
                        (and (char=? character #\\)
                             (not escaped?))))]
           [(char=? character #\")
            (cons character (loop rest #t #f))]
           [(char=? character #\「)
            (cons #\( (loop rest #f #f))]
           [(char=? character #\」)
            (cons #\) (loop rest #f #f))]
           [(char=? character #\『)
            (cons #\[ (loop rest #f #f))]
           [(char=? character #\』)
            (cons #\] (loop rest #f #f))]
               [else
            (cons character (loop rest #f #f))])])
          ))
  (list->string translated))

(: datum->s-tree (-> Any S-Tree))
(define (datum->s-tree datum)
  (if (list? datum)
      (s-list (map datum->s-tree datum))
      (s-atom datum)))

(: read-trees (-> Input-Port (Listof S-Tree)))
(define (read-trees input)
  (let loop : (Listof S-Tree) ([trees : (Listof S-Tree) '()])
    (define datum (read input))
    (if (eof-object? datum)
        (reverse trees)
        (loop (cons (datum->s-tree datum) trees)))))

(: parse-program (-> String (Listof S-Tree)))
(define (parse-program source)
  (read-trees (open-input-string (normalize-delimiters source))))

(: s-tree->datum (-> S-Tree Any))
(define (s-tree->datum tree)
  (cond
    [(s-atom? tree) (s-atom-value tree)]
    [else
         (match (s-list-items tree)
       [(list (s-atom operator) (s-atom name) value)
        (if (eq? operator '山)
        (list 'define name (s-tree->datum value))
        (map s-tree->datum (s-list-items tree)))]
       [items (map s-tree->datum items)])]))

(module+ main
  (displayln (map s-tree->datum
                  (parse-program (port->string (current-input-port))))))