#lang typed/racket/base

(require racket/list
        racket/match
        racket/port)

(require "./type-annotation.rkt")

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

(: loop-binding->datum (-> S-Tree Any))
(define (loop-binding->datum binding)
  (match binding
    [(s-list items)
     (map s-tree->datum items)]
    [_ (s-tree->datum binding)]))


(: middle-var-declar-as-arg->racket (-> MiddleVarDeclar Any))
(define (middle-var-declar-as-arg->racket middle-var-declar)
  (list 
    (MiddleVarDeclar-name middle-var-declar) 
    ': 
    (MiddleVarDeclar-type middle-var-declar)))

(: middle-var-declar-stand-alone->racket (-> MiddleVarDeclar Any Any))
(define (middle-var-declar-stand-alone->racket middle-var-declar value)
  (let [
    [var-name (MiddleVarDeclar-name middle-var-declar)]
    [var-type (MiddleVarDeclar-type middle-var-declar)]]
    (list 'define: var-name ': var-type value )))

(: middle-fun-header-declar->racket (-> MiddleFunHeaderDeclar Any))
(define (middle-fun-header-declar->racket declar)
  (let [
    [func-name (MiddleFunHeaderDeclar-func-name declar)]
    [params (MiddleFunHeaderDeclar-params declar)]]
    (cons func-name (map middle-var-declar-as-arg->racket params))))

(: s-tree->datum (-> S-Tree Any))
(define (s-tree->datum tree)
  (cond
    [(s-atom? tree) (s-atom-value tree)]
    [else
      (match (s-list-items tree)
        [(cons (s-atom '山) others)
          ; Now we are going to parse 山. If a malformed 山, throw error.
          (match (map s-tree->datum others)
            [(list var-name-part body-part) 
              (cond 
                ; here we are serious to generate a definition along the entry
                ; Is it a var? 
                [(valid-yupian-var-declar? var-name-part)
                  (middle-var-declar-stand-alone->racket 
                    (valid-yupian-var-declar->middle var-name-part)
                    body-part)]
                ; Is it a function? 
                [(valid-yupian-fun-header-declar? var-name-part)
                  (list 
                    'define 
                    (middle-fun-header-declar->racket 
                      (valid-yupian-fun-header-declar->middle var-name-part))
                    body-part)]
                [#t 
                  (error 'yupian-compiler/山 
                    "误山字第二号。暂时名部须入变量形或函数形。
变量形须择一：
  变量名
  『变量名 : 类型』
函数形：
  『变量形零号作函数名 变量形一号 如是以降』
  函数名不可缺，暂不可标返回值类型。
  自变量形一号起，可略之。")]
              )]
            [_ (error 
              'yupian-compiler/山 
              "误山字第一号。山之形须为 `「山 名部 实部」`。今阙矣，抑或杂矣。")])
        ]
        ; As the coverage of the type, as the mountain fog covers the trees
        [(cons (s-atom '岚) (cons var-declar others))
          (list 'define var-declar (map s-tree->datum others))]
        [(cons (s-atom '田) others ) (cons 'let* (map s-tree->datum others))]
        [(list (s-atom '口) condition (s-list bindings) (s-list steps) final)
          (list 'let: 'loop ': 'Any
          (map loop-binding->datum bindings)
          (list 'if
            (s-tree->datum condition)
            (cons 'loop (map s-tree->datum steps))
            (s-tree->datum final)))]
        [(list (s-atom operator) (s-atom name) value)
          (map s-tree->datum (s-list-items tree))]
          [items (map s-tree->datum items)])]))

(module+ main
  (displayln (map s-tree->datum
                  (parse-program (port->string (current-input-port))))))