#lang racket/base

(require json
         racket/file
         racket/format
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         yaml)

(define-runtime-path root "../..")
(define spec-path (build-path root "spec" "tokens.yaml"))
(define spec (file->yaml spec-path))

(define (field name [table spec])
  (hash-ref table name
            (lambda () (error 'generate.rkt "missing spec field: ~a" name))))

(define language (field "language"))
(define comment-line (hash-ref (field "comments") "line"))
(define number-pattern (hash-ref (field "numbers") "pattern"))
(define keywords (field "keywords"))
(define delimiters (append (hash-ref (field "delimiters") "list")
                           (hash-ref (field "delimiters") "vector")))

(define (join-values values separator)
  (string-join (map ~a values) separator))

(define (write-generated relative-path content)
  (define path (build-path root relative-path))
  (make-directory* (path-only path))
  (call-with-output-file path
    (lambda (output) (display content output))
    #:exists 'truncate))

(define keyword-text (join-values keywords " "))
(define delimiter-text (regexp-quote (join-values delimiters "")))
(define keyword-regexp
  (string-append "(?:" (join-values keywords "|") ")"))
(define delimiter-regexp
  (string-append "[" (join-values delimiters "") "]"))

(define (xml-escape value)
  (regexp-replace* #rx"&" (regexp-replace* #rx"<" (regexp-replace* #rx">" value "&gt;") "&lt;") "&amp;"))

(define (json-file relative-path value)
  (define path (build-path root relative-path))
  (make-directory* (path-only path))
  (call-with-output-file path
    (lambda (output) (write-json value output))
    #:exists 'truncate))

(write-generated
 "editors/vim/syntax/yupian.vim"
 (format
  "if exists(\"b:current_syntax\")\n  finish\nendif\n\nsyn case match\nsyn keyword yupianKeyword ~a\nsyn match yupianNumeric \"~a\"\nsyn region yupianString start='\"' skip='\\\\\"' end='\"'\nsyn match yupianComment \"~a.*$\"\nsyn match yupianDelimiter \"~a\"\nsyn match yupianIdentifier \"\\<[[:alnum:]_?-][[:alnum:]_?!-]*\\>\"\n\nhi def link yupianKeyword Keyword\nhi def link yupianNumeric Numeric\nhi def link yupianString String\nhi def link yupianComment Comment\nhi def link yupianDelimiter Delimiter\nhi def link yupianIdentifier Identifier\n\nlet b:current_syntax = \"~a\"\n"
  keyword-text number-pattern comment-line delimiter-regexp language))

(write-generated
 "editors/emacs/yupian-mode.el"
 (format
  ";;; yupian-mode.el --- Minimal Yupian major mode -*- lexical-binding: t; -*-\n\n(defconst yupian-keywords\n  '(~a))\n\n(defvar yupian-font-lock-keywords\n  `((,(regexp-opt yupian-keywords 'symbols) . font-lock-keyword-face)\n    (\"~a\" . font-lock-builtin-face)\n    (\"~a\" . font-lock-constant-face)\n    (\"~a.*$\" . font-lock-comment-face)\n    (\"\\\"\\\\(?:\\\\\\\\.\\\\|[^\\\"\\\\\\\\]\\\\)*\\\"\" . font-lock-string-face)))\n\n;;;###autoload\n(define-derived-mode yupian-mode prog-mode \"Yupian\"\n  \"Major mode for the Yupian Lisp DSL.\"\n  (setq-local comment-start \"~a\")\n  (setq-local font-lock-defaults '(yupian-font-lock-keywords)))\n\n;;;###autoload\n(add-to-list 'auto-mode-alist '(\"\\\\.yupian\\\\'\" . yupian-mode))\n\n(provide 'yupian-mode)\n;;; yupian-mode.el ends here\n"
  (string-join (map (lambda (keyword) (format "\"~a\"" keyword)) keywords) " ")
  delimiter-regexp number-pattern comment-line comment-line))

(json-file
 "editors/vscode/syntaxes/yupian.tmLanguage.json"
 (hash
  'scopeName "source.yupian"
  'name "Yupian"
  'patterns (list (hash 'include "#comment")
                 (hash 'include "#string")
                 (hash 'include "#number")
                 (hash 'include "#keyword")
                 (hash 'include "#delimiter"))
  'repository
  (hash
   'comment (hash 'name "comment.line.semicolon.yupian"
                 'match (string-append (regexp-quote comment-line) ".*$"))
   'string (hash 'name "string.quoted.double.yupian"
                'begin "\\\""
                'end "\\\""
                'patterns (list (hash 'match "\\\\."
                                      'name "constant.character.escape.yupian")))
   'number (hash 'name "constant.numeric.yupian" 'match number-pattern)
   'keyword (hash 'name "keyword.control.yupian" 'match keyword-regexp)
   'delimiter (hash 'name "punctuation.definition.list.yupian"
                    'match delimiter-regexp))))

(write-generated
 "editors/skylighting/yupian.xml"
 (format
  "<syntax name=\"~a\" version=\"1\" kateversion=\"5.0\" section=\"Sources\">\n  <highlighting>\n    <list name=\"keywords\">~a</list>\n    <contexts>\n      <context name=\"Normal\" attribute=\"Normal Text\" lineEndContext=\"#stay\">\n        <DetectSpaces />\n        <DetectChar char=\"~a\" context=\"Comment\" />\n        <StringDetect char=\"&quot;\" context=\"String\" />\n        <keyword attribute=\"Keyword\" context=\"#stay\" String=\"keywords\" />\n        <RegExpr attribute=\"Numeric\" String=\"~a\" />\n        <AnyChar attribute=\"Punctuation\" String=\"~a\" />\n      </context>\n      <context name=\"Comment\" attribute=\"Comment\" lineEndContext=\"#pop\" />\n      <context name=\"String\" attribute=\"String\" lineEndContext=\"#pop\">\n        <Detect2Chars char=\"\\\\\" char1=\"&quot;\" attribute=\"Char\" />\n        <DetectChar char=\"&quot;\" context=\"#pop\" />\n      </context>\n    </contexts>\n  </highlighting>\n  <general><keywords casesensitive=\"true\" /></general>\n</syntax>\n"
  language
  (string-join (map (lambda (keyword) (format "<item>~a</item>" (xml-escape keyword))) keywords) "")
  (xml-escape comment-line)
  (xml-escape number-pattern)
  (xml-escape (join-values delimiters ""))))

(write-generated
 "editors/pygments/yupian.py"
 (format
  "from pygments.lexer import RegexLexer\nfrom pygments.token import Comment, Keyword, Name, Numeric, Punctuation, String, Text\n\n\nclass YupianLexer(RegexLexer):\n    name = \"Yupian\"\n    aliases = [\"~a\"]\n    filenames = [\"*.yupian\"]\n    tokens = {\n        \"root\": [\n            (r\"~a.*$\", Comment.Single),\n            (r'\"(\\\\.|[^\"\\\\])*\"', String),\n            (r\"~a\", Numeric),\n            (r\"~a\", Punctuation),\n            (r\"~a\", Keyword),\n            (r\"[\\w?!-]+\", Name),\n            (r\"\\s+\", Text),\n            (r\".\", Text),\n        ]\n    }\n"
  language comment-line number-pattern delimiter-regexp keyword-regexp))

(displayln "generated Yupian editor definitions")