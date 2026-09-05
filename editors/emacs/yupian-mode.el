;;; yupian-mode.el --- Minimal Yupian major mode -*- lexical-binding: t; -*-

(defconst yupian-keywords
  '("木" "水" "火" "土" "竹" "十" "戈" "大" "中" "一" "弓" "人" "心" "手" "口" "尸" "廿" "山" "田" "難" "卜"))

(defvar yupian-font-lock-keywords
  `((,(regexp-opt yupian-keywords 'symbols) . font-lock-keyword-face)
    ("[()「」[]『』]" . font-lock-builtin-face)
    ("[-+]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)" . font-lock-constant-face)
    (";.*$" . font-lock-comment-face)
    ("\"\\(?:\\\\.\\|[^\"\\\\]\\)*\"" . font-lock-string-face)))

;;;###autoload
(define-derived-mode yupian-mode prog-mode "Yupian"
  "Major mode for the Yupian Lisp DSL."
  (setq-local comment-start ";")
  (setq-local font-lock-defaults '(yupian-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.yupian\\'" . yupian-mode))

(provide 'yupian-mode)
;;; yupian-mode.el ends here
