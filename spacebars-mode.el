;;; spacebars-mode.el --- A major mode for the Spacebars template
;;; language for Meteor

;; Copyright (C) 2015 Rúnar Berg Baugsson Sigríðarson

;; Author: Rúnar Berg Baugsson Sigríðarson
;; Version: 0.0.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;   This is an emacs major mode for the Spacebars template language.
;;   It is heavily based on jinja2-mode by paradoxxxzero available at
;;   https://github.com/paradoxxxzero/jinja2-mode.
;;
;;   Like jinja2-mode, spacebars-mode includes:
;;   * syntax highlighting
;;   * sgml/html integration
;;   * indentation (working with sgml)

;; This file comes from http://github.com/runarberg/spacebars-mode

;;; Code:

(require 'sgml-mode)

(defgroup spacebars nil
  "Major mode for editing spacebars templates."
  :prefix "spacebars-"
  :group 'languages)

(defcustom spacebars-user-keywords nil
  "Custom keyword names"
  :type '(repeat string)
  :group 'spacebars)

;; (defcustom spacebars-debug nil
;;   "Log indentation logic"
;;   :type 'boolean
;;   :group 'spacebars)

(defun spacebars-builtin-keywords ()
  (append
   spacebars-user-keywords
   '("if" "unless" "each" "with")))

(defun spacebars-closing-keywords ()
  (spacebars-builtin-keywords))

(defun spacebars-indenting-keywords ()
  (append
   (spacebars-closing-keywords)
   '("else")))

(defun spacebars-find-open-tag ()
  (if (search-backward-regexp
       (rx-to-string
        `(and "{{#"
              (* whitespace)
              (group
               (*? anything))
              (* whitespace)
              "}}")) nil t)
      (match-string 1)
    nil))

(defun spacebars-close-block ()
  "Close the previously opened block tag"
  (interactive)
  (let ((open-tag (save-excursion (spacebars-find-open-tag))))
    (if open-tag
        (insert (format "{{/%s}}" (match-string 1)))
      (error "Nothing to close")))
  (save-excursion (spacebars-indent-line)))

(define-skeleton spacebars-insert-block
  "Insert an open and closing block tags"
  ""
  > "{{#" (setq name (skeleton-read "Block name: "))
  (if (member name (spacebars-builtin-keywords))
      (concat " " (skeleton-read (concat name " ")))
    "")
  "}}" \n
  _ \n
  "{{/" name "}}" >)

(define-skeleton spacebars-insert-tag
  "Insert a double braced tag"
  ""
  "{{" _ "}}")

(define-skeleton spacebars-insert-inclution
  "Insert an inclution tag"
  ""
  "{{>" _ "}}")


(define-skeleton spacebars-insert-comment
  "Insert a comment tag"
  ""
  "{{!--" _ "--}}")

(defconst spacebars-font-lock-comments
  `(
    (,(rx "{{!--"
          (* whitespace)
          (group
           (*? anything))
          (* whitespace)
          "--}}")
     . (1 font-lock-comment-face t))))

(defconst spacebars-font-lock-keywords-1
  (append
   spacebars-font-lock-comments
   sgml-font-lock-keywords-1))

(defconst spacebars-font-lock-keywords-2
  (append
   spacebars-font-lock-keywords-1
   sgml-font-lock-keywords-2))

(defconst spacebars-font-lock-keywords-3
  (append
   spacebars-font-lock-keywords-1
   spacebars-font-lock-keywords-2
   `(
     (,(rx "{{" (* whitespace)
           (group "else")
           (* whitespace) "}}")
      (1 font-lock-keyword-face))

     (,(rx "{{" (* whitespace)
           (? (or ">") (* whitespace))
           (group
            (*? (or word "." "$" "_")))
           (* (+ whitespace) (*? anything))
           (* whitespace) "}}")
      (1 font-lock-variable-name-face))

     ;; (,(rx  (group (word) "=" (* whitespace))
     ;;        (group (word)))
     ;;  (1 font-lock-keyword-face t)
     ;;  (2 font-lock-warning-face t))

     (,(rx
          "{{" (* whitespace) (or "#" "/") (* whitespace)
          (group
           (eval
            (append '(or)
                    (spacebars-builtin-keywords))))
          (* (* whitespace) (*? anything))
          (* whitespace) "}}")
      (1 font-lock-keyword-face))

     (,(rx "{{" (* whitespace) (or "#" "/") (* whitespace)
           (group
            (*? word))
           (* whitespace)
           "}}")
      (1 font-lock-function-name-face))

     (,(rx
        (group "{{{")
        (*? anything)
        (group "}}}"))
      (1 font-lock-type-face)
      (2 font-lock-type-face))

     (,(rx
        (group "{{" (? (* whitespace) (or ">" "#" "/")))
        (*? anything)
        (group "}}"))
      (1 font-lock-type-face)
      (2 font-lock-type-face))

     (,(rx "{{!"
           (* whitespace)
           (group
            (*? anything))
           (* whitespace)
           "}}")
      (1 font-lock-comment-face t))

     (,(rx "{{!--"
           (* whitespace)
           (group
            (*? anything))
           (* whitespace)
           "--}}")
      (1 font-lock-comment-face t))
     (,(rx
        (group "{{!")
        (*? anything)
        (group "}}"))
      (1 font-lock-comment-delimiter-face t)
      (2 font-lock-comment-delimiter-face t))
     (,(rx
        (group "{{!--")
        (*? anything)
        (group "--}}"))
      (1 font-lock-comment-delimiter-face t)
      (2 font-lock-comment-delimiter-face t))
     )))

(defvar spacebars-font-lock-keywords
  spacebars-font-lock-keywords-1)

(defun sgml-indent-line-num ()
  "Indent the current line as SGML."
  (let* ((savep (point))
         (indent-col
          (save-excursion
            (back-to-indentation)
            (if (>= (point) savep) (setq savep nil))
            (sgml-calculate-indent))))
    (if (null indent-col)
        0
      (if savep
          (save-excursion indent-col)
        indent-col))))

(defun spacebars-calculate-indent-backward (default)
  "Return indent column based on previous lines"
  (let ((indent-width sgml-basic-offset) (default (sgml-indent-line-num)))
    (forward-line -1)
    (if (looking-at "^[ \t]*{{/") ; Don't indent after end
        (current-indentation)
      (if (looking-at "^[ \t]*{{# *.*?{{/ ")
          (current-indentation)
        (if (looking-at "^[ \t]*{{# *" ) ; Check start tag
            (+ (current-indentation) indent-width)
          (if (looking-at "^[ \t]*<") ; Assume sgml block trust sgml
              default
            (if (bobp)
                0
              (spacebars-calculate-indent-backward default))))))))


(defun spacebars-calculate-indent ()
  "Return indent column"
  (if (bobp)  ; Check begining of buffer
      0
    (let ((indent-width sgml-basic-offset) (default (sgml-indent-line-num)))
      (if (looking-at "^[ \t]*{{\\(/\\| *else\\)") ; Check close tag
          (save-excursion
            (forward-line -1)
            (if (and (looking-at "^[ \t]*{{#")
                     (not (looking-at "^[ \t]*{{# *.*?{{/")))
                (current-indentation)
              (- (current-indentation) indent-width)))
        (if (looking-at "^[ \t]*</") ; Assume sgml end block trust sgml
            default
          (save-excursion
            (spacebars-calculate-indent-backward default)))))))

(defun spacebars-indent-line ()
  "Indent current line as spacebars code"
  (interactive)
  (let ((old_indent (current-indentation)) (old_point (point)))
    (move-beginning-of-line nil)
    (let ((indent (max 0 (spacebars-calculate-indent))))
      (indent-line-to indent)
      (if (< old_indent (- old_point (line-beginning-position)))
          (goto-char (+ (- indent old_indent) old_point)))
      indent)))


;;;###autoload
(define-derived-mode spacebars-mode html-mode  "Spacebars"
  "Major mode for editing spacebars meteor templates"
  :group 'spacebars
  (set (make-local-variable 'comment-start) "{{!--")
  (set (make-local-variable 'comment-start-skip) "{{!--")
  (set (make-local-variable 'comment-end) "--}}")
  (set (make-local-variable 'comment-end-skip) "--}}")
  ;; it mainly from sgml-mode font lock setting
  (set (make-local-variable 'font-lock-defaults)
       '((
          spacebars-font-lock-keywords
          spacebars-font-lock-keywords-1
          spacebars-font-lock-keywords-2
          spacebars-font-lock-keywords-3)
         nil t nil nil
         (font-lock-syntactic-keywords
          . sgml-font-lock-syntactic-keywords)))
  (set (make-local-variable 'indent-line-function) 'spacebars-indent-line))

(define-key spacebars-mode-map (kbd "C-c C-c t") 'spacebars-insert-tag)
(define-key spacebars-mode-map (kbd "C-c C-c b") 'spacebars-insert-block)
(define-key spacebars-mode-map (kbd "C-c C-c /") 'spacebars-close-block)
(define-key spacebars-mode-map (kbd "C-c C-c >") 'spacebars-insert-inclution)
(define-key spacebars-mode-map (kbd "C-c C-c !") 'spacebars-insert-comment)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.spacebars\\'" . spacebars-mode))

(provide 'spacebars-mode)

;;; spacebars-mode.el ends here
