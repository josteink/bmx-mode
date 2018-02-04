;;; bmx-mode.el --- Batch Mode eXtras

;; Copyright (C) 2018 Jostein Kjønigsen

;; Author: Jostein Kjønigsen <jostein@gmail.com>
;; URL: http://github.com/josteink/bmx-mode
;; Version: 0.1
;; Keywords: bat-mode batch
;; Package-Requires: ((cl-lib "0.5") (company "0.9.4")

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'company)

;;;
;;; utility functions
;;;

(defun bmx--label-prefix (name)
  (if (string-equal (substring-no-properties name 0 1) ":")
      name
    (concat ":" name)))

(defun bmx--label-unprefix (name)
  (if (string-equal (substring-no-properties name 0 1) ":")
      (substring-no-properties name 1)
    name))

(defun bmx--variable-normalize (name)
  (if (string-equal (substring-no-properties name 0 1) "%")
      name
    (concat "%" name "%")))

(defun bmx--variable-unnormalize (name)
  (if (string-equal (substring-no-properties name 0 1) "%")
      (substring-no-properties name 1 (- (length name) 1))
    name))
;;;
;;; consts
;;;

(defconst bmx--rx-label-invocation "\\<\\(call\\|goto\\)\s+\\(:?[[:alnum:]_]*\\)")
(defconst bmx--rx-label-declaration "^\\(:[[:alnum:]_]+\\)\\>")

;;;
;;; labels
;;;

(defun bmx--get-labels ()
  (save-excursion
    (goto-char (point-min))

    (let ((result))
      (while (search-forward-regexp bmx--rx-label-declaration nil t nil)
        (add-to-list 'result (match-string-no-properties 1)))

      (sort result 'string-lessp))))

(defun bmx--get-matching-labels (prefix &optional label-list)
  (let ((prefixed (or label-list (bmx--get-labels))))
    (-filter (lambda (item)
               (s-prefix-p prefix item t))
             prefixed)))

(defun bmx--insert-colon-and-complete ()
  (interactive)
  (insert ?:)
  (when (looking-back bmx--rx-label-invocation)
    (company-manual-begin)))

(defun bmx--company-label-backend (command &optional arg &rest ignored)
  (case command
    (interactive)
    (prefix (when
                (and (equal major-mode 'bat-mode)
                     (looking-back bmx--rx-label-invocation))
              (match-string 2)))
    (candidates (bmx--get-matching-labels arg))
    (meta (format "This value is named %s" arg))
    (ignore-case t)))

(defun bmx--label-at-point ()
  (cond
   ;; cursor within label used in invocation invocation
   ((looking-back "\\(call\\|goto\\)\s+:?[[:alnum:]_]*")
    (bmx--label-prefix
     (string-no-properties
      (symbol-name
       (save-excursion
         (or (symbol-at-point)
             ;; in case we are at the :... in which case
             ;; symbol is nil
             (progn
               (forward-char 1)
               (symbol-at-point))))))))

   ;; cursor on keyword used in invocation
   ((or (string-equal "goto" (downcase (symbol-name (symbol-at-point))))
        (string-equal "call" (downcase (symbol-name (symbol-at-point)))))
    (save-excursion
      (search-forward-regexp "\s")
      (forward-word 1)
      (bmx--label-prefix
       (string-no-properties
        (symbol-name
         (symbol-at-point))))))

   ;; else: label declaration
   ((save-excursion
      (beginning-of-line 1)
      (looking-at bmx--rx-label-declaration))
    (match-string-no-properties 1))

   ;; nada
   (t
    nil)))

(defun bmx--label-find-references (label)
  (let ((rx-label (regexp-quote label))
        (rx-unprefix (regexp-quote (bmx--label-unprefix label))))

    (occur (concat "\\("
                   "^" rx-label "\\>" ;; any usage with :label and nothing/space after
                   ;; usage without : ... must look for keyword identifiers!
                   "\\|"
                   "\\(goto\\|call\\)\s+"
                   "\\(:?" rx-unprefix "\\)"
                   "\\)\\>"))))

(defun bmx--label-navigate-to (label)
  (ring-insert find-tag-marker-ring (point-marker))
  (beginning-of-buffer)
  (search-forward-regexp (concat "^" (regexp-quote label) "\s*$"))
  (beginning-of-line))


;;;
;;; variables
;;;

(defun bmx--get-variables ()
  ;; TODO: include those found in `process-environment'?
  (save-excursion
    (goto-char (point-min))

    (let ((result))
      (while (search-forward-regexp "^set\s+\\([a-zA-Z0-9_]+\\)\s*=.*" nil t nil)
        (add-to-list 'result (bmx--variable-normalize (match-string-no-properties 1))))

      (sort result 'string-lessp))))

(defun bmx--get-matching-variables (prefix &optional variables-list)
  (let ((variables (or variables-list (bmx--get-variables))))
    (if (eq "%" prefix)
        variables
      (-filter (lambda (item)
                 (s-prefix-p prefix item t))
               variables))))

(defun bmx--company-variable-backend (command &optional arg &rest ignored)
  (case command
    (prefix (when
                (and (equal major-mode 'bat-mode)
                     (looking-back "\\(%[a-zA-Z0-9_]*\\)"))
              (match-string 1)))
    (candidates (bmx--get-matching-variables arg))
    (meta (format "This value is named %s" arg))
    (ignore-case t)))

(defun bmx--insert-percentage-and-complete ()
  (interactive)
  (insert ?%)
  (company-manual-begin))



(defun bmx--variable-at-point ()
  (let ((eol))
    (save-excursion
      (move-end-of-line 1)
      (setq eol (point)))

    (save-excursion
      (cond
       ;; cursor at start of variable invocation |%var%
       ((looking-at "%\\([[:alnum:]_]+\\)%")
        (match-string-no-properties 1))

       ;; cursor within a variable - %va|r%
       ((looking-at "\\([[:alnum:]_]+\\)%")
        (string-no-properties (symbol-name (symbol-at-point))))

       ;; cursor within a variable - %var|%
       ((looking-back "%\\([[:alnum:]_]+\\)")
        (match-string-no-properties 1))

       ;; line has variable declaration
       ((progn
          (beginning-of-line 1)
          (search-forward-regexp "^set \\([[:alnum:]_]+\\)=" eol t 1))
        (match-string-no-properties 1))))))

(defun bmx--variable-find-references (variable)
  (let ((rx-variable (regexp-quote variable)))
    (occur (concat "\\("
                   (concat "set " rx-variable "=") ;; declarations
                   "\\|"
                   (concat "%" rx-variable "%") ;; usage
                   "\\)"))))

(defun bmx--variable-navigate-to (variable)
  (ring-insert find-tag-marker-ring (point-marker))
  (beginning-of-buffer)
  (search-forward-regexp (concat
                          "set "
                          (regexp-quote variable)
                          "=")))

;;
;; general commands
;;

(defun bmx-find-references-at-point ()
  (interactive)
  (cond ((bmx--variable-at-point) (bmx--variable-find-references (bmx--variable-at-point)))
        ((bmx--label-at-point) (bmx--label-find-references (bmx--label-at-point)))
        (t (message "No referencable symbol found at point!"))))

(defun bmx-navigate-to-symbol-at-point ()
  (interactive)
  (cond ((bmx--variable-at-point) (bmx--variable-navigate-to (bmx--variable-at-point)))
        ((bmx--label-at-point) (bmx--label-navigate-to (bmx--label-at-point)))
        (t (message "No referencable symbol found at point!"))))

;;
;; mode setup
;;

(setq bmx-keymap (let ((map (make-sparse-keymap)))
                   (define-key map (kbd ":") #'bmx--insert-colon-and-complete)
                   (define-key map (kbd "%") #'bmx--insert-percentage-and-complete)
                   (define-key map (kbd "M-.") #'bmx-navigate-to-symbol-at-point)
                   (define-key map (kbd "<S-f12>") #'bmx-find-references-at-point)
                   map))

(define-minor-mode bmx-mode
  "Small enhancements for editing batch-files."
  :lighter "bat-ide"
  :global nil
  :keymap bmx-keymap)

;; tie it all up with bat-mode
(add-hook 'bat-mode-hook #'bmx-mode)
(add-to-list 'company-backends #'bmx--company-label-backend)
(add-to-list 'company-backends #'bmx--company-variable-backend)


(provide 'bmx-mode)

;;; bmx-mode.el ends here
