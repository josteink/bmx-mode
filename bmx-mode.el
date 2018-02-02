;;; bmx-mode.el --- Batch Mode eXtras

;; Copyright (C) 2018 Jostein Kjønigsen

;; Author: Jostein Kjønigsen <jostein@gmail.com>
;; URL: http://github.com/josteink/bmx-mode
;; Version: 0.1
;; Keywords: bat-mode batch
;; Package-Requires: ((cl-lib "0.5") (popup "0.5.3")

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

(require 'imenu)
(require 'popup)

(defun bmx--get-labels ()
  (let ((index (imenu--make-index-alist)))
    (sort
     (-filter (lambda (item)
                (not (string-equal "*Rescan*" item)))
              (mapcar #'car index))
     'string-lessp)))

;; (defun bmx--get-matching-labels (prefix)
;;   (if (eq "" prefix)
;;       (bmx--get-labels)
;;     (-filter (lambda (item)
;;                (s-prefix-p prefix item t))
;;              (bmx--get-labels))))

;; (defun bmx--company-label-backend (command &optional arg &rest ignored)
;;   (case command
;;     (interactive (company-begin-backend #'my-company-batch-label-backend))
;;     (prefix (and (or (looking-back "call :" 7)
;;                      (looking-back "goto :" 7))))
;;     (candidates (bmx--get-matching-labels arg))
;;     (meta (format "This value is named %s" arg))))

(defun bmx-insert-label ()
  (interactive)

  (if (or (looking-back "call " 6)
          (looking-back "goto " 6))
      (progn
        (insert
         (popup-menu* (bmx--get-labels)))
        (insert " "))
    (insert-char ?:)))

(defun bmx--get-variables ()
  (save-excursion
    (goto-char (point-min))

    (let ((result))
      (while (search-forward-regexp "^set\s+\\([a-zA-Z0-9_]+\\)\s*=.*" nil t nil)
        (add-to-list 'result (match-string-no-properties 1)))

      (sort result 'string-lessp))))

;; TODO: can be applied to company-mode instead? (completion will be scoped to % ?)
(defun bmx-insert-variable ()
  (interactive)

  ;; anything non-alpha
  (if (or (looking-back " " 2)
          (looking-back "\"" 2)
          (looking-back "\\\\" 2)
          (looking-back "-" 2))
      (let ((choice (popup-menu* (bmx--get-variables))))
        (insert-char ?%)
        (insert choice)
        (insert-char ?%))
    (insert-char ?%)))

(defun bmx--label-at-point ()
  ;; look for declarations : from beginning of line, or invocations call/goto :
  (save-excursion
    ;; simplistic aproach: assume only one label per line!
    (let ((eol (progn
                 (move-end-of-line 1)
                 (point))))
      (move-beginning-of-line 1)
      (if (search-forward-regexp ":\\([[:alnum:]_]+\\)" eol t 1)
          (match-string-no-properties 1)
        nil))))

(defun bmx--variable-at-point ()
  (when (eq (face-at-point) 'font-lock-variable-name-face)
    (substring-no-properties (symbol-name (symbol-at-point)))))

(defun bmx--label-find-references (label)
  (let ((rx-label (regexp-quote label)))
    (occur (concat "\\("
                   (concat ":"  rx-label "\\(\s\\|$\\)") ;; any usage with :label and nothing/space after
                   ;; usage without : ... must look for keyword identifiers!
                   (concat "\\|goto\s+" rx-label)
                   (concat "\\|call\s+" rx-label)
                   "\\)"))))

(defun bmx--label-navigate-to (label)
  (ring-insert find-tag-marker-ring (point-marker))
  (imenu (concat ":" label)))

(defun bmx--variable-find-references (variable)
  (let ((rx-variable (regexp-quote variable)))
    (occur (concat "\\("
                   (concat "set " rx-variable "=") ;; declarations
                   "\\|"
                   (concat "%" rx-variable "%") ;; usage
                   "\\)"))))

(defun bmx--variable-navigate-to (variable)
  (ring-insert find-tag-marker-ring (point-marker))
  (goto-char (point-min))
  (search-forward-regexp (concat
                          "set "
                          (regexp-quote variable)
                          "=")))

;; test thingie :CALL_MEE

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

(setq bmx-keymap (let ((map (make-sparse-keymap)))
                   (define-key map (kbd ":") #'bmx-insert-label)
                   (define-key map (kbd "%") #'bmx-insert-variable)
                   (define-key map (kbd "M-.") #'bmx-navigate-to-symbol-at-point)
                   (define-key map (kbd "<S-f12>") #'bmx-find-references-at-point)
                   map))

(define-minor-mode bmx-mode
  "Small enhancements for editing batch-files."
  :lighter "bat-ide"
  :global nil
  :keymap bmx-keymap)


(provide 'bmx-mode)

;;; bmx-mode.el ends here
