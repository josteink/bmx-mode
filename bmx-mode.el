;;; tide.el --- Typescript Interactive Development Environment -*- lexical-binding: t -*-

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
  nil)

(defun bmx--variable-at-point ()
  nil)

(defun bmx-find-references-at-point ()
  ;; YES DO THIS TODAY!
  (interactive)
  (cond ((bmx--label-at-point) (bmx--find-label-references (bmx--label-at-point)))
        ((bmx--variable-at-point) (bmx--find-variable-references (bmx--variable-at-point))
         (t (message "No referencable symbol found at point!")))))

(defun bmx--navigate-to-symbol-at-point ()
  ;; YES DO THIS TODAY!
  (interactive)
  (cond ((bmx--label-at-point) (bmx--navigate-to-label (bmx--label-at-point)))
        ((bmx--variable-at-point) (bmx--navigate-to-variable (bmx--variable-at-point))
         (t (message "No referencable symbol found at point!")))))


(setq bmx-keymap (let ((map (make-sparse-keymap)))
                         (define-key map (kbd ":") #'bmx-insert-label)
                         (define-key map (kbd "%") #'bmx-insert-variable)
                         map))

(define-minor-mode bmx-mode
  "Small enhancements for editing batch-files."
  :lighter "bat-ide"
  :global nil
  :keymap bmx-keymap)


;; (popup-menu '("a" "b" "c"))

;; (company-complete)

;; (setq edebug-on-error t)


(provide 'bmx-mode)

;;; bmx-mode.el ends here
