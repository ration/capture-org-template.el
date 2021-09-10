;;; capture-org-template.el --- Write org capture template in org files  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Tatu Lahtela

;; Author:     Tatu Lahtela <lahtela@iki.fi>
;; Maintainer: Tatu Lahtela <lahtela@iki.fi>
;; Version:    0.0.1
;; Keywords:   lisp, emacs, org-mode
;; Homepage:   https://github.com/ration/capture-org-template.el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses.


;;; Commentary:

;; Write org-capture-templates with a org file

;;; Code:

(defun capture-org-template--drop-one-level (template)
  (replace-regexp-in-string "^*" "" template))

(defun capture-org-template--read-expression (string)
  (car (read-from-string (format "(%s)" string))))

(defun capture-org-template-read-from-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (org-map-entries
     (lambda ()
       (let ((heading (substring-no-properties (org-get-heading)))
             (type (or (org-entry-get nil "TYPE") "entry"))
             (key (org-entry-get nil "KEY"))
             (target (org-entry-get nil "TARGET"))
             (options (or (org-entry-get nil "OPTIONS") "")))
         (unless key (error "All root level headlines must have :KEY: property. '%s' did not" heading))
         (unless target (error "All root level headlines must have :TARGET: property. '%s' did not" heading))
         (outline-next-heading)
         (org-copy-subtree 1)
         (append (list key
                       heading
                       (intern type)
                       (capture-org-template--read-expression target)
                       (capture-org-template--drop-one-level (substring-no-properties (car kill-ring))))
                 (capture-org-template--read-expression options)))
       ) "LEVEL=1" 'nil)))

;; (setq org-capture-templates (capture-org-template-read-from-file "example.org"))

(provide 'capture-org-template)
