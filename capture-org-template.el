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
  "Drop org element in string TEMPLATE on level"
  (replace-regexp-in-string "^*" "" template))

(defun capture-org-template--up-one-level (template)
  "Up org element in string TEMPLATE on level"
  (replace-regexp-in-string "^ " "  " (replace-regexp-in-string "^*" "**" template)))

(defun capture-org-template--read-expression (string)
  "Convert string in STRING to elisp"
  (car (read-from-string (format "(%s)" string))))

(defun capture-org-template--read-body ()
  "Return body text of current headline (after metadata) up to next headline."
  (save-excursion
    (org-back-to-heading t)
    (org-end-of-meta-data t)                  ;; after drawers/planning/blank lines
    (let* ((beg (point))
           (end (save-excursion
                  (or (outline-next-heading) (point-max)))))
      (buffer-substring-no-properties beg end))))

(defun capture-org-template (file)
  "Read capture org-capture-templates from FILE"
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
         (cond
          ;; prefix
          ((string= type "prefix")
           (append (list key heading)
                   (capture-org-template--read-expression options)))

          ;; plain text-y types: item/checkitem/table-line/plain
          ((member type '("item" "checkitem" "table-line" "plain"))
           (when (> (org-outline-level) 1)
             (error "Headline in %s entry!" type))
           (let ((tpl (capture-org-template--read-body)))   ;; ← USE the returned string
             (append (list key heading (intern type)
                           (capture-org-template--read-expression target)
                           tpl)
                     (capture-org-template--read-expression options))))

          ;; default: entry (headline content)
          (t
           (outline-next-heading)
           (if (> (org-outline-level) 1)
               (org-copy-subtree 1)
             (kill-new ""))
           (append (list key heading (intern type)
                         (capture-org-template--read-expression target)
                         (capture-org-template--drop-one-level
                          (substring-no-properties (car kill-ring))))
                   (capture-org-template--read-expression options))))))
     "LEVEL=1" 'nil)))

(defun capture-org-export--string ()
  "Export current org-capture-templates as org string"
  (mapconcat 'identity 
  (seq-map (lambda (template)
             (format
              "* %s\n\
 :PROPERTIES:\n\
 :DESCRIPTION: %s\n\
 :TYPE: %s\n\
 :KEY:      %s\n\
 :TARGET:   %s\n%s\
 :END:\n\
%s\n"
              (nth 1 template)
              (nth 1 template)
              (or (nth 2 template) "prefix")
              (nth 0 template)
              (mapconcat (lambda (x) (format "%S" x)) (nth 3 template) " ")
              (if (nth 5 template)
                  (format " :OPTIONS: %s\n"
                          (mapconcat
                           (lambda (x)
                             (format "%s" x))
                           (subseq template 5) " "))
                "")
              (if (nth 4 template)
              (mapconcat 'capture-org-template--up-one-level
                         (split-string (nth 4 template) "\n") "\n")
              "")))
           org-capture-templates) ""))

(defun capture-org-export-templates-to-org (file)
  "Export current org-capture-templates to FILE"
  (with-temp-buffer
    (insert (capture-org-export--string))
    (write-file file)))

;;(setq org-capture-templates (capture-org-template-read-from-file "~/tmp/foo.org"))

(provide 'capture-org-template)
