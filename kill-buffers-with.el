;;; kill-buffers-with.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2022  osmatsuda

;; Author: osmatsuda;;  <osmatsuda@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Kill buffers at once with a pattern.
;;   https://github.com/osmatsuda/kill-buffers-with

;;; Code:

(require 'cl-lib)

(defun pattern2regexp (pat)
  "Convert a glob pattern to a regular expression."
  (cl-labels
      ((rec (pat-chars result)
            (pcase pat-chars
              ((pred null) (reverse result))
              (`(?* . ,tail) (rec tail (cons '(* not-newline)
                                             result)))
              (`(?? . ,tail) (rec tail (cons 'not-newline
                                             result)))
              ((and `(?\\ . ,tail)
                    (guard (not (null tail))))
               (rec (cdr tail) (cons (char-to-string (car tail))
                                     result)))
              ((and `(?\[ . ,tail)
                    (app (member ?\]) rest)
                    (guard (not (null rest))))
               (rec (cdr rest)
                    (cons (list 'in (concat
                                     (cl-loop for c in tail
                                              until (= c ?\])
                                              collect c)))
                          result)))
              ((and `(?\{ . ,tail)
                    (app (member ?\}) rest)
                    (guard (not (null rest))))
               (rec (cdr rest)
                    (cons (cons 'or
                                (split-string (concat
                                               (cl-loop for c in tail
                                                        until (= c ?\})
                                                        if (not (= c ? )) collect c)) ","))
                          result)))
              (`(,c . ,tail)
               (rec tail (cons (char-to-string c)
                               result)))))
       (exec-rx (pseq)
                (eval `(rx (seq line-start
                                ,@pseq
                                line-end)))))
    (exec-rx
     (rec (string-to-list pat) nil))))

(defun kill-buffers-with--type (name)
  (cond
   ((string= "buffer" name)
    #'(lambda (regexp buffer)
        (string-match regexp (buffer-name buffer))))
   ((string= "mode" name)
    #'(lambda (regexp buffer)
        (let ((mode-name (buffer-local-value 'mode-name buffer)))
          (unless (stringp mode-name) (setq mode-name ""))
          (string-match regexp mode-name))))
   ((string= "file" name)
    #'(lambda (regexp buffer)
        (string-match regexp
                      (abbreviate-file-name
                       (or (buffer-file-name buffer)
                           (buffer-local-value 'list-buffers-directory buffer)
                           "")))))
   (t (error ""))))

(defun kill-buffers-with (pat type)
  (interactive (list (read-string "Pattern: ")
                     (completing-read "Select name type: "
                                      '("buffer" "file" "mode")
                                      nil t)))
  (let* ((regexp (pattern2regexp pat))
         (match (kill-buffers-with--type type))
	 (killed-buffers
	  (cl-loop for b in (buffer-list)
		   for bn = (buffer-name b)
		   when (and (not (string= " " (substring bn 0 1)))
			     (funcall match regexp b))
		   collect bn
		   and do (kill-buffer b)
		   )))
    (when killed-buffers
      (message "Killed buffers %S" killed-buffers))))

(provide 'kill-buffers-with)
;;; kill-buffers-with.el ends here
