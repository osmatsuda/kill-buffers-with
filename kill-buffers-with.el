;;; kill-buffers-with.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2021  osmatsuda

;; Author: osmatsuda;;  <osmatsuda@gmail.com>
;; Keywords: 

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

;; 

;;; Code:

(require 'cl-lib)

(defun pattern2regexp (pat)
  (cl-labels
      ((rec (pat-chars result)
	    (pcase pat-chars
	      ((pred null) (nreverse result))
	      (`(?* . ,tail) (rec tail (cons ".*" result)))
	      (`(?? . ,tail) (rec tail (cons "." result)))
	      ((and `(?\\ . ,tail)
		    (guard (not (null tail))))
	       (rec (cdr tail) (cons (char-to-string (car tail)) result)))
	      ((and `(?\[ . ,tail)
		    (app (member ?\]) rest)
		    (guard (not (null rest))))
	       (rec (cdr rest)
		    (cons (concat
			   "["
			   (cl-loop for c in tail
				    until (= c ?\])
				    collect c)
			   "]")
			  result)))
	      ((and `(?\{ . ,tail)
		    (app (member ?\}) rest)
		    (guard (not (null rest))))
	       (rec (cdr rest)
		    (cons (concat
			   "("
			   (cl-loop for c in tail
				    until (= c ?\})
				    if (= c ?,) collect ?|
				    else if (not (= c ? )) collect c)
			   ")")
			  result)))
	      (`(,c . ,tail)
	       (rec tail (cons (char-to-string c) result))))))
    (rec (string-to-list pat) nil)))

(defun kill-buffers-with (pat type)
  (interactive (list (read-string "Pattern: ")
		     (completing-read "Select name type (defulat: buffer): "
				      '("buffer" "file" "mode")
				      nil t nil nil "buffer")))
  (message "%s %s" pat type))

(provide 'kill-buffers-with)
;;; kill-buffers-with.el ends here

