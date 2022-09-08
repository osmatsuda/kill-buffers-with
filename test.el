;;; -*- lexical-binding: t; -*-

(require 'cl-macs)

(load-file "kill-buffers-with.el")

(defun test-pattern2regexp ()
  (let ((match #'(lambda (pat test)
		   (= (string-match (pattern2regexp pat) test) 0)))
	(match! #'(lambda (pat test)
		    (null (string-match (pattern2regexp pat) test)))))

    (cl-assert (funcall match "*.{py,pyc}" "foo/bar.py"))
    (cl-assert (funcall match "*.{py,pyc}" "foobar.pyc"))
    (cl-assert (funcall match! "*.{py,pyc}" "pyc"))
    (cl-assert (funcall match! "*.{py,pyc}" "foo.python"))

    (cl-assert (funcall match "\\*info\\*<[0-9]>" "*info*<2>"))
    (cl-assert (funcall match! "\\*info\\*<[0-9]>" "*info*<xxx>"))
    (cl-assert (funcall match! "\\*info\\*<[0-9]>" "*info*"))

    (cl-assert (funcall match "*-??.el" "foo/bar-ai.el"))
    (cl-assert (funcall match! "*-??.el" "foo/bar.el"))
    
    ))

(test-pattern2regexp)
