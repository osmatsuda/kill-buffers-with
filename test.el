;;; -*- lexical-binding: t; -*-

(load-file "kill-buffers-with.el")

(defun test-pattern2regexp ()
  (let ((match #'(lambda (pat test)
		   (= (string-match (pattern2regexp pat) test) 0)))
	(match! #'(lambda (pat test)
		    (null (string-match (pattern2regexp pat) test)))))

    (assert (funcall match "*.{py,pyc}" "foo/bar.py"))
    (assert (funcall match "*.{py,pyc}" "foobar.pyc"))
    (assert (funcall match! "*.{py,pyc}" "pyc"))
    (assert (funcall match! "*.{py,pyc}" "foo.python"))

    (assert (funcall match "\\*info\\*<[0-9]>" "*info*<2>"))
    (assert (funcall match! "\\*info\\*<[0-9]>" "*info*<xxx>"))
    (assert (funcall match! "\\*info\\*<[0-9]>" "*info*"))

    (assert (funcall match "*-??.el" "foo/bar-ai.el"))
    (assert (funcall match! "*-??.el" "foo/bar.el"))
    
    ))

(test-pattern2regexp)
