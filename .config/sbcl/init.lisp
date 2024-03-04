#-quicklisp
(let ((quicklisp-init (uiop:xdg-data-home
		       #P"quicklisp/setup.lisp")))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
