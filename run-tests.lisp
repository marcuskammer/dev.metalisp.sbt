#!/usr/bin/sbcl --script

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :dev.metalisp.sbt/tests)
(asdf:test-system :dev.metalisp.sbt/tests)
