;;;; foo-lisp.lisp

(in-package #:foo-lisp)

;; (defun say (text)
;;   (inferior-shell:run (concatenate 'string "flite --sets join_type=simple_join -voice slt -t '" text "'")
;;                       :on-error nil
;;                       :error-output :string
;;                       :output nil))
