(in-package :heavy-bool-examples)

(some #'evenp (range 1 10))
(some (lambda (x) (when (evenp x) x)) (range 1 10))

(some (lambda (x) (when (not x) x)) (list 1 2 3 4 5))
(some (lambda (x) (when (not x) x)) (list 1 2 nil 3 4 5))
