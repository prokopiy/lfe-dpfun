(defmodule proxima
    (export all))


(defrecord point
    (domain-value 0)
    (codomain-value 0))

(defrecord known
    (period 0)
    (points '()))

(defrecord proxima
    (known (make))
    (fun-proxima #'zero-proxima/1 ))

('proxima #())

(defun zero-proxima [_] 0)
