(defmodule dpfun
  (export all))

;;; -----------
;;; library API
;;; -----------
; Discrete Periodic Function

; (list 'dpfun Period ValuesList InterpolationFun)




(defun new
  ([Period ApproximationFun] (when (and (is_number Period) (is_function ApproximationFun))) 
      (list 'dpfun (abs Period) '() ApproximationFun)))

(defun new
  ([Period] (when (is_integer Period)) 
      (list 'dpfun (abs Period) '() #'zero/2)))

(defun new
  ([] (when) (list 'dpfun 0 '() #'zero/2)))



(defun get-points
  ([(cons 'dpfun (list _ points _))] (when) points))

(defun get-period
  ([(cons 'dpfun (list period _ _))] (when) period))


(defun set-period
  ([(list 'dpfun _ points afunc) new-period] (when) (list 'dpfun new-period points afunc)))


(defun add-point
  ([(list 'dpfun period points afunc) new-point] (when (is_list new-point)) 
      (if (lists:member new-point points) 
        (list 'dpfun period points afunc) 
        (list 'dpfun period (++ points (list new-point)) afunc))))


; ApproximationFun определяет значение функции в зависимости от известных значений
; Входные значения: Х, список пар [Xi Yi] (пары - тоже список из 2х значений)

(defun rem-float [x y]
  (- x (* y (floor (/ x y)))))

(defun number-to-zero-period [x period]
  (rem-float x (abs period)))

(defun point-to-zero-period
  ([(list x y) period] (when) (list (rem-float x (abs period)) y)))

(defun points-to-zero-period
  ([points period] (when) (lists:map (lambda (x) (point-to-zero-period x period)) points)))



(defun zero [_ _]
  0)


(defun get-nearest-left-point-from-list
  (('() t points) (get-nearest-left-point-from-list (list 0 0) t points))
  ((last _ '()) last)
  ((last t (cons head tail))
    (if (and (=< (car head) t) (> (car head) (car last))) 
      (get-nearest-left-point-from-list head t tail) 
      (get-nearest-left-point-from-list last t tail))))


(defun my-fun ()
  'hello-world)
