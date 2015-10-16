;(load "utils.lisp")

(defun cria-accao (cole apecas)
    (cons cole apecas)
)

(defun accao-coluna (acc)
    (car acc)
)

(defun accao-peca (acc)
    (cdr acc) 
)

(defstruct tabuleiro
    (tabuleiro))

(defun cria-tabuleiro ()
    (make-array '(18 10))
)

(defun tabuleiro-preenchido-p (tab l c)
    (cond ((equal (aref tab l c) NIL) NIL)
    (T T))
)

(defun tabuleiro-linha-completa-p (tab l)
      
    (let ((comp NIL) (c 0))
            (if (not (equal (aref tab l 0) NIL)) (1+ c) (setf comp T))
            (if (not (equal (aref tab l 1) NIL)) (1+ c) (+ c 10))
            (if (not (equal (aref tab l 2) NIL)) (+ c 10) (setf comp T))
            (if (not (equal (aref tab l 3) NIL)) (1+ c) (setf comp T))
            (if (not (equal (aref tab l 4) NIL)) (1+ c) (setf comp T))
            (if (not (equal (aref tab l 5) NIL)) (1+ c) (setf comp T))
            (if (not (equal (aref tab l 6) NIL)) (1+ c) (setf comp T))
            (if (not (equal (aref tab l 7) NIL)) (1+ c) (setf comp T))
            (if (not (equal (aref tab l 8) NIL)) (1+ c) (setf comp T))
            (if (not (equal (aref tab l 9) NIL)) (1+ c) (+ c 10))
    c)
                
)

