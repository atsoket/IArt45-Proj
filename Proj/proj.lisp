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

(defun copia-tabuleiro (taboriginal)
    (let ( (tabnovo (make-array '(18 10)) ) )
        (dotimes (linha 18)
            (dotimes (coluna 10)
                (setf (aref tabnovo linha coluna) (aref taboriginal linha coluna))
            )
        )
    tabnovo)
)

(defun tabuleiro-preenchido-p (tab l c)
    (cond ((equal (aref tab l c) NIL) NIL)
    (T T))
)

(defun tabuleiro-altura-coluna (tab c)
    (let ((altura 18) (contador 0))
        (loop while (< contador 18)  do
            (if  (equal (aref tab contador c) NIL) (progn (decf altura) (incf contador)) (setf contador 10))
        )
    altura)
)

(defun tabuleiro-linha-completa-p (tab l)
   (let ((contador 0) (completo T))
        (loop while (< contador 10)  do
            (if  (equal (aref tab l contador) NIL) (progn (setf contador 10) (setf completo NIL)) (incf contador))
        )
    completo)
)

(defun tabuleiro-preenche! (tab l c)
    (setf (aref tab l c) T)
)

(defun tabuleiro-remove-linha! (tabOriginal linhaOriginal) 
       
    (let ((linhaApagar linhaOriginal) (linhaACopiar (- linhaOriginal 1)) )
        (loop while (> linhaApagar 0)  do
            (dotimes (coluna 10)
                (setf (aref tabOriginal linhaApagar coluna) (aref tabOriginal linhaACopiar coluna))
            )
            (decf linhaApagar)
            (decf linhaACopiar)
        )
        (dotimes (coluna 10)
            (setf (aref tabOriginal linhaApagar coluna) NIL)
        )
    )
)

;;MELHORAR
(defun tabuleiro-topo-preenchido-p (tab)
   (let ((contador 0) (completo NIL))
        (loop while (< contador 10)  do
            (if  (equal (aref tab 0 contador) NIL) (incf contador) (progn (setf contador 10) (setf completo T)))
        )
    completo)
)

(defun tabuleiros-iguais-p (tab1 tab2)

    (let ((iguais T))
        (dotimes (linha 18)
            (dotimes (coluna 10)
                (if (equal (aref tab1 linha coluna) (aref tab2 linha coluna)) () (progn (setf iguais NIL) (setf linha 18) (setf coluna 10)))
            )
        )
    iguais)
)

;;ESTA FUNCAO PREENCHE O TBAULEIRO COM LIXO
(defun bota-lixo (taboriginal)
        (dotimes (linha 18)
            (dotimes (coluna 10)
                (setf (aref taboriginal linha coluna) (random-element '(NIL "O")))
            )
        )
)

;;DEVOLVE UM ELEMENTO RANDOM DA LISTA
(defun random-element (list)
  (nth (random (length list)) list))