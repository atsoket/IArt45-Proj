;;TESTE 4 (teste 25)
;; TABULEIRO PARCIALMENTE PREENCHIDO e coloca-se o t i l l Pontos: MAis de 500

(format t "~c[43mTESTE_4~c[0m~%" #\ESC #\ESC)

(ignore-value (setf a1 '#2A((T T T T NIL NIL T T T T)(T T T NIL NIL NIL T T T T)(T T T NIL NIL NIL T T T T)(T T T NIL NIL NIL T T T T)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))))
(setf t1 (array->tabuleiro a1))

(format t "~c[46mTabuleiro:~c[0m~%" #\ESC #\ESC)
(print t1)
(format t "~c[46mPONTOS ALVO: >500~c[0m~%" #\ESC #\ESC)
(format t "~c[46mPECAS T I L L~c[0m~%" #\ESC #\ESC)

(setf prob0 (make-problema :estado-inicial (make-estado :pontos 0 :tabuleiro t1 :pecas-colocadas () :pecas-por-colocar '(t i l l)) :solucao #'solucao :accoes #'accoes :resultado #'resultado :custo-caminho #'qualidade))

(format T "Procura-PP~%")
(format t "~c[31mProcura-PP~c[0m~%" #\ESC #\ESC)
(time (procura-pp prob0))
(format t "~c[31mProcura-A* Qualidade~c[0m~%" #\ESC #\ESC)
(time (procura-A* prob0 #'(lambda (x) 0)))

; (setf prob0 (make-problema :estado-inicial (make-estado :pontos 0 :tabuleiro t1 :pecas-colocadas () :pecas-por-colocar '(t i l l)) :solucao #'solucao :accoes #'accoes :resultado #'resultado :custo-caminho #'custo-oportunidade2))
; (format t "~c[31mProcura-A* custo-oportunidade2~c[0m~%" #\ESC #\ESC)
; (time (procura-A* prob0 #'(lambda (x) 0)))

(setf prob0 (make-problema :estado-inicial (make-estado :pontos 0 :tabuleiro t1 :pecas-colocadas () :pecas-por-colocar '(t i l l)) :solucao #'solucao :accoes #'accoes :resultado #'resultado :custo-caminho #'custo-oportunidade))
(format t "~c[31mProcura-A* custo-oportunidade0~c[0m~%" #\ESC #\ESC)
(time (procura-A* prob0 #'(lambda (x) 0)))