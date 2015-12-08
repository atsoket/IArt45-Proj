;;TESTE 3 (teste 21)

(format t "~c[43mTESTE_3~c[0m~%" #\ESC #\ESC)

;;deve retornar IGNORE
(ignore-value (setf t1 (cria-tabuleiro)))
;;deve retornar IGNORE
(ignore-value (dotimes (linha 16) (dotimes (coluna 8) (tabuleiro-preenche! t1 linha coluna))))

(format t "~c[46mTabuleiro:~c[0m~%" #\ESC #\ESC)
(print t1)
(format t "~c[46mPONTOS ALVO: 1700???~c[0m~%" #\ESC #\ESC)
(format t "~c[46mPECAS o l j~c[0m~%" #\ESC #\ESC)

(setf prob0 (make-problema :estado-inicial (make-estado :pontos 0 :tabuleiro t1 :pecas-colocadas () :pecas-por-colocar '(o l j)) :solucao #'solucao :accoes #'accoes :resultado #'resultado :custo-caminho #'qualidade))

(format T "Procura-PP~%")
(format t "~c[31mProcura-PP~c[0m~%" #\ESC #\ESC)
(time (procura-pp prob0))
(format t "~c[31mProcura-A* Qualidade~c[0m~%" #\ESC #\ESC)
(time (procura-A* prob0 #'(lambda (x) 0)))

; (setf prob0 (make-problema :estado-inicial (make-estado :pontos 0 :tabuleiro t1 :pecas-colocadas () :pecas-por-colocar '(o l j)) :solucao #'solucao :accoes #'accoes :resultado #'resultado :custo-caminho #'custo-oportunidade2))
; (format t "~c[31mProcura-A* custo-oportunidade2~c[0m~%" #\ESC #\ESC)
; (time (procura-A* prob0 #'(lambda (x) 0)))

(setf prob0 (make-problema :estado-inicial (make-estado :pontos 0 :tabuleiro t1 :pecas-colocadas () :pecas-por-colocar '(o l j)) :solucao #'solucao :accoes #'accoes :resultado #'resultado :custo-caminho #'custo-oportunidade))
(format t "~c[31mProcura-A* custo-oportunidade0~c[0m~%" #\ESC #\ESC)
(time (procura-A* prob0 #'(lambda (x) 0)))