;;TESTE 2
;; TABULEIRO VAZIO e coloca-se i o o o o objectivo: limpar 2 linhas Pontos 200

(format t "~c[43mTESTE_2~c[0m~%" #\ESC #\ESC)

(format t "~c[46mTabuleiro VAZIO~c[0m~%" #\ESC #\ESC)
(format t "~c[46mPONTOS ALVO: 300~c[0m~%" #\ESC #\ESC)
(format t "~c[46mPECAS I I I I O~c[0m~%" #\ESC #\ESC)

(setf prob0 (make-problema :estado-inicial (make-estado :pontos 0 :tabuleiro (cria-tabuleiro) :pecas-colocadas () :pecas-por-colocar '(i o o o o)) :solucao #'solucao :accoes #'accoes :resultado #'resultado :custo-caminho #'qualidade))


(format t "~c[31mProcura-PP~c[0m~%" #\ESC #\ESC)
(time (procura-pp prob0))

(format t "~c[31mProcura-A* Qualidade~c[0m~%" #\ESC #\ESC)
(time (procura-A* prob0 #'(lambda (x) 0)))

; (setf prob0 (make-problema :estado-inicial (make-estado :pontos 0 :tabuleiro (cria-tabuleiro) :pecas-colocadas () :pecas-por-colocar '(i o o o o)) :solucao #'solucao :accoes #'accoes :resultado #'resultado :custo-caminho #'custo-oportunidade2))
; (format t "~c[31mProcura-A* custo-oportunidade2~c[0m~%" #\ESC #\ESC)
; (time (procura-A* prob0 #'(lambda (x) 0)))

(setf prob0 (make-problema :estado-inicial (make-estado :pontos 0 :tabuleiro (cria-tabuleiro) :pecas-colocadas () :pecas-por-colocar '(i o o o o)) :solucao #'solucao :accoes #'accoes :resultado #'resultado :custo-caminho #'custo-oportunidade))
(format t "~c[31mProcura-A* custo-oportunidade0~c[0m~%" #\ESC #\ESC)
(time (procura-A* prob0 #'(lambda (x) 0)))