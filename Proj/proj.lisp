;Daniel da Costa  - 69720
;Mario Reis       - 70969
;Filipe Fernandes - 73253
;Grupo 16




;;;;;;;;;;;;;;
;;TIPO ACCAO;;
;;;;;;;;;;;;;;
(defun cria-accao (cole apecas)
    (cons cole apecas)
)

(defun accao-coluna (acc)
    (car acc)
)

(defun accao-peca (acc)
    (cdr acc) 
)


;;;;;;;;;;;;;;;;;;
;;TIPO TABULEIRO;;
;;;;;;;;;;;;;;;;;;

(defstruct tabuleiro
    (tabuleiro))

;;Cria um tabuleiro de 17 linhas e 10 colunas.
(defun cria-tabuleiro ()
    (make-array '(18 10))
)

;;Esta funcao, cria um tabuleiro auxiliar, e conforme percorre o 
;;tabuleiro_original, copia os valores para o auxiliar.
;;No fim retorna o tabuleiro auxiliar.

(defun copia-tabuleiro (taboriginal)
    (let ( (tabnovo (make-array '(18 10)) ) )
        (dotimes (linha 18)
            (dotimes (coluna 10)
                (setf (aref tabnovo linha coluna) (aref taboriginal linha coluna))
            )
        )
    tabnovo)
)


;;Esta funcao verifica se uma dada posicao de um tabuleiro
;; esta' preenchida.
(defun tabuleiro-preenchido-p (tab l c)
    (aref tab l c)
)

;;Percorre uma dada coluna, e retorna a altura assim que encontra uma casa preenchida.
(defun tabuleiro-altura-coluna (tab c)
    (let ((altura 18) (contador 0))
        (loop while (< contador 18)  do
            (if  (equal (aref tab contador c) NIL) (progn (decf altura) (incf contador)) (setf contador 18)) ;(setf contador 18)
        )
    altura)
)

;;esta funcao retorna NIL, assim que encontrar uma posicao vazia.
;;consequentemente interrompe o ciclo, visto que deixa de ser necessa'rio.
(defun tabuleiro-linha-completa-p (tab l)
   (let ((contador 0) (completo T))
        (loop while (< contador 10)  do
            (if  (equal (aref tab l contador) NIL) (progn (setf contador 10) (setf completo NIL)) (incf contador))
        )
    completo)
)

;;preenche uma posicao do tabuleiro.
;;NOTA: Coloquei True, pk ainda nao decidimos que valor vai ser usado.
(defun tabuleiro-preenche! (tab l c)
    (setf (aref tab l c) T)
)


;;escreve os valores da linha imediatamente acima na linha que tem de ser removida.
;;NOTA: A linha do topo e' a zero, e a mais abaixo e' a 17.
(defun tabuleiro-remove-linha! (tabuleiro linha)
    (loop while (> linha 0) do
        (dotimes (coluna 10)
            (setf (aref tabuleiro linha coluna) (aref tabuleiro (- linha 1) coluna))
        )
    (decf linha)
    )
)

;;Nesta funcao, conforme e' encontrado uma posicao preenchida, retorna True.
(defun tabuleiro-topo-preenchido-p (tab)
   (let ((contador 0) (completo NIL))
       (loop while (< contador 10)  do
            (if  (equal (aref tab 0 contador) NIL) (incf contador) (progn (setf contador 10) (setf completo T)))
        )
    completo)
)


;;Esta fucnao, compara posicao a posicao dois tabuleiros,
;;se todos os valores forem iguais, retorna True.
(defun tabuleiros-iguais-p (tab1 tab2)

    (let ((iguais T))
        (dotimes (linha 18)
            (dotimes (coluna 10)
                (if (equal (aref tab1 linha coluna) (aref tab2 linha coluna)) () (progn (setf iguais NIL) (setf linha 18) (setf coluna 10)))
            )
        )
    iguais)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defun tabuleiro->array (tabuleiro) ())
;(defun array->tabuleiro (array) ())
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;
;;TIPO ESTADO;;
;;;;;;;;;;;;;;;

(defstruct estado
  (pontos NIL)
  (pecas_por_colocar NIL)
  (pecas_colocadas NIL)
  (tabuleiro NIL)
)

(defun cria-estado (pontos pecas pecasColocadas tabuleiro)
    (make-estado :pontos pontos :pecas_por_colocar pecas :pecas_colocadas pecasColocadas :tabuleiro tabuleiro)
)


(defun copia-estado (estadoCopiar)
    (make-estado :pontos (estado-pontos estadoCopiar) :pecas_por_colocar (estado-pecas_por_colocar estadoCopiar) :pecas_colocadas (estado-pecas_colocadas estadoCopiar) :tabuleiro (estado-tabuleiro estadoCopiar))
)

;;Esta função recebe dois estados e devolve true caso sejam iguais, caso contrário retorna null

(defun estados-iguais-p (estado1 estado2)
  (and
   (equal (estado-pontos estado1) (estado-pontos estado2))
   (equal (estado-pecas_por_colocar estado1) (estado-pecas_por_colocar estado2))
   (equal (estado-pecas_colocadas estado1) (estado-pecas_colocadas estado2))
   (tabuleiros-iguais-p (estado-tabuleiro estado1) (estado-tabuleiro estado2))
  )

)


(defun estado-final-p (estado)
  (or (equal (estado-pecas_por_colocar estado) 0) (tabuleiro-topo-preenchido-p (estado-tabuleiro estado)))
)

;;;;;;;;;;;;;;;;;
;;TIPO PROBLEMA;;
;;;;;;;;;;;;;;;;;

(defstruct problema ;;os elementos da estrutura sao os resultados das funcoes abaixo - esclarecer duvidas!
    (estado-inicial (make-estado( )))
    (solucao (solucao estado))
    (accoes (accoes estado))
    (resultado (resultado estado accao))
    (custo-caminho (custo-caminho estado))
    
    
)


;;;;;;;;;;;;;
;; FUNCOES ;;
;;;;;;;;;;;;;

(defun solucao (estado)
    (and (not (tabuleiro-topo-preenchido-p (estado-tabuleiro estado))) (equal (estado-pecas_por_colocar estado) 0))
)

;(defun accoes (estado) ( ))

;(defun resultado (estado accao) ( ))

;(defun qualidade (estado) ( ))

;(defun custo-oportunidade (estado) ( ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;DEVOLVE UM ELEMENTO RANDOM DA LISTA
(defun random-element-meu (list)
  (nth (random (length list)) list))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;FUNCOES EXTRA, PARA AJUDAR A CRIAR TABULEIROS PREENCHIDOS;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;ESTA FUNCAO PREENCHE O TBAULEIRO COM LIXO
(defun bota-lixo (taboriginal)
        (dotimes (linha 18)
            (dotimes (coluna 10)
                (setf (aref taboriginal linha coluna) (random-element-meu '(NIL "O")))
            )
        )
)

;;ESTA FUNCAO PREENCHE O TBAULEIRO COM LINHAS
(defun preenche (taboriginal)
        (dotimes (linha 18)
            (dotimes (coluna 10)
                (setf (aref taboriginal linha coluna) linha))
            )
        )

        
        (load "utils.lisp")