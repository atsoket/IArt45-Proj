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
    (aref tab (- 17 l) c )
)

;;Percorre uma dada coluna, e retorna a altura assim que encontra uma casa preenchida.
(defun tabuleiro-altura-coluna (tab c)
    (let ((altura 18) (contador 0))
        (loop while (< contador 18)  do
            (if (equal (aref tab contador c) NIL)
                (progn (decf altura) (incf contador))
                (setf contador 18)
            )
        )
    altura)
)

;;esta funcao retorna NIL, assim que encontrar uma posicao vazia.
;;consequentemente interrompe o ciclo, visto que deixa de ser necessa'rio.
(defun tabuleiro-linha-completa-p (tab l)
    (setf l (- 17 l))
    (let (
          (contador 0)
          (completo T))
        (loop while (< contador 10)  do
            (if (equal (aref tab l contador) NIL)
                    (progn (setf contador 10) (setf completo NIL))
                    (incf contador)
            )
    )
    completo)
)

;;preenche uma posicao do tabuleiro.

(defun tabuleiro-preenche! (tab l c)
    (if (and (<= l 17) (<= c 9) )
        (setf (aref tab (- 17 l ) c) T)
        NIL
    )
)


;;escreve os valores da linha imediatamente acima na linha que tem de ser removida.
;;NOTA: A linha do topo e' a zero, e a mais abaixo e' a 17.
(defun tabuleiro-remove-linha! (tabuleiro linha)
    (setf linha (- 17 linha))
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

(defun array->tabuleiro (tabuleiro)
    (let ((tabnovo (make-array '(18 10)) ))
        (dotimes (linha 18)
            (dotimes (coluna 10)
                (if (tabuleiro-preenchido-p tabuleiro linha coluna)
                    (tabuleiro-preenche! tabnovo (- 17 linha) coluna)
                    ()
                )
            )
        )
    tabnovo)
)

(defun tabuleiro->array (tabuleiro)
    ;(copia-tabuleiro tabuleiro)
     (let ((tabnovo (make-array '(18 10)) ))
        (dotimes (linha 18)
            (dotimes (coluna 10)
                (if (tabuleiro-preenchido-p tabuleiro linha coluna)
                    (tabuleiro-preenche! tabnovo (- 17 linha) coluna)
                    ()
                )
            )
        )
    tabnovo)
)


;;;;;;;;;;;;;;;
;;TIPO ESTADO;;
;;;;;;;;;;;;;;;

(defstruct estado
  (pontos 0)
  (pecas-por-colocar NIL)
  (pecas-colocadas NIL)
  (tabuleiro NIL)
)

(defun cria-estado (pontos pecas pecasColocadas tabuleiro)
    (make-estado :pontos pontos :pecas-por-colocar pecas :pecas-colocadas pecasColocadas :tabuleiro tabuleiro)
)

(defun copia-estado (estadoCopiar)
    (make-estado 
    :pontos (estado-pontos estadoCopiar) 
    :pecas-por-colocar (copy-list (estado-pecas-por-colocar estadoCopiar)) 
    :pecas-colocadas (copy-list (estado-pecas-colocadas estadoCopiar)) 
    :tabuleiro (copia-tabuleiro (estado-tabuleiro estadoCopiar)))
)



;;Esta funcao recebe dois estados e devolve true caso sejam iguais, caso contrario retorna null

(defun estados-iguais-p (estado1 estado2)
  (and
   (equal (estado-pontos estado1) (estado-pontos estado2))
   (equal (estado-pecas-por-colocar estado1) (estado-pecas-por-colocar estado2))
   (equal (estado-pecas-colocadas estado1) (estado-pecas-colocadas estado2))
   (tabuleiros-iguais-p (estado-tabuleiro estado1) (estado-tabuleiro estado2))
  )
)


(defun estado-final-p (estado)
  (or (equal (estado-pecas-por-colocar estado) nil) (tabuleiro-topo-preenchido-p (estado-tabuleiro estado)))
)

;;;;;;;;;;;;;;;;;
;;TIPO PROBLEMA;;
;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;NOTAS                                                                          ;;;
;;;                                                                               ;;;
;;;Imaginando que algue'm ja' criou um estado.                                    ;;;
;;;Quando chamam o make-problema este e' enunciado da seguinte forma:              ;;;
;;;(setf probex (make-problema :estado-inicial e1))                               ;;;
;;;Ele cria um problema e dentro do estado-inicial tem o estado(como deve de ser) ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct problema
    (estado-inicial)
    (solucao) 
    (accoes)
    (resultado)
    (custo-caminho)
)

; ;;;;;;;;;;;;;
; ;; FUNCOES ;;
; ;;;;;;;;;;;;;

(defun solucao (_estado)
    (and (not (tabuleiro-topo-preenchido-p (estado-tabuleiro _estado))) (equal (estado-pecas-por-colocar _estado) nil))      
)

;;;Funcao: accoes
;;;lista_accoes->Lista que vai guardar as accoes possiveis de realizar
;;;lista_pecas->Lista que contem as pecas a serem testadas
;;;limite->ultima coluna aonde e' possivel inserir a peca
(defun accoes (_estado)
    (let ( (lista_accoes (list))
           (lista_pecas (escolhe_peca (car (estado-pecas-por-colocar _estado))))
           (limite 0)
          )
          (dolist (peca_actual lista_pecas)   ;;itera a lista de pecas a testar

              (setf limite (- 11 (array-dimension peca_actual 1))) ;;calculo da ultima coluna aonde e' possivel inserir a peca

              (dotimes (coluna limite)
                  (push (cria-accao coluna peca_actual) lista_accoes)   ;;adiciona o par accao a' lista de accoes possiveis
              )
          )
            (cond 
              ( (estado-final-p _estado) (setf lista_accoes '()))
            )
          (reverse lista_accoes)
    )
)


(defun escolhe_peca (_letra)
    (cond
      ((equal _letra 'i) (list peca-i0 peca-i1) )
      ((equal _letra 'l) (list peca-l0 peca-l1 peca-l2 peca-l3) )
      ((equal _letra 'j) (list peca-j0 peca-j1 peca-j2 peca-j3) )
      ((equal _letra 'o) (list peca-o0) )
      ((equal _letra 's) (list peca-s0 peca-s1) )
      ((equal _letra 'z) (list peca-z0 peca-z1) )
      ((equal _letra 't) (list peca-t0 peca-t1 peca-t2 peca-t3) )
    (T (list)))
)


(defun resultado (_estado _accao)
     (let ( 
            (_estado_resultado nil)
            (largura_peca (array-dimension (accao-peca _accao) 1)) 
            (altura_peca (array-dimension (accao-peca _accao) 0))
            (peca nil)
            (coluna_alvo -1)
            (actual -1)
            (col_array -1)
            (maximus -1)
            (linha_alvo -1)
            (conta_pontos 0)
          )
          
          
          
          (setf _estado_resultado (copia-estado _estado))
          (setf peca  (accao-peca _accao) )
          (setf coluna_alvo (accao-coluna _accao))
          
         ;;;calcula a altura onde desenhar a peca
         
         ;iterar as colunas da peca + coluans do tabuleiro e somaxr
         ;coluna peca + (coluna_alvo + coluna_peca) 
         
         (dotimes (c largura_peca)
            (setf actual (+  (aux-peca-altura-coluna peca c) (tabuleiro-altura-coluna (estado-tabuleiro _estado) (+ coluna_alvo c))))
            (if (< maximus actual) (progn (setf col_array c) (setf maximus actual)) ())
         )
         
         (setf linha_alvo (- (+ (aux-peca-altura-coluna peca col_array) (tabuleiro-altura-coluna (estado-tabuleiro _estado) (+ coluna_alvo col_array))) 1))
         
         ;col_array = coluna CHEFE A.K.A KITAMANDA
        
        ;A cada posicao do array temos a altura correspondente (a soma da altura da peca com a da coluna).
        
    
;;          ;;;desenha a peca no tabuleiro
          (dotimes (_linha_peca altura_peca)
            (dotimes (_coluna_peca largura_peca)             
                 (if (aref peca (- (- altura_peca 1) _linha_peca) _coluna_peca)
                      (tabuleiro-preenche! (estado-tabuleiro _estado_resultado) (- linha_alvo _linha_peca) (+ coluna_alvo _coluna_peca))
                      ()

                 )
             )
         )
         
         (if (tabuleiro-topo-preenchido-p (estado-tabuleiro _estado_resultado))
            _estado_resultado
            ()
         )
        
;;       ;;;verifica se ha linhas preenchidas e remove-as
         (dotimes (_linha 18)
              
              (if (tabuleiro-linha-completa-p (estado-tabuleiro _estado_resultado) (- 17 _linha))
                  (progn (tabuleiro-remove-linha! (estado-tabuleiro _estado_resultado) (- 17 _linha)) (incf conta_pontos) )
              )
         )         

         (cond
          ((= conta_pontos 0) ())
          ((= conta_pontos 1) (setf (estado-pontos _estado_resultado) (+ (estado-pontos _estado_resultado) 100)) )
          ((= conta_pontos 2) (setf (estado-pontos _estado_resultado) (+ (estado-pontos _estado_resultado) 300)) )
          ((= conta_pontos 3) (setf (estado-pontos _estado_resultado) (+ (estado-pontos _estado_resultado) 500)) )
          ((= conta_pontos 4) (setf (estado-pontos _estado_resultado) (+ (estado-pontos _estado_resultado) 800)) )
         )
         
         (push (pop (estado-pecas-por-colocar _estado_resultado)) (estado-pecas-colocadas _estado_resultado))
         
      _estado_resultado
      
     )
    
      
)


 (defun aux-peca-altura-coluna (_peca coluna)
     (let (  (altura (array-dimension _peca 0))   (contador 0) )          
           (loop while (<= contador (array-dimension _peca 0) ) do
             (if (equal (aref _peca contador coluna ) nil)
                 (progn (decf altura) (incf contador))
                 (setf contador 18)
             )
         )
     altura
     )
 )



 ; (defun aux-peca-altura-coluna (_peca coluna)
 ;     (let (  (altura (array-dimension _peca 0))   (contador (array-dimension _peca 0)) )          
 ;           ; (loop while (>= contador 0) do
 ;           ;   (if (equal (aref _peca (- contador 1) coluna ) NIL)
 ;           ;       (progn (decf altura)(decf contador))
 ;           ;       (setf contador -1)
 ;           ;   )
 ;          (dotimes (altura_coluna contador)
 ;              (if (equal (aref _peca altura_coluna coluna) NIL)
 ;                (decf altura)
 ;                ())
 ;          )

 ;     altura
 ;     )
 ; )

;; ;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun qualidade (_estado)
    (- 0 (estado-pontos _estado))
    
)

(defun custo-oportunidade (_estado)
     (let (      (custo (* (length (estado-pecas-colocadas _estado)) 300)) )
         (dolist (_peca (estado-pecas-colocadas _estado))
             (cond ;;verificar como funciona a comparacao das pecas
                 ( (or (equal _peca 'j) (equal _peca 'l)) (setf custo (+ custo 200)))
                 ( (equal _peca  'i) (setf custo (+ custo 500)))
             )
         )
     (- custo (estado-pontos _estado))
     )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROCURAS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun procura-pp (_problema)
  (reverse (procura-pp-inicial _problema (problema-estado-inicial _problema) (list)))
  ; (let ((listinha (list)))
  ; (push (estado-pecas-por-colocar (problema-estado-inicial _problema)) listinha)
  ; (dotimes (capa 10)
  ;   (push (tabuleiro-altura-coluna (estado-tabuleiro (problema-estado-inicial _problema)) capa) listinha) )
  ; listinha)
)


(defun procura-pp-inicial (_problema _estado _caminho)
  ;(print _estado)
    (cond
        ;((solucao _estado) _caminho) ;se o no' e' solucao, devolve o caminho: novo campo na estrutura problema
        ((funcall (problema-solucao _problema) _estado) _caminho) ;se o no' e' solucao, devolve o caminho: novo campo na estrutura problema
      ;  ((estado-final-p _estado) nil ) ;se e' estado final, nao faz nada e continua a procura (nota que a procura e' recursiva)
        (t
           ; (sucessor_recursivo _problema _estado _caminho  (reverse (accoes _estado)))
             (sucessor_recursivo _problema _estado _caminho  (reverse (funcall (problema-accoes _problema) _estado)))

        )
    )
)

(defun sucessor_recursivo (_problema _estado _caminho _movimentos)

          (cond ((null _movimentos) nil)
          ;  (T (let (  (filho (funcall (problema-resultado _problema) (copia-estado _estado) (car _movimentos)) ) )
              (T (let (  (filho (funcall (problema-resultado _problema) _estado (car _movimentos)) ) )
                  (if filho
                    (or (procura-pp-inicial _problema filho (push (car _movimentos) _caminho))
                        (sucessor_recursivo _problema _estado (cdr _caminho) (cdr _movimentos)) ) ;Falso
                    (sucessor_recursivo _problema _estado (cdr _caminho) (cdr _movimentos)))))
                                                
              )
          )
  
(defun procura-A* (_problema _heuristica)
        (let  (
                (best 99999)
                (caminho (list) )
                (actual 0)
                (_estado (problema-estado-inicial _problema))
                (_estado_resultado nil)
                (_resultadoaux nil)
                (_accaoaux nil )
                (_accoes nil)
                (_semsolucao t)
                (_custo 0)
                )
                
               (loop while (and (not (solucao _estado)) _semsolucao) do
                (format T "---------------------------------------------")
                (format T "---------------------------------------------")
                        (setf _accoes (funcall (problema-accoes _problema) _estado))
               
                        
                
                        (dolist (_accao  _accoes)
                                (setf _resultadoaux (funcall (problema-resultado _problema)  _estado _accao))
                               ;(setf actual (+ (- 0 (qualidade _resultadoaux)) (funcall _heuristica  _resultadoaux)))
                               
                               (setf _custo (funcall (problema-custo-caminho _problema) _resultadoaux))
                               (print  _custo )

                               (setf actual (+  _custo (funcall _heuristica  _resultadoaux)))
                               
                                (format T "Actual: ")
                                (print actual)
                                 (format T "~%Accao: ")
                                (print _accao)
                                (print (estado-tabuleiro _resultadoaux))
                                (format T "~%----------------~%")
                                (if (<= actual best)
                                        (progn (setf best actual)  (setf _estado_resultado (copia-estado  _resultadoaux)) (setf _accaoaux _accao))
                                        ()
                                )
                        )
                        
                        (setf _estado (copia-estado _estado_resultado))
                        (push _accaoaux caminho)
  
                        (cond ((null _accoes) (setf _semsolucao nil) (setf caminho nil))
                          (T ()))
                )
        (reverse caminho)
        )
)
;(procura-A* meu #'(lambda (x) 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
                (setf (aref taboriginal linha coluna) linha)
            )peca-t1
        )
)

        
        ;(load "utils.lisp")
        ;(load (compile-file "utils.lisp"))
       (load "utils.fas")


