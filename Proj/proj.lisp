;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grupo 16                 ;;
;; Daniel da Costa  - 69720 ;;
;; Mario Reis       - 70969 ;;
;; Filipe Fernandes - 73253 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;
;; TIPO ACCAO ;;
;;;;;;;;;;;;;;;;

(defun cria-accao (_coluna _peca)
    (cons _coluna _peca)
)

(defun accao-coluna (_accao)
    (car _accao)
)

(defun accao-peca (_accao)
    (cdr _accao)
)

;;;;;;;;;;;;;;;;;;;;
;; TIPO TABULEIRO ;;
;;;;;;;;;;;;;;;;;;;;

;Cria um tabuleiro de 17 linhas e 10 colunas.
(defun cria-tabuleiro ()
    (make-array '(18 10))
)

;Cria um tabuleiro auxiliar, percorre o tabuleiro_original, copia o seu conteudo para o auxiliar e devolve o auxiliar
(defun copia-tabuleiro (_taboriginal)
    (let (
          (tabnovo (make-array '(18 10))) 
         )
        (dotimes (linha 18)
            (dotimes (coluna 10)
                (setf (aref tabnovo linha coluna) (aref _taboriginal linha coluna))
            )
        )
    tabnovo
	  )
)

(defun tabuleiro-preenchido-p (_tabuleiro _linha _coluna)
    (aref _tabuleiro (- 17 _linha) _coluna )
)

;;Percorre uma coluna a partir do topo, e retorna a sua altura assim que encontra uma posicao preenchida.
(defun tabuleiro-altura-coluna (_tabuleiro _coluna)
    (let (
          (altura 18)
          (contador 0)
         )
        (loop while (< contador 18)  do
            (if (equal (aref _tabuleiro contador _coluna) NIL)
                (progn (decf altura) (incf contador))
                (setf contador 18)
            )
        )
    altura
	)
)

(defun tabuleiro-linha-completa-p (_tabuleiro _linha)
    
    (setf _linha (- 17 _linha)) ;Desta forma, quando se entra no if do loop, nao e' calculado (- 17 _linha). O calculo e' efectuado uma unica vez.
    (let(
      		(contador 0)
      		(completo T)
    		)
        (loop while (< contador 10)  do
            (if	(equal (aref _tabuleiro _linha contador) NIL)	;se encontra uma posicao vazia termina a execucao e devolve NIL
                (progn (setf contador 10) (setf completo NIL))
                (incf contador)
            )
		    )
    completo
	)
)

(defun tabuleiro-preenche! (_tabuleiro _linha _coluna)
    (if (and (<= _linha 17) (<= _coluna 9))
        (setf (aref _tabuleiro (- 17 _linha ) _coluna) T)
        NIL
    )
)

;escreve a linha acima da linha a remover na linha a remover, e faz o mesmo para cada outra linha acima desta
;NOTA: A linha do topo corresponde a 0 e a do fundo corresponde a 17
(defun tabuleiro-remove-linha! (tabuleiro linha)
    (setf linha (- 17 linha))
    (loop while (> linha 0) do
        (dotimes (coluna 10)
            (setf (aref tabuleiro linha coluna) (aref tabuleiro (- linha 1) coluna))
        )
		(decf linha)
    )
)

(defun tabuleiro-topo-preenchido-p (tab)
   (let (
          (contador 0)
          (completo NIL)
        )
       (loop while (< contador 10)  do
            (if (equal (aref tab 0 contador) NIL)	;se encontra uma posicao preenchida termina a execucao e devolve TRUE
				        (incf contador)
				        (progn (setf contador 10) (setf completo T))
			      )
       )
    completo
	)
)

;Compara cada posicao de cada tabuleiro e devolve TRUE se todas as posicoes forem iguais
(defun tabuleiros-iguais-p (tab1 tab2)
    (let ((iguais T))
        (dotimes (linha 18)
            (dotimes (coluna 10)
                (if (equal (aref tab1 linha coluna) (aref tab2 linha coluna))	;se encontra uma posicao diferente termina a execucao e devolve NIL
					           ()
					           (progn (setf iguais NIL) (setf linha 18) (setf coluna 10))
                )
            )
        )
    iguais
    )
)

(defun array->tabuleiro (tabuleiro)
    (let (
          (tabnovo (make-array '(18 10)))
         )
          (dotimes (linha 18)
            (dotimes (coluna 10)
                (if (tabuleiro-preenchido-p tabuleiro linha coluna)
                    (tabuleiro-preenche! tabnovo (- 17 linha) coluna)
                    ()
                )
            )
          )
    tabnovo
	  )
)

(defun tabuleiro->array (tabuleiro)
     (let (
            (tabnovo (make-array '(18 10)))
          )
          (dotimes (linha 18)
              (dotimes (coluna 10)
                  (if (tabuleiro-preenchido-p tabuleiro linha coluna)
                      (tabuleiro-preenche! tabnovo (- 17 linha) coluna)
                      ()
                  )
              )
          )
      tabnovo
	   )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;
;; TIPO ESTADO ;;
;;;;;;;;;;;;;;;;;

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;
;; TIPO PROBLEMA ;;
;;;;;;;;;;;;;;;;;;;

(defstruct problema
    (estado-inicial)	
    (solucao) 
    (accoes)
    (resultado)
    (custo-caminho)
)


;;;;;;;;;;;;;
;; FUNCOES ;;
;;;;;;;;;;;;;

;Um estado e solucao se ja nao houverem pecas por colocar e se o topo do tabuleiro nao estiver preenchido
(defun solucao (_estado)
    (and
  		(not (tabuleiro-topo-preenchido-p (estado-tabuleiro _estado)))
  		(equal (estado-pecas-por-colocar _estado) nil)
	  )      
)



(defun accoes (_estado)
    (let	(
      			(lista_accoes (list))													;lista que ira conter as accoes possiveis de realizar
      			(lista_pecas (escolhe_peca (car (estado-pecas-por-colocar _estado))))	;lista que contem as rotacoes possiveis da peca a colocar no tabuleiro
      			(limite 0)																;ultima coluna onde vai ser possivel colocar a peca
      		)
			
    			(dolist (peca_actual lista_pecas)   							;para cada peca da lista de rotacoes possiveis a testar
    				(setf limite (- 11 (array-dimension peca_actual 1)))		;calcula a ultima coluna aonde e possivel colocar a peca

    				(dotimes (coluna limite)									;para cada coluna 
    					(push (cria-accao coluna peca_actual) lista_accoes)		;adiciona o par accao a lista de accoes possiveis
    				)
    			)


          (if (estado-final-p _estado)	;se o estado 
  				    (setf lista_accoes '())
          )

    (reverse lista_accoes)
    )
)

;para cada peca devolve as possiveis rotacoes da mesma
(defun escolhe_peca (_letra)
    (cond
		((equal _letra 'i) (list peca-i0 peca-i1) )
		((equal _letra 'l) (list peca-l0 peca-l1 peca-l2 peca-l3) )
		((equal _letra 'j) (list peca-j0 peca-j1 peca-j2 peca-j3) )
		((equal _letra 'o) (list peca-o0) )
		((equal _letra 's) (list peca-s0 peca-s1) )
		((equal _letra 'z) (list peca-z0 peca-z1) )
		((equal _letra 't) (list peca-t0 peca-t1 peca-t2 peca-t3) )
		(T (list))
	)
)

(defun resultado (_estado _accao)
    (let( 
          (peca nil)													;peca a ser colocada
          (coluna_alvo -1)											;coluna do tabuleiro onde a peca esta a ser colocada
  		    (linha_alvo -1)												;linha do tabuleiro a partir da qual vai ser desenhada a peca
  		    (largura_peca (array-dimension (accao-peca _accao) 1)) 		;largura do array da peca
          (altura_peca (array-dimension (accao-peca _accao) 0))		;altura do array da peca
  			
          (actual -1)													;a soma entre a altura do tabuleiro e a altura da peca na coluna a ser analisada										
          (coluna_max -1)												;coluna da peca onde o valor da soma entre a altura do tabuleiro e a altura da peca e maxima
          (maximus -1)												;maximo da soma entre as alturas do tabuleiro e as alturas da peca
              
  		    (conta_linhas_removidas 0)									;numero de linhas removidas com a accao
  		    (_estado_resultado nil)										;estado que resulta de aplicar a accao ao estado original
		    )

    		(setf _estado_resultado (copia-estado _estado))
    		(setf peca  (accao-peca _accao) )
    		(setf coluna_alvo (accao-coluna _accao))
          
    		;Calcula a linha onde a peca vai ser desenhada, obtendo o maximo das somas entre a altura do tabuleiro e a altura da peca em cada coluna
    		(dotimes (coluna_peca largura_peca)	;percorre a largura da peca
            (setf actual (+  (aux-peca-altura-coluna peca coluna_peca) (tabuleiro-altura-coluna (estado-tabuleiro _estado) (+ coluna_alvo coluna_peca))))
            (if (< maximus actual)
    				    (progn (setf coluna_max coluna_peca) (setf maximus actual))
    				    ()
    			   )
        )
         
        (setf linha_alvo (- (+ (aux-peca-altura-coluna peca coluna_max) (tabuleiro-altura-coluna (estado-tabuleiro _estado) (+ coluna_alvo coluna_max))) 1))
        
		;desenha a peca no tabuleiro
        (dotimes (_linha_peca altura_peca)
            (dotimes (_coluna_peca largura_peca)             
                 (if (aref peca (- (- altura_peca 1) _linha_peca) _coluna_peca)
                    (tabuleiro-preenche! (estado-tabuleiro _estado_resultado) (- linha_alvo _linha_peca) (+ coluna_alvo _coluna_peca))
                    ()
                )
            )
        )

		;se o o topo do tabuleiro esta preenchido, devolve o estado        
        (if (tabuleiro-topo-preenchido-p (estado-tabuleiro _estado_resultado))
            _estado_resultado
            ()
        )
        
		;verifica se ha linhas preenchidas, caso haja, remove-as e atualiza a pontuacao
        (dotimes (_linha 18)
            (if (tabuleiro-linha-completa-p (estado-tabuleiro _estado_resultado) (- 17 _linha))
                (progn (tabuleiro-remove-linha! (estado-tabuleiro _estado_resultado) (- 17 _linha)) (incf conta_linhas_removidas) )
            )
        )

        (cond
      			((= conta_linhas_removidas 0) ())
      			((= conta_linhas_removidas 1) (setf (estado-pontos _estado_resultado) (+ (estado-pontos _estado_resultado) 100)) )
      			((= conta_linhas_removidas 2) (setf (estado-pontos _estado_resultado) (+ (estado-pontos _estado_resultado) 300)) )
      			((= conta_linhas_removidas 3) (setf (estado-pontos _estado_resultado) (+ (estado-pontos _estado_resultado) 500)) )
      			((= conta_linhas_removidas 4) (setf (estado-pontos _estado_resultado) (+ (estado-pontos _estado_resultado) 800)) )
        )

		    ;remove a peca da lista de pecas por colocar e insere-a na lista de pecas colocadas		
		    (push (pop (estado-pecas-por-colocar _estado_resultado)) (estado-pecas-colocadas _estado_resultado))
			
    _estado_resultado
    )
)

;Esta funcao auxiliar devolve a altura de uma peca na sua coluna correspondente
;O processo e' semelhante a calcular a altura de uma coluna do tabuleiro, porem aplicado ao array de uma peca
(defun aux-peca-altura-coluna (_peca _coluna)
    (let(
      		(altura (array-dimension _peca 0))
      		(contador 0)
		    )
			
    		(loop while (<= contador (array-dimension _peca 0) ) do
      			(if (equal (aref _peca contador _coluna ) nil)
      				(progn (decf altura) (incf contador))
      				(setf contador 18)
      			)
    		)

     altura
    )
)

(defun qualidade (_estado)
    (- 0 (estado-pontos _estado)) 
)

(defun custo-oportunidade (_estado)
    (let(
          (custo (* (length (estado-pecas-colocadas _estado)) 300))
        )
        
        (dolist (_peca (estado-pecas-colocadas _estado))
                (cond 
                    ((or (equal _peca 'j) (equal _peca 'l)) (setf custo (+ custo 200)))
                    ((equal _peca  'i) (setf custo (+ custo 500)))
                )
        )

    (- custo (estado-pontos _estado))
    )
)

(defun custo-oportunidade2 (_estado)
    ; (let(
		  ;     (custo (* (length (estado-pecas-colocadas _estado)) 300))
		  ;   )
        
    ; 		(dolist (_peca (estado-pecas-colocadas _estado))
    ;             (cond 
    ;                 ((or (equal _peca 'j) (equal _peca 'l)) (setf custo (+ custo 200)))
    ;                 ((equal _peca  'i) (setf custo (+ custo 500)))
    ;             )
    ;     )

    ; (- custo (estado-pontos _estado))
    ; )

        (let(
          ;(custo (* (length (estado-pecas-colocadas _estado)) 300))
          (custo 0)
          (lista_pecas (estado-pecas-colocadas _estado))
        )
        
        (dolist (_peca lista_pecas)
                (cond 
                    ((or (equal _peca 'j) (equal _peca 'l)) (setf custo (+ custo 500)))
                    ((equal _peca  'i) (setf custo (+ custo 800)))
                    (T (setf custo (+ custo 300)))
                )
        )

    (- custo (estado-pontos _estado))
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;
;; PROCURAS ;;
;;;;;;;;;;;;;;

;;
;; procura-pp - Esta funcao serve como "proxy", como precisavamos de uma lista para guardar o caminho
;;              optamos por criar uma "segunda" procura-pp.
;;
(defun procura-pp (_problema)
  (reverse (procura-pp-inicial _problema (problema-estado-inicial _problema) (list)))
)

;;
;; procura-pp-inicial - Esta e a funcao "oficial", recebe um problema, um estado e um caminho.
;;                      comeca por verifica se o estado e solucao, em caso aformativo retorna o caminho,
;;                      em caso negativo chama a funcao sucessor.
;;

(defun procura-pp-inicial (_problema _estado _caminho)
    (cond
        ((funcall (problema-solucao _problema) _estado) (format t "~c[41mPONTOS:~c[0m" #\ESC #\ESC) (print (estado-pontos _estado)) _caminho)
        (t
            (sucessor _problema _estado _caminho  (reverse (funcall (problema-accoes _problema) _estado)))
        )
    )
)

;;
;; sucessor - Esta funcao recebe um problema, um estado, uma lista que e o caminho e uma lista de movimentos.
;;            Primeiramente testamos se a lista de movimentos esta vazia, em caso afirmativo retornamos nil.
;;            Caso a lista contenha movimentos, comecamos por aplicar um movimento ao estado recebido, de seguida
;;            chamamos a procura-pp, para continuar a "seguir o 1o filho", caso a procura-pp retorne nil, testamos
;;            o filho seguinte(do mesmo pai). Quando se esgota os filhos do pai, subimos e comecamos a testar os irmaos do pai.
;;

(defun sucessor (_problema _estado _caminho _movimentos)
    (cond
		      ((null _movimentos) nil)
          (T (let (
					         (filho (funcall (problema-resultado _problema) _estado (car _movimentos)))
                  )
                
          				(if filho
                      (or (procura-pp-inicial _problema filho (push (car _movimentos) _caminho))
                          (sucessor _problema _estado (cdr _caminho) (cdr _movimentos))) 
                          (sucessor _problema _estado (cdr _caminho) (cdr _movimentos))  
          				)
              )
          )
    )
)

;;
;; Procura-A* - Nesta procura usamos uma hashtable, em que a chave e' o f(n) do no', e la' dentro colocamos uma lista
;;              de estados que possuem esse custo. Como fazemos push la' para dentro, o 1o "par" ((estado, custo), caminho), foi o ultimo a ser gerado.
;;              De forma auxilair, possuimos uma lista com as chaves, que reordenamos por ordem crescente, antes de seleccionarmos
;;              qual vai se ro no' seguinte. 
;;              Quando queremos o no' seguinte, pedimos o 1o elemento da lista de chaves e de seguida fazemos gethash para obter o "par"
;;
;;



(defun procura-A* (_problema _heuristica)
	(let(
    		(_ListaAccoes)
    		(caminho (list))								;lista de accoes que correspondem ao caminho desde o estado inicial ate ao estado atual
    		(_estado (problema-estado-inicial _problema))	;estado inicial do problema
    		(_par_resultado nil)
    		(_resultadoaux nil)
    		(_semsolucao t)
    		(_custo 0)										;custo de ir do estado inicial ate ao estado atual
    		(hashtb (make-hash-table))						;hash table que contem os vertices por expandir enderecados pelo custo
    		(_listacustos (list))							;lista dos custos dos vertices por expandir (um valor por custo)
    		(_accaoaux nil)
    		(_accaoOLD nil)
  		)

		(loop while _semsolucao do

			(if (funcall (problema-solucao _problema) _estado)
			  
				;se o estado e solucao
				(progn (setf caminho (copy-list _accaoOLD)) (setf _semsolucao nil))
				  
				;se o estado nao e solucao
				(progn	(setf _ListaAccoes (funcall (problema-accoes _problema) _estado))
						(cond
							((null _ListaAccoes) ())
							(t 
								(dolist (_accao  _ListaAccoes) ;ITERAR A LISTA DE ACCOES
								  
									(setf _resultadoaux (funcall (problema-resultado _problema)  _estado _accao));FILHO GERADO
									(setf _custo  (+ (funcall (problema-custo-caminho _problema) _resultadoaux) (funcall _heuristica  _resultadoaux)) ) ;calculo do custo

									(if _accaoOLD 
										(progn (setf _accaoaux (copy-list _accaoOLD)) ;coloca o aux, como sendo a lista de accoes antigas e de seguida adiciona a nova accao
										       (push _accao _accaoaux)
                    ) 
                    (push _accao _accaoaux) ;adiciona a accao ao accaoaux
									)

									(if (gethash _custo hashtb)
										(push (cons _resultadoaux _accaoaux) (gethash _custo hashtb))
										(progn	(setf (gethash _custo hashtb)(list))
												(push _custo _listacustos)
												(push (cons _resultadoaux _accaoaux)
												(gethash _custo hashtb))
										) 
									)
									(setf _accaoaux (list))
								)
							)
						)
						  
						(setf _listacustos (sort _listacustos #'<)) ;ORDENA A LISTA
						(setf _par_resultado (pop (gethash (car _listacustos) hashtb))) ;SACA O PRIMEIRO PAR ORDENADO
						(setf _estado (car _par_resultado)) ;ATRIBUI O ESTADO
						(setf _accaoOLD (cdr _par_resultado)) ;GUARDA O CAMINHO

						(if (gethash (car _listacustos) hashtb) 
							()
							(progn	(remhash (car _listacustos) hashtb)  
									(pop _listacustos) ;remove da lista de custos, se a chave de custo, esta vazia
							)
						)  

						(if (eql (hash-table-count hashtb) 0)  ;verifica se a hashtable esta vazia
							(progn	(format t "~c[41mSEM_SOLUCAO:~c[0m" #\ESC #\ESC) (setf _semsolucao nil)
									(setf caminho nil)
							) 
							()
						)

				)
			)
		)
(format t "~c[41mPONTOS:~c[0m" #\ESC #\ESC)
  (print (estado-pontos _estado))
	(reverse caminho)
	)
)
;;;;;;;;;;;;;;;;;;;;





(defun procura-best (_array _lista-pecas)
   ( let ((_problema nil))
        (setf _problema
            (make-problema
               :estado-inicial
                    (make-estado
                        :pontos 0
                        :tabuleiro (array->tabuleiro _array)
                        :pecas-colocadas ()
                        :pecas-por-colocar _lista-pecas
                    )
                :solucao #'solucao
                :accoes #'accoes
                :resultado #'resultado
                :custo-caminho #'custo-oportunidade
            )
        )
    (procura-A* _problema #'H)
    )
)  
;;;;;;;;;;;;;;;;;;;





(defun zero () 0) 

;Funcao heuristica geral
(defun H (_estado)
	(let(			;constantes que correspondem aos pesos a atribuir a cada heuristica
		(c0 20)
		(c1 1)
		(c2 1)
		(c3 1)
		(c4 1)
		(c5 1)
	;	(c99 0)
		)

		(+ (* c0 (h0 _estado)) (* c1 (h1 _estado)) (* c2 (h2 _estado)) (* c3 (h3 _estado)) (* c4(h4 _estado)) (* c5 (h5 _estado)))
	)
)

(defun h0 (_estado)	;devolve o numero de buracos no tabuleiro
    (let(
        (_tabuleiro (estado-tabuleiro _estado))
        (_colunas (array-dimension (estado-tabuleiro _estado) 1))
        (_nrBuracos 0)
        (_alturaColuna 0)
        )
        
		(dotimes (coluna _colunas)
            (setf _alturaColuna (tabuleiro-altura-coluna _tabuleiro coluna))s
            (dotimes (pos _alturaColuna)
                (if  (equal (aref _tabuleiro (- 17 pos) coluna) nil) (incf _nrBuracos) ())
            )
        )
        _nrBuracos
    )

)

(defun h1 (_estado) ;devolve a altura de coluna mais alta do tabuleiro
	(let(
        (_tabuleiro (estado-tabuleiro _estado))
        (_colunas (array-dimension (estado-tabuleiro _estado) 1))
        (_alturaColunaMax -1)
        (_alturaColuna 0)
		)
		
		(dotimes (coluna _colunas)
			(setf _alturaColuna (tabuleiro-altura-coluna _tabuleiro coluna))
			(if (< _alturaColunaMax _alturaColuna) 
				(setf _alturaColunaMax _alturaColuna)
				()
			)
		)
  _alturaColunaMax
	)
)

(defun h2 (_estado)	;devolve o numero de posicoes preenchidas no tabuleiro
    (- (h99 _estado) (h0 _estado))
)

(defun h3 (_estado)	;devolve a maior diferenca entre alturas de colunas adjacentes
	(let(
		(_tabuleiro (estado-tabuleiro _estado))
        (_colunas (- (array-dimension (estado-tabuleiro _estado) 1) 1))
        (_maxDiferenca 0)
        (_Diferenca 0)
		)
		
		(dotimes (coluna _colunas)
			(setf _Diferenca (calcula_diferenca_altura_aux _tabuleiro coluna))
			(if (< _maxDiferenca _Diferenca)
				(setf _maxDiferenca _Diferenca)
				()
			)
		)
	_maxDiferenca
	)
)

(defun calcula_diferenca_altura_aux (_tabuleiro _coluna)
    (abs (- (tabuleiro-altura-coluna _tabuleiro _coluna) (tabuleiro-altura-coluna _tabuleiro (+ _coluna 1))))
)

(defun h4 (_estado)	;devolve a soma das diferencas de alturas entre cada coluna e as suas adjacentes
	(let(
        (_tabuleiro (estado-tabuleiro _estado))
        (_colunas (- (array-dimension (estado-tabuleiro _estado) 1) 1))
        (_sumDiferenca 0)
		  )
      
  		(dotimes (coluna _colunas)
  			(setf _sumDiferenca (+ _sumDiferenca (calcula_diferenca_altura_aux _tabuleiro coluna)))
  		)
  _sumDiferenca
	)
)

(defun h5 (_estado) ;devolve o valor correspondente a soma dos valores de todas as posicoes preenchidas,
					;sendo que o valor de cada uma destas corresponde a linha em que se encontra
	(let(
        (_tabuleiro (estado-tabuleiro _estado))
        (_colunas (array-dimension (estado-tabuleiro _estado) 1))
        (_linhas (array-dimension (estado-tabuleiro _estado) 0))
        (soma 0)
        (total 0)
      )
		
  		(dotimes (linha _linhas)
          (dotimes (coluna _colunas)
              (if (aref _tabuleiro (- 17 linha) coluna) 
                  (incf soma)
                  ()
              )
          )
          (setf total (+ total (* (+ linha 1) soma)))
          (setf soma 0)
      )

	total
	)
)

(defun h99 (_estado) ;devolve a soma das alturas de todas as colunas
  (let	(
		      (_tabuleiro (estado-tabuleiro _estado))
		      (_colunas (array-dimension (estado-tabuleiro _estado) 1))
          (_sum 0)
        )
		
        (dotimes (coluna _colunas)
	          (setf _sum (+ _sum (tabuleiro-altura-coluna _tabuleiro coluna)))
        )

  _sum  
  )
)
;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;
;; FUNCOES EXTRA ;;
;;;;;;;;;;;;;;;;;;;

;DEVOLVE UM ELEMENTO RANDOM DA LISTA
(defun random-element-meu (list)
	(nth (random (length list)) list)
)
	
;;PREENCHE O TBAULEIRO COM LIXO
(defun bota-lixo (taboriginal)
        (dotimes (linha 18)
            (dotimes (coluna 10)
                (setf (aref taboriginal linha coluna) (random-element-meu '(NIL T)))
            )
        )
)

;;PREENCHE O TABULEIRO COM NUMEROS CORRESPONDENTES A LINHAS
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

(load "teste_1.lisp")
(format t "~c[44m------------------------~c[0m~%~%" #\ESC #\ESC)
(load "teste_2.lisp")
(format t "~c[44m------------------------~c[0m~%~%" #\ESC #\ESC)
(load "teste_3.lisp")
(format t "~c[44m------------------------~c[0m~%~%" #\ESC #\ESC)
(load "teste_3a.lisp")
(format t "~c[44m------------------------~c[0m~%~%" #\ESC #\ESC)
(load "teste_3b.lisp")
(format t "~c[44m------------------------~c[0m~%~%" #\ESC #\ESC)
(load "teste_4.lisp")
