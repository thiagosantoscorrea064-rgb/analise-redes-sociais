;;; ============================================================
;;; LABORATORIO DE PROGRAMACAO - 2026/1
;;; Opcao 3: Analise de Redes Sociais
;;; Paradigma: Funcional (Common Lisp)
;;; ============================================================
;;;
;;; Decisao de projeto geral: nao existe "objeto RedeSocial" com estado
;;; mutavel. O grafo e um valor imutavel (uma lista de associacao nome ->
;;; lista-de-quem-segue), construido uma unica vez e depois somente LIDO.
;;; Toda consulta e uma FUNCAO PURA: recebe o grafo e devolve um resultado
;;; novo, sem nunca alterar o grafo recebido (setf so e usado na fase de
;;; construcao do grafo, nunca nas consultas). Isso e o oposto direto do
;;; Python, onde cada Usuario guarda e MUTA dois sets internos.

;; ------------------------------------------------------------------
;; CONSTRUCAO DO GRAFO
;; ------------------------------------------------------------------

(defparameter *conexoes*
  ;; Mesmos dados de teste do Python e do Prolog.
  '((alice . bob) (alice . carol) (alice . diana)
    (bob   . carol) (bob   . eve)
    (carol . diana) (carol . frank)
    (diana . eve)   (diana . george)
    (eve   . frank) (eve   . henry)
    (frank . george)
    (george . henry)
    (henry  . ivan)
    (ivan   . alice) (ivan . julia)
    (julia  . alice) (julia . bob))
  "Lista de pares (SEGUIDOR . SEGUIDO), igual aos dados de teste das
   outras duas versoes.")

(defun nomes-unicos (conexoes)
  "Extrai, sem duplicatas, todos os nomes que aparecem em CONEXOES.
   Decisao: usar REDUCE para acumular um conjunto, em vez de um loop
   imperativo com push/setf — cada passo devolve uma lista NOVA, a lista
   anterior nunca e mutada."
  (reduce (lambda (acc par)
            (let ((acc1 (adjoin (car par) acc :test #'eq)))
              (adjoin (cdr par) acc1 :test #'eq)))
          conexoes
          :initial-value '()))

(defun seguindo-de (nome conexoes)
  "Lista (sem duplicatas) de quem NOME segue, conforme CONEXOES.
   Decisao: MAPCAR + REMOVE-IF-NOT em vez de um loop com acumulador
   mutavel — e a forma idiomatica funcional de filtrar e transformar."
  (remove-duplicates
   (mapcar #'cdr
           (remove-if-not (lambda (par) (eq (car par) nome)) conexoes))))

(defun construir-grafo (conexoes)
  "Constroi o grafo como uma lista de associacao imutavel:
   ((nome1 . (seguindo...)) (nome2 . (seguindo...)) ...).
   Decisao: o grafo so guarda o sentido 'seguindo'. O sentido 'seguidores'
   nunca e armazenado (ao contrario do Python, que mantinha os dois sets) —
   e recalculado sob demanda em SEGUIDORES-DE, trocando memoria por
   simplicidade e evitando ter duas estruturas que precisariam ficar
   sincronizadas."
  (mapcar (lambda (nome) (cons nome (seguindo-de nome conexoes)))
          (nomes-unicos conexoes)))

(defun seguindo (grafo nome)
  "Quem NOME segue, segundo o GRAFO ja construido."
  (cdr (assoc nome grafo)))

(defun seguidores-de (grafo nome)
  "Quem segue NOME — calculado por busca reversa no grafo inteiro.
   Decisao: REMOVE-IF-NOT sobre o grafo todo e O(n), mais caro que o
   acesso O(1) do set _seguidores do Python; e o preco de nao duplicar
   estado em uma estrutura imutavel."
  (mapcar #'car
          (remove-if-not (lambda (entrada) (member nome (cdr entrada)))
                          grafo)))

;; ------------------------------------------------------------------
;; CONSULTAS
;; ------------------------------------------------------------------

(defun amigos-diretos (grafo nome)
  (sort (copy-list (seguindo grafo nome)) #'string< :key #'symbol-name))

(defun grau-de-separacao (grafo origem destino)
  "BFS funcional: a 'fila' e um parametro explicito da funcao recursiva,
   nao uma variavel mutavel externa como no Python (deque) ou implicita
   como a pilha de chamadas do Prolog.

   Decisao: BFS-AUX recebe (FRONTEIRA VISITADOS DISTANCIA) e devolve um
   valor (a distancia) ou NIL — nunca muta nada que recebeu. A cada nivel,
   a nova fronteira e construida com MAPCAN (achatando as listas de
   vizinhos) e os visitados sao acumulados com UNION, sempre gerando
   listas NOVAS em vez de alterar as antigas."
  (cond
    ((eq origem destino) 0)
    (t (labels ((bfs-aux (fronteira visitados distancia)
                  (cond
                    ((null fronteira) nil)
                    ((member destino fronteira) distancia)
                    (t (let* ((vizinhos (remove-duplicates
                                          (mapcan (lambda (n) (copy-list (seguindo grafo n)))
                                                  fronteira)))
                              (novos (remove-if (lambda (v) (member v visitados))
                                                 vizinhos)))
                         (if (null novos)
                             nil
                             (bfs-aux novos
                                      (union novos visitados)
                                      (1+ distancia))))))))
         (bfs-aux (list origem) (list origem) 1)))))

(defun amigos-em-comum (grafo a b)
  "Interseccao das listas de quem A e quem B seguem.
   Decisao: INTERSECTION e a funcao de conjuntos nativa do Lisp — o
   equivalente direto do operador '&' usado na versao Python com sets."
  (sort (copy-list (intersection (seguindo grafo a) (seguindo grafo b)))
        #'string< :key #'symbol-name))

(defun contar-ocorrencias (lista)
  "Recebe uma lista com repeticoes e devolve uma lista de associacao
   (elemento . contagem), sem usar nenhuma tabela hash mutavel.
   Decisao: REDUCE constroi a alist de contagens passo a passo; em cada
   passo, ou incrementamos a contagem existente (criando um par novo) ou
   adicionamos uma entrada nova — a alist anterior nunca e destrutivamente
   alterada, so substituida pela nova."
  (reduce (lambda (acc elem)
            (let ((par (assoc elem acc)))
              (if par
                  (cons (cons elem (1+ (cdr par))) (remove par acc :test #'equal))
                  (cons (cons elem 1) acc))))
          lista
          :initial-value '()))

(defun sugerir-conexoes (grafo nome &optional (top-n 5))
  "Sugere contatos por 'amigos de amigos', igual a heuristica das outras
   versoes: quanto mais amigos em comum, mais relevante a sugestao.
   Decisao: a sequencia MAPCAN -> REMOVE-IF -> CONTAR-OCORRENCIAS ->
   SORT e um pipeline de transformacoes puras passando o dado de uma
   funcao para a outra — sem nenhuma variavel de estado guardando
   'progresso' da busca, ao contrario do dicionario mutavel do Python."
  (let* ((ja-segue (cons nome (seguindo grafo nome)))
         (candidatos-brutos
           (mapcan (lambda (amigo) (copy-list (seguindo grafo amigo)))
                   (seguindo grafo nome)))
         (candidatos-validos
           (remove-if (lambda (c) (member c ja-segue)) candidatos-brutos))
         (contagens (contar-ocorrencias candidatos-validos))
         (ordenado (sort (copy-list contagens)
                          (lambda (p1 p2)
                            (cond ((> (cdr p1) (cdr p2)) t)
                                  ((< (cdr p1) (cdr p2)) nil)
                                  (t (string< (symbol-name (car p1))
                                               (symbol-name (car p2)))))))))
    (subseq ordenado 0 (min top-n (length ordenado)))))

(defun usuarios-influentes (grafo &optional (top-n 5))
  "Ranking de usuarios por numero de seguidores.
   Decisao: como o grafo so guarda 'seguindo', SEGUIDORES-DE precisa
   varrer a lista toda para cada nome — outra consequencia direta de nao
   duplicar estado em estruturas mutaveis espelhadas."
  (let* ((ranking (mapcar (lambda (entrada)
                             (cons (car entrada) (length (seguidores-de grafo (car entrada)))))
                           grafo))
         (ordenado (sort (copy-list ranking)
                          (lambda (p1 p2)
                            (cond ((> (cdr p1) (cdr p2)) t)
                                  ((< (cdr p1) (cdr p2)) nil)
                                  (t (string< (symbol-name (car p1))
                                               (symbol-name (car p2)))))))))
    (subseq ordenado 0 (min top-n (length ordenado)))))

(defun listar-usuarios (grafo)
  (sort (copy-list (mapcar #'car grafo)) #'string< :key #'symbol-name))

;; ------------------------------------------------------------------
;; EXECUCAO DEMONSTRATIVA
;; ------------------------------------------------------------------

(defun main ()
  (let ((grafo (construir-grafo *conexoes*)))
    (format t "=======================================================~%")
    (format t "   ANALISE DE REDES SOCIAIS -- Lisp (Funcional)~%")
    (format t "=======================================================~%")

    (format t "~%Usuarios na rede: ~{~a~^, ~}~%" (listar-usuarios grafo))

    (format t "~%Amigos diretos de alice: ~{~a~^, ~}~%" (amigos-diretos grafo 'alice))

    (format t "~%Graus de separacao:~%")
    (dolist (par '((alice . henry) (alice . ivan) (alice . julia) (julia . george)))
      (format t "   ~a -> ~a: ~a grau(s)~%"
              (car par) (cdr par) (grau-de-separacao grafo (car par) (cdr par))))

    (format t "~%Amigos em comum:~%")
    (format t "   alice & bob:   ~{~a~^, ~}~%" (amigos-em-comum grafo 'alice 'bob))
    (format t "   alice & carol: ~{~a~^, ~}~%" (amigos-em-comum grafo 'alice 'carol))
    (format t "   bob & diana:   ~{~a~^, ~}~%" (amigos-em-comum grafo 'bob 'diana))

    (format t "~%Sugestoes de conexao para alice:~%")
    (dolist (par (sugerir-conexoes grafo 'alice))
      (format t "   ~a (~a amigo(s) em comum)~%" (car par) (cdr par)))

    (format t "~%Top usuarios por numero de seguidores:~%")
    (dolist (par (usuarios-influentes grafo))
      (format t "   ~a: ~a seguidores~%" (car par) (cdr par)))))

(main)
