(defparameter *conexoes*
  '((alice . bob) (alice . carol) (alice . diana)
    (bob   . carol) (bob   . eve)
    (carol . diana) (carol . frank)
    (diana . eve)   (diana . george)
    (eve   . frank) (eve   . henry)
    (frank . george)
    (george . henry)
    (henry  . ivan)
    (ivan   . alice) (ivan . julia)
    (julia  . alice) (julia . bob)))

(defun nomes-unicos (conexoes)
  (reduce (lambda (acc par)
            (let ((acc1 (adjoin (car par) acc :test #'eq)))
              (adjoin (cdr par) acc1 :test #'eq)))
          conexoes
          :initial-value '()))

(defun seguindo-de (nome conexoes)
  (remove-duplicates
   (mapcar #'cdr
           (remove-if-not (lambda (par) (eq (car par) nome)) conexoes))))

(defun construir-grafo (conexoes)
  (mapcar (lambda (nome) (cons nome (seguindo-de nome conexoes)))
          (nomes-unicos conexoes)))

(defun seguindo (grafo nome)
  (cdr (assoc nome grafo)))

(defun seguidores-de (grafo nome)
  (mapcar #'car
          (remove-if-not (lambda (entrada) (member nome (cdr entrada)))
                          grafo)))

(defun amigos-diretos (grafo nome)
  (sort (copy-list (seguindo grafo nome)) #'string< :key #'symbol-name))

(defun grau-de-separacao (grafo origem destino)
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
  (sort (copy-list (intersection (seguindo grafo a) (seguindo grafo b)))
        #'string< :key #'symbol-name))

(defun contar-ocorrencias (lista)
  (reduce (lambda (acc elem)
            (let ((par (assoc elem acc)))
              (if par
                  (cons (cons elem (1+ (cdr par))) (remove par acc :test #'equal))
                  (cons (cons elem 1) acc))))
          lista
          :initial-value '()))

(defun sugerir-conexoes (grafo nome &optional (top-n 5))
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
