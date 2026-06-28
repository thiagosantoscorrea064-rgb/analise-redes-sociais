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
