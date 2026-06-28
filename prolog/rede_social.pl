% ============================================================
% LABORATORIO DE PROGRAMACAO - 2026/1
% Opcao 3: Analise de Redes Sociais
% Paradigma: Logico (Prolog / SWI-Prolog)
% ============================================================
%
% Decisao de projeto geral: em vez de modelar "objetos" com estado proprio
% (como em Python), aqui o conhecimento e representado diretamente como
% FATOS (segue/2) e o comportamento emerge de REGRAS que fazem unificacao
% e busca (backtracking) sobre esses fatos. Nao existe "classe RedeSocial":
% a base de fatos inteira E a rede social.
%
% Isso e a maior diferenca de paradigma: em OO encapsulamos dados+metodos
% dentro de um objeto; em logico, dados (fatos) e comportamento (regras)
% sao coisas completamente separadas, e qualquer regra pode consultar
% qualquer fato livremente. Ganha-se declaratividade (a regra parece a
% propria definicao matematica do problema), perde-se encapsulamento.

:- use_module(library(lists)).
:- use_module(library(aggregate)).

% ------------------------------------------------------------------
% BASE DE FATOS (dados de teste — os mesmos do Python e do Lisp)
% ------------------------------------------------------------------

usuario(alice).
usuario(bob).
usuario(carol).
usuario(diana).
usuario(eve).
usuario(frank).
usuario(george).
usuario(henry).
usuario(ivan).
usuario(julia).

% segue(Seguidor, Seguido): relacao direcionada, igual a adicionar_conexao
% do Python. Nao guardamos "seguidores" como fato separado (ao contrario do
% Python, que mantinha dois sets por usuario) porque em Prolog a relacao
% inversa e so uma consulta com os argumentos trocados — nao precisamos
% duplicar a informacao para ter acesso eficiente nos dois sentidos.
segue(alice, bob).
segue(alice, carol).
segue(alice, diana).
segue(bob,   carol).
segue(bob,   eve).
segue(carol, diana).
segue(carol, frank).
segue(diana, eve).
segue(diana, george).
segue(eve,   frank).
segue(eve,   henry).
segue(frank, george).
segue(george,henry).
segue(henry, ivan).
segue(ivan,  alice).
segue(ivan,  julia).
segue(julia, alice).
segue(julia, bob).

% ------------------------------------------------------------------
% CONSULTAS
% ------------------------------------------------------------------

% amigos_diretos(+Nome, -Lista)
% Decisao: findall/3 coleta todas as solucoes da unificacao segue(Nome, X)
% por backtracking automatico do motor — nao escrevemos nenhum loop.
% E o equivalente logico do "for vizinho in usuario.seguindo" do Python.
amigos_diretos(Nome, ListaOrdenada) :-
    findall(X, segue(Nome, X), Lista),
    sort(Lista, ListaOrdenada).

% caminho_existe(+Origem, +Destino, +Tamanho)
% Decisao: a SLD-resolution do Prolog busca em PROFUNDIDADE por padrao, nao
% em largura. Para obter o MENOR caminho (equivalente ao BFS do Python),
% fixamos o tamanho do caminho e usamos between/3, de fora para dentro,
% testando tamanho 1, depois 2, depois 3... Isso simula um BFS por
% "iterative deepening", mas e bem menos natural do que a fila explicita
% do Python ou o acumulador de fronteira do Lisp.
caminho_existe(X, Y, 1) :-
    segue(X, Y).
caminho_existe(X, Y, N) :-
    N > 1,
    segue(X, Z),
    N1 is N - 1,
    caminho_existe(Z, Y, N1).

% grau_de_separacao(+Origem, +Destino, -Grau)
% Decisao: o corte (!) depois de achar o primeiro Grau que funciona e
% essencial — sem ele, o backtracking continuaria tentando tamanhos maiores
% e produziria caminhos mais longos como solucoes alternativas, o que nao
% faz sentido para "grau de separacao" (queremos o MENOR).
% MaxGrau limita a busca para nao entrar em loop infinito em grafos com
% ciclos (a rede de teste tem ciclos, ex: alice->ivan->julia->alice).
grau_de_separacao(X, X, 0) :- !.
grau_de_separacao(X, Y, Grau) :-
    X \== Y,
    between(1, 10, Grau),
    caminho_existe(X, Y, Grau),
    !.

% amigos_em_comum(+A, +B, -Comuns)
% Decisao: a conjuncao "segue(A,X), segue(B,X)" dentro do findall E a
% interseccao — nao existe operador de interseccao de conjuntos embutido
% sendo chamado explicitamente, a unificacao conjunta faz o papel do "&"
% do Python (sets) ou do intersection do Lisp.
amigos_em_comum(A, B, ComunsOrdenados) :-
    findall(X, (segue(A, X), segue(B, X)), Comuns),
    sort(Comuns, ComunsOrdenados).

% candidato_sugestao(+Nome, -Candidato)
% Gera (por backtracking) cada "amigo de amigo" de Nome que Nome ainda nao
% segue e que nao e o proprio Nome. Cada solucao desse predicado e uma
% ocorrencia, nao um candidato unico — a contagem de ocorrencias e que
% determina o numero de amigos em comum.
candidato_sugestao(Nome, Candidato) :-
    segue(Nome, Amigo),
    segue(Amigo, Candidato),
    Candidato \== Nome,
    \+ segue(Nome, Candidato).

% sugerir_conexoes(+Nome, -RankingTopN, +TopN)
% Decisao: aggregate_all(count, ...) por candidato faz o papel do
% dicionario candidatos[nome] += 1 do Python. msort com a chave invertida
% (Score negativo) ordena por contagem decrescente; como Pares e uma lista
% de pares Score-Nome, o sort padrao do Prolog (ordem padrao de termos) ja
% desempata por nome em ordem alfabetica quando os scores sao iguais.
sugerir_conexoes(Nome, TopN, RankingTopN) :-
    findall(Candidato, candidato_sugestao(Nome, Candidato), Brutos),
    list_to_set(Brutos, Unicos),
    findall(ScoreNeg-Candidato,
            ( member(Candidato, Unicos),
              aggregate_all(count, candidato_sugestao(Nome, Candidato), Score),
              ScoreNeg is -Score
            ),
            Pares),
    msort(Pares, Ordenado),
    primeiros_n(Ordenado, TopN, OrdenadoTopN),
    maplist(inverter_score, OrdenadoTopN, RankingTopN).

inverter_score(ScoreNeg-Nome, Nome-Score) :- Score is -ScoreNeg.

primeiros_n(Lista, N, Primeiros) :-
    length(Prefixo, N),
    ( append(Prefixo, _, Lista) -> Primeiros = Prefixo ; Primeiros = Lista ).

% usuarios_influentes(-RankingTopN, +TopN)
% Decisao: "seguidores de Nome" nunca foi armazenado como fato — e
% calculado aqui como aggregate_all(count, segue(_, Nome), C), ou seja,
% "para quantos X existe segue(X, Nome)?". Isso mostra uma vantagem do
% paradigma logico: nao precisamos manter uma estrutura espelhada (como o
% set _seguidores do Python) so para responder essa pergunta — a relacao
% inversa sempre esteve disponivel nos mesmos fatos.
usuarios_influentes(TopN, RankingTopN) :-
    findall(ScoreNeg-Nome,
            ( usuario(Nome),
              aggregate_all(count, segue(_, Nome), C),
              ScoreNeg is -C
            ),
            Pares),
    msort(Pares, Ordenado),
    primeiros_n(Ordenado, TopN, OrdenadoTopN),
    maplist(inverter_score, OrdenadoTopN, RankingTopN).

listar_usuarios(ListaOrdenada) :-
    findall(Nome, usuario(Nome), Lista),
    sort(Lista, ListaOrdenada).

% ------------------------------------------------------------------
% EXECUCAO DEMONSTRATIVA
% ------------------------------------------------------------------
% Decisao: ao contrario do Python (que usa um bloco __main__) e do Lisp
% (que chama uma funcao main), aqui escrevemos um predicado main/0 que e
% chamado explicitamente via diretiva initialization/1, pois Prolog nao
% tem um conceito nativo de "ponto de entrada do script".

main :-
    writeln('======================================================='),
    writeln('   ANALISE DE REDES SOCIAIS -- Prolog (Logico)'),
    writeln('======================================================='),

    listar_usuarios(Usuarios),
    format('~nUsuarios na rede: ~w~n', [Usuarios]),

    amigos_diretos(alice, AmigosAlice),
    format('~nAmigos diretos de alice: ~w~n', [AmigosAlice]),

    writeln('\nGraus de separacao:'),
    forall(member(A-B, [alice-henry, alice-ivan, alice-julia, julia-george]),
           ( grau_de_separacao(A, B, Grau)
           -> format('   ~w -> ~w: ~w grau(s)~n', [A, B, Grau])
           ;  format('   ~w -> ~w: sem caminho~n', [A, B])
           )),

    writeln('\nAmigos em comum:'),
    amigos_em_comum(alice, bob, ComAB), format('   alice & bob:   ~w~n', [ComAB]),
    amigos_em_comum(alice, carol, ComAC), format('   alice & carol: ~w~n', [ComAC]),
    amigos_em_comum(bob, diana, ComBD), format('   bob & diana:   ~w~n', [ComBD]),

    writeln('\nSugestoes de conexao para alice:'),
    sugerir_conexoes(alice, 5, Sugestoes),
    forall(member(Cand-Score, Sugestoes),
           format('   ~w (~w amigo(s) em comum)~n', [Cand, Score])),

    writeln('\nTop usuarios por numero de seguidores:'),
    usuarios_influentes(5, Influentes),
    forall(member(Nome-Score, Influentes),
           format('   ~w: ~w seguidores~n', [Nome, Score])).

:- initialization(main, main).
