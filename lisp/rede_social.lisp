:- use_module(library(lists)).
:- use_module(library(aggregate)).

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

amigos_diretos(Nome, ListaOrdenada) :-
    findall(X, segue(Nome, X), Lista),
    sort(Lista, ListaOrdenada).

caminho_existe(X, Y, 1) :-
    segue(X, Y).
caminho_existe(X, Y, N) :-
    N > 1,
    segue(X, Z),
    N1 is N - 1,
    caminho_existe(Z, Y, N1).

grau_de_separacao(X, X, 0) :- !.
grau_de_separacao(X, Y, Grau) :-
    X \== Y,
    between(1, 10, Grau),
    caminho_existe(X, Y, Grau),
    !.

amigos_em_comum(A, B, ComunsOrdenados) :-
    findall(X, (segue(A, X), segue(B, X)), Comuns),
    sort(Comuns, ComunsOrdenados).

candidato_sugestao(Nome, Candidato) :-
    segue(Nome, Amigo),
    segue(Amigo, Candidato),
    Candidato \== Nome,
    \+ segue(Nome, Candidato).

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
