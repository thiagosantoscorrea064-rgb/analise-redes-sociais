"""
LABORATÓRIO DE PROGRAMAÇÃO - 2026/1
Opção 3: Análise de Redes Sociais
Paradigma: Orientação a Objetos (Python)

Decisão de projeto: usamos classes para modelar entidades do domínio (Usuario, RedeSocial).
Cada entidade encapsula seus dados e comportamentos, permitindo reutilização e extensão.
O grafo de seguidores é representado internamente como dicionário de adjacência,
acesso O(1) por nome, o que é mais eficiente do que listas de arestas.
"""

from collections import deque


class Usuario:
    """
    Representa um usuário da rede social.

    Decisão: manter seguidores e seguindo separados permite distinguir
    relações direcionadas (A segue B ≠ B segue A), como no Twitter/Instagram.
    """

    def __init__(self, nome: str, bio: str = ""):
        self.nome = nome
        self.bio = bio
        # Conjuntos para busca O(1) e sem duplicatas
        self._seguindo: set[str] = set()   # quem este usuário segue
        self._seguidores: set[str] = set()  # quem segue este usuário

    def __repr__(self):
        return f"Usuario('{self.nome}')"

    @property
    def seguindo(self) -> frozenset[str]:
        return frozenset(self._seguindo)

    @property
    def seguidores(self) -> frozenset[str]:
        return frozenset(self._seguidores)

    def adicionar_seguindo(self, nome: str):
        """Registra que este usuário passou a seguir 'nome'."""
        self._seguindo.add(nome)

    def adicionar_seguidor(self, nome: str):
        """Registra que 'nome' passou a seguir este usuário."""
        self._seguidores.add(nome)

    def amigos_em_comum_com(self, outro: "Usuario") -> set[str]:
        """
        Retorna conjunto de usuários que ambos seguem.
        Decisão: usar interseção de conjuntos é O(min(|A|,|B|)) — trivial em Python.
        O equivalente em Lisp exige duas chamadas de remove-if-not (filtro);
        em Prolog é uma conjunção de duas chamadas a findall seguida de interseção.
        """
        return self._seguindo & outro._seguindo


class RedeSocial:
    """
    Grafo direcionado de usuários e conexões.

    Decisão: classe agregadora que coordena as operações sobre o conjunto de usuários.
    Encapsular aqui (e não em funções soltas) facilita injetar dados de teste,
    trocar o back-end de armazenamento e adicionar funcionalidades sem quebrar a interface
    pública (princípio de encapsulamento da OO).
    """

    def __init__(self, nome_plataforma: str = "SocialNet"):
        self.nome_plataforma = nome_plataforma
        self._usuarios: dict[str, Usuario] = {}

    # ------------------------------------------------------------------ #
    # Mutação: adicionar usuários e conexões
    # ------------------------------------------------------------------ #

    def adicionar_usuario(self, nome: str, bio: str = "") -> Usuario:
        """Cria e registra um novo usuário, ou retorna o existente."""
        if nome not in self._usuarios:
            self._usuarios[nome] = Usuario(nome, bio)
        return self._usuarios[nome]

    def adicionar_conexao(self, seguidor: str, seguido: str):
        """
        Cria a aresta dirigida seguidor -> seguido.
        Ambos os usuários são criados automaticamente se ainda não existirem.
        Decisão: manter a bidirecionalidade nos objetos evita varreduras completas
        do grafo quando precisamos dos seguidores de alguém — trocamos um pouco de
        memória (cada conexão é guardada duas vezes) por consultas O(1).
        """
        u_seguidor = self.adicionar_usuario(seguidor)
        u_seguido = self.adicionar_usuario(seguido)
        u_seguidor.adicionar_seguindo(seguido)
        u_seguido.adicionar_seguidor(seguidor)

    # ------------------------------------------------------------------ #
    # Consultas
    # ------------------------------------------------------------------ #

    def amigos_diretos(self, nome: str) -> set[str]:
        """Usuários que 'nome' segue."""
        if nome not in self._usuarios:
            return set()
        return set(self._usuarios[nome].seguindo)

    def grau_de_separacao(self, origem: str, destino: str) -> int | None:
        """
        BFS para calcular o grau de separação entre dois usuários.

        Decisão: BFS garante o menor caminho em grafos não-ponderados.
        Em OO, o estado da busca (fila, visitados) fica natural como variáveis locais
        mutáveis. Em Prolog a busca padrão é em profundidade (SLD), exigindo controle
        explícito de profundidade para simular BFS. Em Lisp a mesma fila precisa ser
        passada explicitamente entre chamadas recursivas, já que não há mutação de
        variáveis externas no estilo funcional puro.
        """
        if origem not in self._usuarios or destino not in self._usuarios:
            return None
        if origem == destino:
            return 0

        visitados = {origem}
        fila = deque([(origem, 0)])  # (usuário, distância)

        while fila:
            atual, dist = fila.popleft()
            for vizinho in self._usuarios[atual].seguindo:
                if vizinho == destino:
                    return dist + 1
                if vizinho not in visitados:
                    visitados.add(vizinho)
                    fila.append((vizinho, dist + 1))
        return None  # Não há caminho

    def amigos_em_comum(self, nome_a: str, nome_b: str) -> set[str]:
        """Retorna quem tanto A quanto B seguem."""
        if nome_a not in self._usuarios or nome_b not in self._usuarios:
            return set()
        return self._usuarios[nome_a].amigos_em_comum_com(self._usuarios[nome_b])

    def sugerir_conexoes(self, nome: str, top_n: int = 5) -> list[tuple[str, int]]:
        """
        Sugere novos contatos baseado em 'amigos de amigos'.

        Heurística: conta quantos amigos em comum um candidato tem com 'nome'.
        Quanto mais amigos em comum, mais relevante a sugestão.

        Decisão: usar um dicionário de contagem com um loop explícito é mais legível
        em OO do que uma compreensão aninhada. A ordenação por contagem descendente
        fica em uma linha com sorted() e uma chave composta.
        Em Lisp, o mesmo cálculo é feito com reduce sobre uma lista de pares
        (associação) e sort com uma função de comparação; em Prolog,
        aggregate_all(count, ...) agrupado por candidato faz o papel do dicionário.
        """
        if nome not in self._usuarios:
            return []

        ja_segue = self._usuarios[nome].seguindo | {nome}  # excluir si mesmo
        candidatos: dict[str, int] = {}

        for amigo in self._usuarios[nome].seguindo:
            if amigo not in self._usuarios:
                continue
            for amigo_do_amigo in self._usuarios[amigo].seguindo:
                if amigo_do_amigo not in ja_segue:
                    candidatos[amigo_do_amigo] = candidatos.get(amigo_do_amigo, 0) + 1

        # Ordenar por contagem desc, depois por nome asc (para resultado determinístico)
        return sorted(candidatos.items(), key=lambda x: (-x[1], x[0]))[:top_n]

    def usuarios_influentes(self, top_n: int = 5) -> list[tuple[str, int]]:
        """
        Retorna os usuários com mais seguidores (métrica de influência simples).
        Decisão: encapsular aqui permite trocar a métrica futuramente (ex: PageRank)
        sem alterar o código cliente — outro benefício direto do encapsulamento OO.
        """
        ranking = [(nome, len(u.seguidores)) for nome, u in self._usuarios.items()]
        return sorted(ranking, key=lambda x: (-x[1], x[0]))[:top_n]

    def listar_usuarios(self) -> list[str]:
        return sorted(self._usuarios.keys())


# ============================================================
# DADOS DE TESTE (os mesmos em Prolog e Lisp, para comparação justa)
# ============================================================

def criar_rede_exemplo() -> RedeSocial:
    """
    Rede fictícia com 10 usuários para demonstração.
    Os mesmos relacionamentos são representados nos outros dois paradigmas.
    """
    rede = RedeSocial("SocialNet")

    conexoes = [
        ("alice", "bob"),
        ("alice", "carol"),
        ("alice", "diana"),
        ("bob", "carol"),
        ("bob", "eve"),
        ("carol", "diana"),
        ("carol", "frank"),
        ("diana", "eve"),
        ("diana", "george"),
        ("eve", "frank"),
        ("eve", "henry"),
        ("frank", "george"),
        ("george", "henry"),
        ("henry", "ivan"),
        ("ivan", "alice"),
        ("ivan", "julia"),
        ("julia", "alice"),
        ("julia", "bob"),
    ]

    for seguidor, seguido in conexoes:
        rede.adicionar_conexao(seguidor, seguido)

    return rede


# ============================================================
# EXECUÇÃO DEMONSTRATIVA
# ============================================================

if __name__ == "__main__":
    rede = criar_rede_exemplo()

    print("=" * 55)
    print("   ANÁLISE DE REDES SOCIAIS — Python (OO)")
    print("=" * 55)

    print(f"\nUsuários na rede: {', '.join(rede.listar_usuarios())}")

    usuario = "alice"
    print(f"\nAmigos diretos de {usuario}: {sorted(rede.amigos_diretos(usuario))}")

    pares = [("alice", "henry"), ("alice", "ivan"), ("alice", "julia"), ("julia", "george")]
    print("\nGraus de separação:")
    for a, b in pares:
        grau = rede.grau_de_separacao(a, b)
        print(f"   {a} -> {b}: {grau} grau(s)")

    print("\nAmigos em comum:")
    print(f"   alice & bob:   {sorted(rede.amigos_em_comum('alice', 'bob'))}")
    print(f"   alice & carol: {sorted(rede.amigos_em_comum('alice', 'carol'))}")
    print(f"   bob & diana:   {sorted(rede.amigos_em_comum('bob', 'diana'))}")

    print("\nSugestões de conexão para alice:")
    for candidato, score in rede.sugerir_conexoes("alice"):
        print(f"   {candidato} ({score} amigo(s) em comum)")

    print("\nTop usuários por número de seguidores:")
    for nome, seguidores in rede.usuarios_influentes():
        print(f"   {nome}: {seguidores} seguidores")
