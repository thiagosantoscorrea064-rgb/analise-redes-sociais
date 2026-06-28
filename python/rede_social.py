```python
from collections import deque


class Usuario:
    def __init__(self, nome: str, bio: str = ""):
        self.nome = nome
        self.bio = bio
        self._seguindo: set[str] = set()
        self._seguidores: set[str] = set()

    def __repr__(self):
        return f"Usuario('{self.nome}')"

    @property
    def seguindo(self) -> frozenset[str]:
        return frozenset(self._seguindo)

    @property
    def seguidores(self) -> frozenset[str]:
        return frozenset(self._seguidores)

    def adicionar_seguindo(self, nome: str):
        self._seguindo.add(nome)

    def adicionar_seguidor(self, nome: str):
        self._seguidores.add(nome)

    def amigos_em_comum_com(self, outro: "Usuario") -> set[str]:
        return self._seguindo & outro._seguindo


class RedeSocial:
    def __init__(self, nome_plataforma: str = "SocialNet"):
        self.nome_plataforma = nome_plataforma
        self._usuarios: dict[str, Usuario] = {}

    def adicionar_usuario(self, nome: str, bio: str = "") -> Usuario:
        if nome not in self._usuarios:
            self._usuarios[nome] = Usuario(nome, bio)
        return self._usuarios[nome]

    def adicionar_conexao(self, seguidor: str, seguido: str):
        u_seguidor = self.adicionar_usuario(seguidor)
        u_seguido = self.adicionar_usuario(seguido)
        u_seguidor.adicionar_seguindo(seguido)
        u_seguido.adicionar_seguidor(seguidor)

    def amigos_diretos(self, nome: str) -> set[str]:
        if nome not in self._usuarios:
            return set()
        return set(self._usuarios[nome].seguindo)

    def grau_de_separacao(self, origem: str, destino: str) -> int | None:
        if origem not in self._usuarios or destino not in self._usuarios:
            return None
        if origem == destino:
            return 0

        visitados = {origem}
        fila = deque([(origem, 0)])

        while fila:
            atual, dist = fila.popleft()
            for vizinho in self._usuarios[atual].seguindo:
                if vizinho == destino:
                    return dist + 1
                if vizinho not in visitados:
                    visitados.add(vizinho)
                    fila.append((vizinho, dist + 1))
        return None

    def amigos_em_comum(self, nome_a: str, nome_b: str) -> set[str]:
        if nome_a not in self._usuarios or nome_b not in self._usuarios:
            return set()
        return self._usuarios[nome_a].amigos_em_comum_com(self._usuarios[nome_b])

    def sugerir_conexoes(self, nome: str, top_n: int = 5) -> list[tuple[str, int]]:
        if nome not in self._usuarios:
            return []

        ja_segue = self._usuarios[nome].seguindo | {nome}
        candidatos: dict[str, int] = {}

        for amigo in self._usuarios[nome].seguindo:
            if amigo not in self._usuarios:
                continue
            for amigo_do_amigo in self._usuarios[amigo].seguindo:
                if amigo_do_amigo not in ja_segue:
                    candidatos[amigo_do_amigo] = candidatos.get(amigo_do_amigo, 0) + 1

        return sorted(candidatos.items(), key=lambda x: (-x[1], x[0]))[:top_n]

    def usuarios_influentes(self, top_n: int = 5) -> list[tuple[str, int]]:
        ranking = [(nome, len(u.seguidores)) for nome, u in self._usuarios.items()]
        return sorted(ranking, key=lambda x: (-x[1], x[0]))[:top_n]

    def listar_usuarios(self) -> list[str]:
        return sorted(self._usuarios.keys())


def criar_rede_exemplo() -> RedeSocial:
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
```
