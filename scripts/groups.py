#!/usr/bin/env python3
#
import random, math # Inicialmente importamos os dois modulos

answer = int(input("""Qual para qual curso devemos criar grupos?
    1. Matemática
    2. Química
    3. Agronomia
    4. Zootecnia
    > """ ))

cases = ["matematica", "quimica", "agronomia", "zootecnia"]

estudantes = f"{answer}_estudantes_{cases[answer - 1]}.txt"
topicos = f"{answer}_topicos_{cases[answer - 1]}.txt"


# Agora iremos ler a lista com o nome dos estudantes.
# O número de estudantes é o comprimento da lista definida
# pelo método .readlines()

with open(estudantes) as file:
    estudantes_lista = file.readlines()
    quantidade_estudantes = len(estudantes_lista)

# Vamos agora criar a lista de tópicos a serem apresentados
with open(topicos) as file:
    topicos_lista = file.readlines()
    assuntos = len(topicos_lista)


minimo_estudantes_grupo = math.floor(quantidade_estudantes / assuntos)
excedente_estudantes = quantidade_estudantes % minimo_estudantes_grupo

# Agora criaremos um conjunto de listas (set) vazias igual ao número de assuntos
# nas quais adicionaremos os estudantes.
conjunto_grupos = []
for n in range(assuntos):
    conjunto_grupos.append([])

# Também criaremos uma lista dos estudantes ordenada alfabeticamente -- Use dictionary
ordem_estudantes = list(range(quantidade_estudantes))

# Considero esse shuffle apenas para maximizar a 'entropia'
for n in range(quantidade_estudantes):
    random.shuffle(ordem_estudantes)
    random.shuffle(topicos_lista)

# Finalmente, podemos criar os nossos grupos
while len(ordem_estudantes) > 0:
    for n in range(assuntos):
        random.shuffle(ordem_estudantes)
        if len(ordem_estudantes) > 0:
            conjunto_grupos[n].append(ordem_estudantes.pop())
        else:
            break

# Com os grupos formados, agora podemos exportar os grupos

grupos = f"{answer}_grupos_{cases[answer - 1]}.txt"

with open(grupos, "w") as file:

    if excedente_estudantes == 0:
        file.write(f'''Temos {quantidade_estudantes} estudantes divididos em:
        => {assuntos - excedente_estudantes} grupos com {minimo_estudantes_grupo} estudantes''')
    else:
        file.write(f'''Temos {quantidade_estudantes} estudantes divididos em:
        => {assuntos - excedente_estudantes} grupos com {minimo_estudantes_grupo} estudantes
        => {excedente_estudantes} grupos com {minimo_estudantes_grupo + 1} estudantes\n''')

    for n in range(assuntos):
        #print(f"\n # Grupo {n + 1}:")
        file.write(f"\n # Grupo {n + 1} :: {len(conjunto_grupos[n])} estudantes :: {topicos_lista[n]}")
        conjunto_grupos[n].sort()

        for m in range(len(conjunto_grupos[n])):
            #print(f"{conjunto_grupos[n][m] + 1} :: {estudantes_lista[conjunto_grupos[n][m]][:-1]}")
            # Os condicionais abaixo ajudam a alinhar a tabela
            if conjunto_grupos[n][m] < 9:
                file.write(f"0{conjunto_grupos[n][m] + 1} :: {estudantes_lista[conjunto_grupos[n][m]]}")
            else:
                file.write(f"{conjunto_grupos[n][m] + 1} :: {estudantes_lista[conjunto_grupos[n][m]]}")


print("Grupos criados!")
