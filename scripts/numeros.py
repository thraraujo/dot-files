import random

Jogo = [set([]),set([])]

i = 0

# while i in [0,1]:
#     while len(Jogo[i]) < 15:
#         a = random.randint(1,26)
#         if a not in Jogo[0] and a not in Jogo[1]:
#             Jogo[i].add(a)
#     print(f"Jogo {i + 1}: {sorted(Jogo[i])}")
#     i += 1

while i in [0,1]:
    while len(Jogo[i]) < 15:
        a = random.randint(1,25)
        Jogo[i].add(a)
    if len(Jogo[1]) == 15 and len(Jogo[0].intersection(Jogo[1])) > 5:
        i = 1
        Jogo[1] = set([])
    else:
        i += 1
    
print(f"Jogo 1: {sorted(Jogo[0])}")
print(f"Jogo 2: {sorted(Jogo[1])}")
print(set(Jogo[0]).intersection(Jogo[1]))
print(len(set(Jogo[0]).intersection(Jogo[1])))
print(len(Jogo[0]))
print(len(Jogo[1]))
