#!/usr/bin/env bash

FILENAME=$1

echo "Concursos:"
echo " "
echo "1. UFTM | 2. UFF-VR | 3. UFF-NITEROI | 4. UNIFEI"
echo " "
read -p "Qual desses concursos? " CONCURSO
echo " "
awk -f topics.awk $FILENAME $CONCURSO
echo " "


