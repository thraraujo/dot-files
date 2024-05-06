#!/usr/bin/env bash

# These are my git repositories
gitRepos=(
    $HOME/.config/dot-files/
    $HOME/Sync/projects/aulas/
    $HOME/Sync/projects/cv-projects-documents/
    $HOME/Sync/projects/physics-math/
    $HOME/Sync/projects/wiki/
    $HOME/Sync/projects/work/research-reviews/
    $HOME/Sync/projects/work/q-bosons/
    $HOME/Sync/projects/work/gauge-integrability/
    $HOME/Sync/projects/work/spectral-curves/
    $HOME/Sync/projects/work/pySymmPol
)

for repo in "${gitRepos[@]}"
do
    test -n "$(git -C $repo status --porcelain)" && echo ">> MODIFIED :: $repo" #|| echo "<< NOTHING :: $repo"
done
