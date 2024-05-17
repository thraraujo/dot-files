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


echo "Here are the options you have:"
echo "    1. See status."

read -p ">> What option do you want? " CHOICE

echo " "

if [ $CHOICE -eq 1 ] ; then

#echo "Option 1: Go to repo:"
#for ((i=0; i<${#gitRepos[@]}; i++)); do
#   echo " $i - ${gitRepos[i]}"
#done
#
#echo " "
#
#read -p " What directory do you want? " DIR
#
#echo "${gitRepos[$DIR]}"
#cd "${gitRepos[$DIR]}"
#
#else

echo "Option 1: See modified repos"
echo "--- --- --- --- --- --- --- --- --- --- --- --- --- --- ---"
for repo in "${gitRepos[@]}"
do
    test -n "$(git -C $repo status --porcelain)" && echo "   >> MODIFIED :: $repo" #|| echo "<< NOTHING :: $repo"
done
echo "--- --- --- --- --- --- --- --- --- --- --- --- --- --- ---"
fi
