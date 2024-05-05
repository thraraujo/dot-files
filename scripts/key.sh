#!/usr/bin/env bash 

# Here is the folder where my keybindings are located. 
F=$HOME/Documents/projects/programs/scripts/.keybindings

# Here we define the configurations for the window 
WINDOW='yad --text-info --back=#2e3440 --fore=#8fbcbb'
OPTIONSA='--no-buttons --geometry=1100x800'
OPTIONSB='--no-buttons --geometry=500x400'

echo "What do you wanna see?"

echo "1. Neovim keybindings"
echo "2. Tmux keybindings"
echo "3. Terminal"
echo "4. Vim - Basics"
echo "5. Vim - Advanced"

read -p "> " choice # prompt string

if [[ $choice -eq 1 ]]; then 
  column $F/nvim-kb.md -t -s ":" | $WINDOW $OPTIONSA --fontname="hack 14"
elif [[ $choice -eq 2 ]]; then
  column $F/tmux-kb.md -t -s ":" | $WINDOW $OPTIONSB --fontname="hack 14"
elif [[ $choice -eq 3 ]]; then
  column $F/terminal-kb.md -t -s ":" | $WINDOW $OPTIONSA --fontname="hack 14"
elif [[ $choice -eq 4 ]]; then
  cat $F/vim-kb-I.md | $WINDOW $OPTIONSA --fontname="hack 14"
elif [[ $choice -eq 5 ]]; then
  cat $F/vim-kb-II.md | $WINDOW $OPTIONSA --fontname="hack 14"
else 
  echo "See you soon!"
fi 
