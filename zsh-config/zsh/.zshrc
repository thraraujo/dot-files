# _____    _        ____             __ _
# |__  /___| |__    / ___|___  _ __  / _(_) __ _
#   / // __| '_ \  | |   / _ \| '_ \| |_| |/ _` |
#  / /_\__ \ | | | | |__| (_) | | | |  _| | (_| |
# /____|___/_| |_|  \____\___/|_| |_|_| |_|\__, |
#                                          |___/

# I am using some of Luke Smith configs: 
# https://gist.github.com/LukeSmithxyz/e62f26e55ea8b0ed41a65912fbebbe52

# Enable colors and change prompt:
autoload -U colors && colors
# PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}%b~%b "

# History in cache directory:
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/zsh/history

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

# Fuzzy finder configuration
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Load aliases, shortcuts (and keybindings) and paths if existent.
[ -f "$HOME/.config/shortcutrc" ] && source "$HOME/.config/shortcutrc"
[ -f "$HOME/.config/aliasrc" ] && source "$HOME/.config/aliasrc"
[ -f "$HOME/.config/pathrc" ] && source "$HOME/.config/pathrc"

# Load Neofetch
neofetch

# Load zsh-syntax-highlighting
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# load starship prompt
eval "$(starship init zsh)"
