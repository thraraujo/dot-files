# _____    _        ____             __ _
# |__  /___| |__    / ___|___  _ __  / _(_) __ _
#   / // __| '_ \  | |   / _ \| '_ \| |_| |/ _` |
#  / /_\__ \ | | | | |__| (_) | | | |  _| | (_| |
# /____|___/_| |_|  \____\___/|_| |_|_| |_|\__, |
#                                          |___/

if [ $HOST = 'thiago-thinkpad' ]; then 
# the following commands make the capslock an additional escape and change the 
# repetition rate
  setxkbmap -option caps:escape 
  xset r rate 200 25
fi

## case insensitive path-completion
autoload -Uz +X compinit && compinit
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' menu select

# Vim Mode

# I add the following part to use some vim commands. See this in command line editing in the Linux Bible, page 73. 
# We can do it with 'set -o vi' or with bindkey -v
set -o vi
bindkey -v
export KEYTIMEOUT=1

# Use vim keys in tab complete menu: 
# There seems to be a problem with the menuselect thing. 
zmodload -i zsh/complist

bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

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
# [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh


# Load aliases, shortcuts (and keybindings) and paths if existent.
# [ -f "$HOME/.config/dot-files/zsh-config/shortcutrc" ] && source "$HOME/.config/dot-files/zsh-config/shortcutrc"
[ -f "$HOME/.config/dot-files/zsh-config/aliasrc" ] && source "$HOME/.config/dot-files/zsh-config/aliasrc"
[ -f "$HOME/.config/dot-files/zsh-config/pathrc" ] && source "$HOME/.config/dot-files/zsh-config/pathrc"


# Load Neofetch
neofetch

# load starship prompt
eval "$(starship init zsh)"

# Load zsh-syntax-highlighting
source $HOME/.config/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
