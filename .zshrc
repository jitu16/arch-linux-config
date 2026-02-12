# 1. Redirection: If not running interactively, don't do anything
[[ $- != *i* ]] && return

# 2. Path Exports (Doom Emacs & Pipx)
export PATH="$HOME/.config/emacs/bin:$HOME/.local/bin:$PATH"

# 3. Core Aliases
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias d="doom"
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

# 4. SSH Agent Logic (Keeps your Org-mode push working)
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent -s > "$HOME/.ssh/agent.env"
fi
if [[ ! "$SSH_AUTH_SOCK" ]]; then
    eval "$(<"$HOME/.ssh/agent.env")" > /dev/null
fi

PROMPT='%F{cyan}%n@%m %F{yellow}%1~ %F{white}%# '

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

autoload -Uz compinit
compinit
