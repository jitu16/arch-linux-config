#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
PS1='[\u@\h \W]\$ '

export path="$HOME/.config/emacs/bin:$PATH"

# Created by `pipx` on 2026-01-24 10:25:58
export PATH="$PATH:/home/jitu/.local/bin"

export PATH="$HOME/.config/emacs/bin:$PATH"
alias d="doom"
