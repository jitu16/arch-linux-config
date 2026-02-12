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

echo "alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'" >> ~/.bashrc
alias config='/usr/bin/git --git-dir=/home/jitu/.cfg/ --work-tree=/home/jitu'

# Start SSH Agent if not running
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent -s > "$HOME/.ssh/agent.env"
fi
if [[ ! "$SSH_AUTH_SOCK" ]]; then
    eval "$(<"$HOME/.ssh/agent.env")" > /dev/null
fi
alias config='/usr/bin/git --git-dir=/home/jitu/.cfg/ --work-tree=/home/jitu'
