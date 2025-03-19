# -*- mode: shell-script -*-

# typos
alias emasc="emacs"
alias cd-="cd -"
alias cd..="cd .."

# commands

alias ..="cd .."
alias g='git'
alias ll='ls -laFrth --color=auto'
alias la='ls -A --color=auto'
alias l='ls -CF --color=auto'
alias ccat="batcat -pp"
alias eem="emacs -l ~/.emacs-evil"
alias emxdt='( ulimit -v 16000000; source ~/venv/bin/activate; emacs --fg-daemon=$(tmux display-message -p "#S") )'
alias ec="emacsclient"
alias ect='emacsclient -a "" -t'
alias ectt='emacsclient -t --socket-name=$(tmux display-message -p "#S")'
alias ecc='emacsclient -a "" -c'
alias enw="emacs -nw"
alias enwq="emacs -nw -q"
alias fd="fdfind"
alias uuid='python -c "import uuid; import base64; print(\"_\" + base64.b32encode(uuid.uuid4().bytes).decode().strip(\"=\"))"'
alias tmx='export MPLBACKEND=Agg; tmux'
alias dmesg-less='sudo dmesg --color=always | less -R'
