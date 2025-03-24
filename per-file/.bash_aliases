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
alias aider-local-coder="env LOCAL_AIDER_SKIP_HEALTH_CHECK=1 local-model-enablement-wrapper contaider --model litellm_proxy/local-qwen25-coder-32b"
alias aider-local-qwq32="env LOCAL_AIDER_SKIP_HEALTH_CHECK=1 local-model-enablement-wrapper contaider --architect --model litellm_proxy/local-qwq-32b --editor-model litellm_proxy/local-qwen25-coder-32b"
alias aider-local-fuse01="env LOCAL_AIDER_SKIP_HEALTH_CHECK=1 local-model-enablement-wrapper contaider --model litellm_local-fuseo1"
alias aider-local-tabby="env LOCAL_AIDER_SKIP_HEALTH_CHECK=1 local-model-enablement-wrapper contaider --architect --model litellm_proxy/local-tabby-qwq-32b-architect --editor-model litellm_proxy/local-tabby-qwq-32b-editor"
alias aider-local-exllamav2="env LOCAL_AIDER_SKIP_HEALTH_CHECK=1 local-model-enablement-wrapper contaider --architect --model litellm_proxy/local-exllamav2-qwq-32b --editor-model litellm_proxy/local-exllamav2-qwen25-coder-32b"
alias ccat="batcat -pp"
alias eem="emacs -l ~/.emacs-evil"
alias emacs-fg-daemon-tmux='( set -x; ulimit -v 16000000; source ~/venv/bin/activate; emacs --fg-daemon=$(tmux display-message -p "#S") )'
alias ec="emacsclient"
alias ect='emacsclient -a "" -t'
alias ect-tmux='emacsclient -t --socket-name=$(tmux display-message -p "#S")'
alias ecc='emacsclient -a "" -c'
alias enw="emacs -nw"
alias enwq="emacs -nw -q"
alias fd="fdfind"
alias uuid='python -c "import uuid; import base64; print(\"_\" + base64.b32encode(uuid.uuid4().bytes).decode().strip(\"=\"))"'
alias tmx='export MPLBACKEND=Agg; tmux'
alias dmesg-less='sudo dmesg --color=always | less -R'
