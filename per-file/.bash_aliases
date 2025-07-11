# -*- mode: shell-script -*-

# typos
alias emasc="emacs"
alias cd-="cd -"
alias cd..="cd .."

# commands

alias ..="cd .."
alias g='GPG_TTY=$(tty) git'
alias ll='ls -laFrth --color=auto'
alias la='ls -A --color=auto'
alias l='ls -CF --color=auto'
alias aider-local="env OPENAI_API_KEY=sk-empty OPENAI_API_BASE=http://host.docker.internal:8686/v1 contaider --architect --model openai/llamacpp-QwQ-32B --editor-model openai/llamacpp-Qwen2.5-Coder-32B-Instruct"
# alias aider-local-coder="env LOCAL_AIDER_SKIP_HEALTH_CHECK=1 local-model-enablement-wrapper contaider --model litellm_proxy/local-qwen25-coder-32b"
# alias aider-local-qwq32="env LOCAL_AIDER_SKIP_HEALTH_CHECK=1 local-model-enablement-wrapper contaider --architect --model litellm_proxy/local-qwq-32b --editor-model litellm_proxy/local-qwen25-coder-32b"
# alias aider-local-fuse01="env LOCAL_AIDER_SKIP_HEALTH_CHECK=1 local-model-enablement-wrapper contaider --model litellm_local-fuseo1"
# alias aider-local-tabby="env LOCAL_AIDER_SKIP_HEALTH_CHECK=1 local-model-enablement-wrapper contaider --architect --model litellm_proxy/local-tabby-qwq-32b-architect --editor-model litellm_proxy/local-tabby-qwq-32b-editor"
# alias aider-local-exllamav2="env LOCAL_AIDER_SKIP_HEALTH_CHECK=1 local-model-enablement-wrapper contaider --architect --model litellm_proxy/local-exllamav2-qwq-32b --editor-model litellm_proxy/local-exllamav2-qwen25-coder-32b"
alias qc="~/venv/bin/python -m llmmbc query --model llamacpp-Qwen2.5-Coder-7B -t"
qask() {
    ~/venv/bin/python -m llmmbc stream --model llamacpp-Qwen3-30B-A3B -t "$@" # | glow --stream
}
ask_multiple_choice_three() {
    ( set -x ; \
      curl -s -X POST "http://localhost:8686/v1/chat/completions" \
         -H "Content-Type: application/json" \
         -H "Authorization: Bearer sk-empty" \
         -d '
{
 "model": "'"$1"'",
 "messages": [{"role": "user", "content": "'"$2"'"}],
 "logprobs": true,
 "top_logprobs": 2,
 "max_tokens": 5,
 "logit_bias": {"**": -10}
}' | jq )
}
alias ccat="batcat -pp"
alias eem="emacs -l ~/.emacs-evil"
alias emacs-fg-daemon-tmux='( source ~/venv/bin/activate; source ~/doc/it/apei-nycklar/source-env-vars.sh; set -x; ulimit -v 16000000; emacs --fg-daemon=$(tmux display-message -p "#S") )'
alias ec="emacsclient"
alias ect='emacsclient -a "" -t'
alias ect-tmux='emacsclient -t --socket-name=$(tmux display-message -p "#S")'
alias ecc='emacsclient -a "" -c'
alias enw="emacs -nw"
alias enwq="emacs -nw -q"
alias fd="fdfind -H"
alias uuid='python -c "import uuid; import base64; print(\"_\" + base64.b32encode(uuid.uuid4().bytes).decode().strip(\"=\"))"'
alias tmx='export MPLBACKEND=Agg; tmux'
alias dmesg-less='sudo dmesg --color=always | less -R'
