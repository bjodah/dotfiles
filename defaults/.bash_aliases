vv() {
   # list folder sizes of argument 1 (default *)
   files=${@:-"*"} # vv !(.gvfs||afs) misses e.g. "~/VirtualBox VMs"
   du -ks $files | sort -n | cut -f2 | xargs -d '\n' du -sh
}
pygrep() {
    grep --include "*.py" "${1}" -R "${2:-.}"
}

alias g='git'  # see also .bash_completion.d

alias ll='ls -laFrth --color=auto'
alias ec="emacsclient"
alias ecn="emacsclient -n"
alias enw="emacs -nw"

alias uuid='python -c "import uuid; import base64; print(\"_\" + base64.b32encode(uuid.uuid4().bytes).decode().strip(\"=\"))"'


# Lets fix some obvious typos
alias emasc="emacs"
alias cd-="cd -"
alias cd..="cd .."

