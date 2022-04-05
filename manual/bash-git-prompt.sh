if [ ! -e ~/.bash-git-prompt ]; then
    git clone https://github.com/magicmonty/bash-git-prompt.git ~/.bash-git-prompt --depth=1
fi

if [ -f ~/.bash-git-prompt/gitprompt.sh ]; then
    GIT_PROMPT_ONLY_IN_REPO=1
    . ~/.bash-git-prompt/gitprompt.sh
fi
