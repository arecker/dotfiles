# history
export HISTSIZE="500"
export HISTFILE="$HOME/.bash_history"

export EDITOR="nano"
export PS1="\u@\h:\w\$ "

# Fuck you, apple.
export BASH_SILENCE_DEPRECATION_WARNING=1

# functions
docs_path() {
    if [ "$(uname)" == "Darwin" ]; then
        echo "$HOME/Documents"
    else
        echo "$HOME/docs"
    fi
}

is_mac() {
    [[ "$(uname)" == "Darwin" ]]
}

# blog
alias b="cd $HOME/src/blog && python -m src"

# aliases
alias be="bundle exec"
alias k='kubectl'
alias plexbot="reckerbot --user '#plex'"
alias aws-whoami="aws sts get-caller-identity | jq -r '.Arn'"
alias aws-local='aws --profile local --endpoint-url http://localhost:4566/'

if ! is_mac; then
    alias ls="ls --color"
fi

# python
export WORKON_HOME="$HOME/.virtualenvs"
if [ -d "$HOME/.pyenv" ]; then
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init --path)"
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
fi

# poetry
if [ -d "$HOME/.poetry/bin" ]; then
    export PATH="$HOME/.poetry/bin:$PATH"
fi

# rbenv
if [ -d "$HOME/.rbenv" ]; then
    export PATH="$HOME/.rbenv/bin:$PATH"
    eval "$(rbenv init -)"
fi

# nodenv
if [ -d "$HOME/.nodenv" ]; then
    export PATH="$HOME/.nodenv/bin:$PATH"
    eval "$(nodenv init -)"
fi

# tfenv
if [ -d "$HOME/.tfenv/bin" ]; then
    export PATH="$HOME/.tfenv/bin:$PATH"
fi

# ssh
if [ -f "$HOME/.ssh/personal" ]; then
    ssh-add -K "$HOME/.ssh/personal" 2>/dev/null
fi
if [ -f "$HOME/.ssh/work/id_rsa" ]; then
    ssh-add -K "$HOME/.ssh/work/id_rsa" 2>/dev/null
fi

# go
# if is_mac; then
#     export GOPATH="$HOME/src"
#     export GOROOT="$(brew --prefix golang)/libexec"
#     export PATH="$PATH:${GOPATH}/bin:${GOROOT}/bin"
# fi

# gcloud
if [ -d "/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/" ]; then
    . /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.bash.inc
    . /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.bash.inc
fi

# hack for openvpn installed by homebrew
if [ "$(uname)" == "Darwin" ]; then
    export PATH="/usr/local/sbin:$PATH"
fi

# asdf
if [ "$(uname)" == "Darwin" ]; then
    test -f /usr/local/opt/asdf/asdf.sh && . /usr/local/opt/asdf/asdf.sh
fi

# local bin
export PATH="$HOME/bin:$PATH"

if [ -f "$HOME/bin/bashrc-work" ]; then
    source "$HOME/bin/bashrc-work"
fi
