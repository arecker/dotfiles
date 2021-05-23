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
export BLOG_PATH="$HOME/src/blog"
if is_mac; then
    export BLOG_SED="gsed"
fi
export PATH="$BLOG_PATH/bin:$PATH"

# aliases
alias be="bundle exec"
alias wallpaper="~/src/wallpaper/wallpaper"
alias plexbot="reckerbot --user '#plex'"
alias demo='docker run --rm -it arecker/demo:latest'
alias aws-whoami="aws sts get-caller-identity | jq -r '.Arn'"
alias aws-local='aws --profile local --endpoint-url http://localhost:4566/'
if [ "$(uname)" != "Darwin" ]; then
    alias ls="ls --color"
else
    alias quickkill='kill -9 $(pgrep -i "Quicktime")'
    alias make='gmake'
    alias sed='gsed'
fi

# pyenv
if [ -d "$HOME/.pyenv" ]; then
    export PATH="$HOME/.pyenv/bin:$PATH"
    export WORKON_HOME="$HOME/.virtualenvs"
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

# gcloud
if [ -d "/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/" ]; then
    . /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.bash.inc
    . /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.bash.inc
fi

# GOLANG
export GOROOT="/usr/local/go"
export GOPATH="$HOME/go"
export PATH="$GOPATH/bin:$GOROOT/bin:$PATH"

# hack for openvpn installed by homebrew
if [ "$(uname)" == "Darwin" ]; then
    export PATH="/usr/local/sbin:$PATH"
fi

# asdf
if [ "$(uname)" == "Darwin" ]; then
    test -f /usr/local/opt/asdf/asdf.sh && . /usr/local/opt/asdf/asdf.sh
fi

# use GNU bin, if it exists
if [ -d "/usr/local/opt/findutils/libexec/gnubin/" ]; then
    export PATH="/usr/local/opt/findutils/libexec/gnubin:$PATH"
fi

# local bin
export PATH="$HOME/bin:$PATH"
