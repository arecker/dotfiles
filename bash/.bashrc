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

# aliases
alias blog='PYTHONPATH="$HOME/src/blog" python -m src'
alias be="bundle exec"
alias wallpaper="~/src/wallpaper/wallpaper"
alias aws-whoami="aws sts get-caller-identity | jq -r '.Arn'"
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
ssh-add -K ~/.ssh/personal 2>/dev/null
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

# kubernetes
export KUBECONFIG="$(docs_path)/configs/kube.yml"

# local bin
export PATH="$HOME/bin:$PATH"
