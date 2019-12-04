export EDITOR="nano"
export PS1="\u@\h:\w\$ "

# aliases
alias be="bundle exec"
alias aws-whoami="aws sts get-caller-identity | jq -r '.Arn'"
if [ "$(uname)" != "Darwin" ]; then
    alias ls="ls --color"
else
    alias knife='/opt/chefdk/bin/knife'
    alias quickkill='kill -9 $(pgrep -i "Quicktime")'
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

# goenv
if [ -d "$HOME/.goenv" ]; then
    export GOENV_ROOT="$HOME/.goenv"
    export GOENV_GOPATH_PREFIX="$HOME/.go"
    export PATH="$GOENV_ROOT/bin:$PATH"
    eval "$(goenv init -)"
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

# hack for openvpn installed by homebrew
if [ "$(uname)" == "Darwin" ]; then
    export PATH="/usr/local/sbin:$PATH"
fi

export PATH="$HOME/bin:$PATH"
