# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export HISTSIZE="500"
export HISTFILE="$HOME/.bash_history"
export PS1="\u@localhost:\w\$ "

is_mac() {
    [[ "$(uname)" == "Darwin" ]]
}

is_arm() {
    [[ "$(uname -m)" == "arm64" ]]
}

# aliases
alias be="bundle exec"
alias k='kubectl'

if ! is_mac; then
    alias ls="ls --color"
fi

# add homebrew path
if [ -d "/opt/homebrew/bin" ]; then
    export PATH="/opt/homebrew/bin:$PATH"
    export PATH="/opt/homebrew/opt/openjdk/bin:$PATH" # prefer brew java over system
fi

# emacs
if is_mac; then  # the emacsclient binary is buried somewhere in the EmacsForOSX application
    if is_arm; then
        export PATH="/Applications/Emacs.app/Contents/MacOS/bin-arm64-11/:$PATH"
    else
        export PATH="/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_14/:$PATH"
    fi
    export GPG_TTY=$(tty)
fi
export EDITOR="emacsclient"

# homebrew (on linux)
if [ -d "/home/linuxbrew/.linuxbrew" ]; then
    eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi

# asdf
export ASDF_GOLANG_MOD_VERSION_ENABLED=true
if [ -f "$HOME/.asdf/asdf.sh" ]; then
    . "$HOME/.asdf/asdf.sh"
fi
export PATH="${ASDF_DATA_DIR:-$HOME/.asdf}/shims:$PATH"

# hack for openvpn installed by homebrew
if [ "$(uname)" == "Darwin" ]; then
    export PATH="/usr/local/sbin:$PATH"
fi

# local bin
export PATH="$HOME/bin:$PATH"

# mixtape
alias mixtape="$HOME/src/mixtape/venv/bin/mixtape"

# Load work stuff
if [ -f "$HOME/bin/bashrc-work" ]; then
    source "$HOME/bin/bashrc-work"
fi

cowsay "$(fortune)" || echo "Warning: cowsay is not installed (you might literally die)"
