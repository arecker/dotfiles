#!/usr/bin/env bash

log() {
    echo "bootstrap.sh: $1"
}

log "running bootstrap as $(whoami) on $(hostname)"

if [ ! -d "$HOME/src/dotfiles" ]; then
    log "cloning dotfiles"
    mkdir -p "$HOME/src"
    git clone "https://github.com/arecker/dotfiles.git" "$HOME/src/dotfiles"
else
    log "dotfiles already cloned"
fi
