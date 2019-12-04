#!/usr/bin/env bash

set -e

log() {
    echo "bootstrap.sh: $1"
}

maybe_clone() {
    if [ -d "$2" ]; then
	log "$1 already cloned to $2"
    else
	log "cloning $2"
	git clone "git@github.com:$1" "$2"
    fi
}

log "running bootstrap as $(whoami) on $(hostname)"

log "decrypting SSH key"
cat <<EOF | gpg --output "$HOME/.ssh/id_rsa" --decrypt --batch --yes
-----BEGIN PGP MESSAGE-----

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
dojgy+epWWhVcBO5mvvK
=IK8/
-----END PGP MESSAGE-----
EOF

chmod 600 "$HOME/.ssh/id_rsa"

log "creating src directory"
mkdir -p "$HOME/src"

maybe_clone "arecker/password-store.git" "$HOME/.password-store"
maybe_clone "arecker/dotfiles.git" "$HOME/src/dotfiles"
maybe_clone "arecker/emacs.d.git" "$HOME/.emacs.d"

log "creating symlinks"
cd "$HOME/src/dotfiles/" && make stow

log "done!"
