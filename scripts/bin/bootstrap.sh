#!/usr/bin/env bash
set -e
set -x

VERSION_EMACS="emacs-27"
VERSION_PYTHON="3.9.4"
VERSION_RUBY="3.0.1"

log() {
    local msg="$(echo $1 | sed "s|${HOME}|~|g")"
    echo "bootstrap.sh: $msg" 1>&2
}

reload() {
    . "$HOME/.bashrc"
}

dir_exists() {
    [[ -d "$1" ]]
}

is_mac() {
    [[ "$(uname)" == "Darwin" ]]
}

in_screen_session() {
    local name="$1"
    local cmd="$2"
    screen -S "$name" -dm bash -c "${cmd}"
}

bootstrap_packages() {
    local bootstrap_deps="
screen curl unzip build-essential git
"
    local lisp_deps="
sbcl clisp
"
    local rbenv_deps="
git curl libssl-dev libreadline-dev zlib1g-dev
autoconf bison build-essential libyaml-dev
libreadline-dev libncurses5-dev libffi-dev libgdbm-dev
"
    local pyenv_deps="
make build-essential libssl-dev zlib1g-dev libbz2-dev
libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev libncursesw5-dev
xz-utils tk-dev libffi-dev liblzma-dev
"

    if is_mac; then
        log "packages - nothing to do for mac"
    else
        log "packages - refreshing list"
        sudo apt-get update
        log "packages - installing APT packages"
        sudo apt-get install -y \
             $bootstrap_deps \
             $lisp_deps \
             $rbenv_deps \
             $pyenv_deps \

    fi
}

bootstrap_lisp() {
    local quicklisp_dir="$HOME/.quicklisp"
    local quicklisp_url="https://beta.quicklisp.org/quicklisp.lisp"
    local quicklisp_dst="/tmp/quicklisp.lisp"
    local quicklisp_cmd="(quicklisp-quickstart:install :path \"${quicklisp_dir}\")"

    if dir_exists "$quicklisp_dir"; then
        log "lisp - $quicklisp_dir already exists, nothing to do!"
    else
        log "lisp - downloading quicklisp library"
        curl -s -o "$quicklisp_dst" "$quicklisp_url"
        log "lisp - running quicklisp installer"
        sbcl --load "$quicklisp_dst" \
             --eval "$quicklisp_cmd" \
             --quit
    fi
}

bootstrap_python() {
    local pyenv_dir="$HOME/.pyenv"
    local pyenv_bin="${pyenv_dir}/bin/pyenv"
    local pyenv_url="git://github.com/pyenv/pyenv"
    local pyenv_cmd="${pyenv_bin} install ${VERSION_PYTHON} && ${pyenv_bin} global ${VERSION_PYTHON}"

    if dir_exists "$pyenv_dir"; then
        log "python - $pyenv_dir already exists, nothing to do!"
    else
        log "python - cloning pyenv library"
        git clone "${pyenv_url}.git" "$pyenv_dir"
        log "python - cloning pyenv dependencies"
        for dep in doctor installer update virtualenv which-ext; do
            git clone \
                "${pyenv_url}-${dep}.git" \
                "${pyenv_dir}/plugins/pyenv-${dep}"
        done
        log "python - installing v${VERSION_PYTHON}"
        eval "${pyenv_cmd}"
        reload
        log "python - installing poetry"
        curl -sSL https://raw.githubusercontent.com/python-poetry/poetry/master/get-poetry.py | python -
    fi

    log "python - installing pip packages"
    pip install --upgrade pip awscli
}

bootstrap_ruby() {
    local rbenv_dir="$HOME/.rbenv"
    local rbenv_bin="${rbenv_dir}/bin/rbenv"
    local rbenv_url="git://github.com/rbenv/rbenv"
    local rbenv_cmd="${rbenv_bin} install ${VERSION_RUBY} && ${rbenv_bin} global ${VERSION_RUBY}"

    if dir_exists "$rbenv_dir"; then
        log "ruby - $rbenv_dir already exists, nothing to do!"
    else
        log "ruby - cloning rbenv library"
        git clone "${rbenv_url}.git" "$rbenv_dir"
        log "ruby - cloning rbenv dependencies"
        git clone \
            "git://github.com/rbenv/ruby-build" \
            "${rbenv_dir}/plugins/ruby-build"
        log "ruby - installing v${VERSION_RUBY}"
        eval "${rbenv_cmd}"
        reload
    fi

    log "ruby - installing ruby gems"
    gem install bundler pry jekyll
}

bootstrap_terraform() {
    local tfenv_dir="$HOME/.tfenv"
    local tfenv_bin="${tfenv_dir}/bin/tfenv"
    local tfenv_url="git://github.com/tfutils/tfenv"
    local tfenv_cmd="${tfenv_bin} install latest"

    if dir_exists "$tfenv_dir"; then
        log "terraform - $tfenv_dir already exists, nothing to do!"
    else
        log "terraform - cloning tfenv library"
        git clone "${tfenv_url}.git" "$tfenv_dir"
        log "terraform - cloning tfenv dependencies"
        log "terraform - installing latest terraform"
        eval "${tfenv_cmd}"
    fi
}

bootstrap_emacs() {
    local emacs_src="$HOME/src/emacs"
    local emacs_url="https://git.savannah.gnu.org/git/emacs.git"

    mkdir -p "$HOME/src"

    if dir_exists "$emacs_src"; then
        log "emacs - source cloned, nothing to do!"
    else
        if is_mac; then
            log "emacs - mac, nothing to do!"
        else
            log "emacs - installing dependencies"
            sudo apt build-dep -y emacs25
            log "emacs - cloning source"
            git clone --depth 1 --branch "$VERSION_EMACS" "$emacs_url" "$emacs_src"
            log "emacs - compiling source"
            eval "$HOME/bin/emacs-build.sh $VERSION_EMACS"
        fi
    fi
}

bootstrap_packages
# bootstrap_lisp
# bootstrap_python
# bootstrap_ruby
# bootstrap_terraform
bootstrap_emacs
