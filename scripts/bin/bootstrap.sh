#!/usr/bin/env bash
set -e

VERSION_PYTHON="3.9.4"
VERSION_RUBY="3.0.1"

log() {
    local msg="$(echo $1 | sed "s|${HOME}|~|g")"
    echo "bootstrap.sh: $msg" 1>&2
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
    screen -S "$name" -dm bash -c "${cmd}" > /dev/null 2>&1
}

bootstrap_packages() {
    local bootstrap_deps="
screen curl
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
        sudo apt-get update > /dev/null 2>&1
        log "packages - installing APT packages"
        sudo apt-get install -y \
             $bootstrap_deps \
             $lisp_deps \
             $rbenv_deps \
             $pyenv_deps \
             > /dev/null 2>&1
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
             --quit > /dev/null 2>&1
        log "lisp - finished!"
    fi
}

bootstrap_python() {
    local pyenv_dir="$HOME/.pyenv"
    local pyenv_bin="${pyenv_dir}/bin/pyenv"
    local pyenv_url="https://github.com/pyenv/pyenv"
    local pyenv_cmd="${pyenv_bin} install ${VERSION_PYTHON} && ${pyenv_bin} global ${VERSION_PYTHON}"

    if dir_exists "$pyenv_dir"; then
        log "python - $pyenv_dir already exists, nothing to do!"
    else
        log "python - cloning pyenv library"
        git clone "${pyenv_url}.git" "$pyenv_dir" > /dev/null 2>&1
        log "python - cloning pyenv dependencies"
        for dep in doctor installer update virtualenv which-ext; do
            git clone \
                "${pyenv_url}-${dep}.git" \
                "${pyenv_dir}/plugins/pyenv-${dep}" > /dev/null 2>&1
        done
        log "python - installing v${VERSION_PYTHON} (background)"
        in_screen_session "bootstrap-python" "${pyenv_cmd}"
    fi
}

bootstrap_ruby() {
    local rbenv_dir="$HOME/.rbenv"
    local rbenv_bin="${rbenv_dir}/bin/rbenv"
    local rbenv_url="https://github.com/rbenv/rbenv"
    local rbenv_cmd="${rbenv_bin} install ${VERSION_RUBY} && ${rbenv_bin} global ${VERSION_RUBY}"

    if dir_exists "$rbenv_dir"; then
        log "ruby - $rbenv_dir already exists, nothing to do!"
    else
        log "ruby - cloning rbenv library"
        git clone "${rbenv_url}.git" "$rbenv_dir" > /dev/null 2>&1
        log "ruby - cloning rbenv dependencies"
        git clone \
            "${rbenv_url}-build.git" \
            "${rbenv_dir}/plugins/rbenv-build" > /dev/null 2>&1
        log "ruby - installing v${VERSION_RUBY} (background)"
        in_screen_session "bootstrap-ruby" "${rbenv_cmd}"
    fi
}

bootstrap_packages
bootstrap_lisp
bootstrap_python
bootstrap_ruby
