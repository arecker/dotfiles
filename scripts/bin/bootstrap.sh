#!/usr/bin/env bash -e

log() {
    echo "bootstrap.sh: $1" 1>&2
}

dir_exists() {
    [[ -d "$1" ]]
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

log "starting bootstrap"
bootstrap_lisp
