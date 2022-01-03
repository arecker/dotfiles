#!/usr/bin/env bash

parse_configs() {
    local configs
    configs="$(echo "$KUBECONFIG" | tr ":" "\n")"

    local pattern='^current-context: '

    for config in ${configs}; do
        if grep -q "$pattern" "$config"; then
            grep "$pattern" "$config" | sed "s|${pattern}||"
            return
        fi
    done
}

__kube_ps1() {
    local result
    result="$(parse_configs)"

    if [ -n "$result" ]; then
        echo "[k8s: ${result}] "
    fi
}
