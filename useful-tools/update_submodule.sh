#!/bin/bash

log_dir=/tmp/git_update_logs
rm -rf $log_dir
mkdir -p $log_dir

function pull() {
    submodule_path="$1"
    log_file="$log_dir/$(basename "$submodule_path").log"
    (
        cd "$submodule_path"
        PN=$(basename $(pwd))
        label="package:    $PN"
        box_pad=2  # spaces on both sides
        box_inner_len=$((${#label} + box_pad * 2))
        top_box="┌$(printf '─%.0s' $(seq 1 $box_inner_len))┐"
        mid_box="│$(printf ' %.0s' $(seq 1 $box_pad))${label}$(printf ' %.0s' $(seq 1 $box_pad))│"
        bottom_box="└$(printf '─%.0s' $(seq 1 $box_inner_len))┘"
        echo -e "\033[1;36m$top_box\033[0m" >>"$log_file"
        echo -e "\033[1;36m$mid_box\033[0m" >>"$log_file"
        echo -e "\033[1;36m$bottom_box\033[0m" >>"$log_file"
        last_commit=$(git rev-parse HEAD)
        # checkout then reset, output appended
        git checkout main >/dev/null 2>&1 || git checkout master >/dev/null 2>&1
        # git reset --hard >>"$log_file" 2>&1
        pull_output=$(git -c color.ui=always pull --rebase 2>&1)
        new_commit=$(git rev-parse HEAD)
        if [[ "$pull_output" != "Already up to date." ]]; then
            echo -e "$pull_output" >>"$log_file"
        fi
        # Only show log if HEAD moved
        # if [[ "$last_commit" != "$new_commit" ]]; then
        git log $last_commit...$new_commit --no-merges --color --graph --date=format:'%Y-%m-%d %H:%M:%S' --pretty=format:'%C(red)%h%Creset -%C(green)(%cd) %C(yellow)%d%C(blue)  %Creset%s %C(bold blue)<%an>%Creset' --abbrev-commit >>"$log_file"
        # fi
        echo $'\n' >>"$log_file"
    )
}

export -f pull

submodule_paths=$(find lib -mindepth 1 -maxdepth 1 -type d)

for path in $submodule_paths; do
    pull "$path" &
done

wait

cat $log_dir/*.log
