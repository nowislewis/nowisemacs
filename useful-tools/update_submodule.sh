#!/usr/bin/env bash

log_dir=/tmp/git_update_logs
rm -rf $log_dir
mkdir -p "$log_dir" || exit 1

function pull() {
    submodule_path="$1"
    log_file="$log_dir/$(basename "$submodule_path").log"
    (
        PN=$(basename "$submodule_path")
        label="package:    $PN"
        box_pad=2  # spaces on both sides
        box_inner_len=$((${#label} + box_pad * 2))
        top_box="┌$(printf '─%.0s' $(seq 1 $box_inner_len))┐"
        mid_box="│$(printf ' %.0s' $(seq 1 $box_pad))${label}$(printf ' %.0s' $(seq 1 $box_pad))│"
        bottom_box="└$(printf '─%.0s' $(seq 1 $box_inner_len))┘"
        echo -e "\033[1;36m$top_box\033[0m" >>"$log_file"
        echo -e "\033[1;36m$mid_box\033[0m" >>"$log_file"
        echo -e "\033[1;36m$bottom_box\033[0m" >>"$log_file"
        last_commit=$(git -C "$submodule_path" rev-parse HEAD) || exit 1
        # Run in the parent repository so Git reads the submodule's branch setting.
        if ! pull_output=$(git -c color.ui=always submodule update --remote --rebase -- "$submodule_path" 2>&1); then
            echo "$pull_output"
            echo "Update failed: $submodule_path: submodule update --remote --rebase failed"
            exit 1
        fi
        new_commit=$(git -C "$submodule_path" rev-parse HEAD) || exit 1
        if [[ "$pull_output" != "Already up to date." ]]; then
            echo -e "$pull_output" >>"$log_file"
        fi
        # Only show log if HEAD moved
        # if [[ "$last_commit" != "$new_commit" ]]; then
        git -C "$submodule_path" log "$last_commit...$new_commit" --no-merges --color --graph --date=format:'%Y-%m-%d %H:%M:%S' --pretty=format:'%C(red)%h%Creset -%C(green)(%cd) %C(yellow)%d%C(blue)  %Creset%s %C(bold blue)<%an>%Creset' --abbrev-commit >>"$log_file" || exit 1
        # fi
        echo $'\n' >>"$log_file"
    ) >>"$log_file" 2>&1
}

export -f pull

submodule_paths=$(git config -f .gitmodules --get-regexp '^submodule\..*\.path$')
config_status=$?
if (( config_status > 1 )); then
    exit "$config_status"
fi

# Wait for each job explicitly: bare wait loses individual update failures.
pids=()
while read -r key path; do
    [[ -n "$key" && "$path" == lib/* ]] || continue
    pull "$path" &
    pids+=("$!")
done <<< "$submodule_paths"

status=0
for pid in "${pids[@]}"; do
    wait "$pid" || status=1
done

if (( ${#pids[@]} > 0 )); then
    cat "$log_dir"/*.log || status=1
fi
exit "$status"
