log_file=/tmp/git_update_log
rm $log_file
touch $log_file

function pull() {
    log_file=/tmp/git_update_log
    PN=$(basename $(pwd))
    last_commit=$(git rev-parse HEAD)
        git checkout master || git checkout main; echo "package:    $PN" >> $log_file&&git pull; git log $last_commit...HEAD --no-merges --color --graph --date=format:'%Y-%m-%d %H:%M:%S' --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Cblue %s %sCgreen(%cd) %C(bold blue)<%an>%Creset' --abbrev-commit >>$log_file;echo "\n">>$log_file;
}

export -f pull
git submodule foreach 'pull' && cat $log_file
