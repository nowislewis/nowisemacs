log_file=/tmp/git_update_log
rm $log_file
touch $log_file

function pull() {
	log_file=/tmp/git_update_log
	PN=$(basename $(pwd))
    git checkout main >/dev/null 2>&1||git checkout master >/dev/null 2>&1
	last_commit=$(git rev-parse HEAD)
	echo "package:    $PN" >>$log_file
	git reset --hard  && git pull
	git log $last_commit...HEAD --no-merges --color --graph --date=format:'%Y-%m-%d %H:%M:%S' --pretty=format:'%C(red)%h%Creset -%C(green)(%cd) %C(yellow)%d%C(blue)  %Creset%s %C(bold blue)<%an>%Creset' --abbrev-commit >>$log_file
	echo $'\n' >>$log_file
	echo $'\n'
}

export -f pull
git submodule foreach 'pull' && cat $log_file
